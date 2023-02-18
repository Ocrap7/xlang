#![feature(box_patterns)]

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

use tokio::net::TcpListener;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::request::Request;
use tower_lsp::{lsp_types::*, LanguageServer};
use tower_lsp::{Client, LspService, Server};
use xlang_core::ast::{ArgList, AstNode, Expression, ParamaterList, Statement, Type};
use xlang_core::token::{Operator, Span, SpannedToken, Token};
use xlang_core::{Module, SymbolKind};

struct ReadDirectoryRequest {}

impl Request for ReadDirectoryRequest {
    type Params = String;

    type Result = Vec<(String, u32)>;

    const METHOD: &'static str = "lsif/readDirectory";
}

const STOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::KEYWORD,
    SemanticTokenType::TYPE,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::CLASS,
    SemanticTokenType::ENUM,
    SemanticTokenType::INTERFACE,
    SemanticTokenType::STRUCT,
    SemanticTokenType::TYPE_PARAMETER,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::ENUM_MEMBER,
    SemanticTokenType::EVENT,
    SemanticTokenType::FUNCTION,
    SemanticTokenType::METHOD,
    SemanticTokenType::MACRO,
    SemanticTokenType::MODIFIER,
    SemanticTokenType::COMMENT,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::REGEXP,
    SemanticTokenType::OPERATOR,
];

pub struct SemanticTokenBuilder {
    tokens: Vec<SemanticToken>,
    last_line: u32,
    last_pos: u32,
}

impl SemanticTokenBuilder {
    pub fn new() -> SemanticTokenBuilder {
        SemanticTokenBuilder {
            tokens: Vec::new(),
            last_line: 0,
            last_pos: 0,
        }
    }

    pub fn push(&mut self, line: u32, position: u32, length: u32, token: u32, modifier: u32) {
        if self.last_line == line {
            let delta_pos = position - self.last_pos;
            self.last_pos = position;
            self.tokens.push(SemanticToken {
                delta_line: 0,
                delta_start: delta_pos,
                length,
                token_type: token,
                token_modifiers_bitset: modifier,
            })
        } else {
            let delta_line = line - self.last_line;
            self.last_line = line;
            self.last_pos = position;
            self.tokens.push(SemanticToken {
                delta_line,
                delta_start: position,
                length,
                token_type: token,
                token_modifiers_bitset: modifier,
            })
        }
    }

    pub fn build(self) -> Vec<SemanticToken> {
        self.tokens
    }
}

const PROPERTY_COMPLETES: &[&str] = &["class"];

struct Backend {
    element_names: HashSet<String>,
    style_enum: HashMap<String, CompletionType>,

    documents: RwLock<HashMap<Url, Module>>,
    client: Arc<Client>,
}

fn get_stype_index(ty: SemanticTokenType) -> u32 {
    STOKEN_TYPES.iter().position(|f| *f == ty).unwrap_or(0) as u32
}

fn get_stype_index_from_str(ty: &str) -> u32 {
    STOKEN_TYPES
        .iter()
        .position(|f| f.as_str() == ty)
        .unwrap_or(0) as u32
}

impl Backend {
    fn recurse_expression(
        &self,
        value: &Expression,
        module: &Module,
        scope_index: &mut Vec<usize>,
        builder: &mut SemanticTokenBuilder,
    ) {
        match value {
            Expression::Ident(tok @ SpannedToken(_, Token::Ident(value_str))) => {
                // TODO: lookup identifier in symbol tree
                // if let Some(this_sym) = module.resolve_symbol_chain_indicies(scope_index.iter()) {
                //     if let Some(found_sym) = module.resolve_symbol(&this_sym, &value_str) {
                //         builder.push(
                //             tok.span().line_num,
                //             tok.span().position,
                //             tok.span().length,
                //             get_stype_index(SemanticTokenType::TYPE),
                //             0,
                //         );
                //     };
                // };
                // }
                builder.push(
                    tok.span().line_num,
                    tok.span().position,
                    tok.span().length,
                    get_stype_index(SemanticTokenType::VARIABLE),
                    0,
                );
            }
            Expression::Float(_, _, tok) => {
                builder.push(
                    tok.span().line_num,
                    tok.span().position,
                    tok.span().length,
                    get_stype_index(SemanticTokenType::NUMBER),
                    0,
                );
            }
            Expression::Integer(_, _, tok) => {
                builder.push(
                    tok.span().line_num,
                    tok.span().position,
                    tok.span().length,
                    get_stype_index(SemanticTokenType::NUMBER),
                    0,
                );
            }
            Expression::Function {
                parameters,
                return_parameters,
                body,
                ..
            } => {
                self.recurse_params(module, parameters, scope_index, builder);
                self.recurse_params(module, return_parameters, scope_index, builder);

                if let Some(body) = body {
                    // for item in body.iter_items() {
                    self.recurse(module, body, scope_index, builder);
                    // }
                }
            }
            Expression::FunctionCall { expr, args } => {
                match &**expr {
                    Expression::Ident(tok) => {
                        builder.push(
                            tok.span().line_num,
                            tok.span().position,
                            tok.span().length,
                            get_stype_index(SemanticTokenType::FUNCTION),
                            0,
                        );
                    }
                    _ => {
                        self.recurse_expression(expr, module, scope_index, builder);
                    }
                }

                self.recurse_args(module, args, scope_index, builder);
            }
            Expression::Tuple(_) => (),
            Expression::Array { values, .. } => values
                .iter_items()
                .for_each(|item| self.recurse_expression(item, module, scope_index, builder)),
            Expression::BinaryExpression { left, right, .. } => {
                if let Some(left) = left {
                    self.recurse_expression(left, module, scope_index, builder);
                }
                if let Some(right) = right {
                    self.recurse_expression(right, module, scope_index, builder);
                }
            }
            Expression::Record { parameters } => {
                self.recurse_params(module, parameters, scope_index, builder);
            }
            _ => (),
        }
    }

    fn recurse_args(
        &self,
        module: &Module,
        args: &ArgList,
        scope_index: &mut Vec<usize>,
        builder: &mut SemanticTokenBuilder,
    ) {
        for item in args.iter_items() {
            self.recurse_expression(item, module, scope_index, builder)
        }
    }

    fn recurse_params(
        &self,
        module: &Module,
        args: &ParamaterList,
        scope_index: &mut Vec<usize>,
        builder: &mut SemanticTokenBuilder,
    ) {
        for item in args.iter_items() {
            if let Some(ty) = &item.ty {
                // TODO: recurse type
                self.recurse_type(module, ty, scope_index, builder);
            }

            if let Some(name) = &item.name {
                builder.push(
                    name.span().line_num,
                    name.span().position,
                    name.span().length,
                    get_stype_index(SemanticTokenType::VARIABLE),
                    0,
                );
            }
        }
    }

    fn recurse_type(
        &self,
        module: &Module,
        ty: &Type,
        scope_index: &mut Vec<usize>,
        builder: &mut SemanticTokenBuilder,
    ) {
        match ty {
            Type::Integer { token, .. } => {
                builder.push(
                    token.span().line_num,
                    token.span().position,
                    token.span().length,
                    get_stype_index_from_str("type"),
                    0,
                );
            }
            Type::Float { token, .. } => {
                builder.push(
                    token.span().line_num,
                    token.span().position,
                    token.span().length,
                    get_stype_index_from_str("type"),
                    0,
                );
            }
            Type::Ident(ident) => {
                builder.push(
                    ident.span().line_num,
                    ident.span().position,
                    ident.span().length,
                    get_stype_index_from_str("type"),
                    0,
                );
            }
        }
    }

    fn recurse(
        &self,
        module: &Module,
        stmt: &Statement,
        scope_index: &mut Vec<usize>,
        builder: &mut SemanticTokenBuilder,
    ) {
        match stmt {
            Statement::List(list) => {
                for l in list.iter_items() {
                    self.recurse(module, l, scope_index, builder);
                }
            }
            Statement::Decleration { ident, expr, .. } => {
                let func = match expr {
                    Some(Expression::Function { .. }) => get_stype_index_from_str("function"),
                    Some(Expression::Record { .. }) => get_stype_index_from_str("struct"),
                    _ => get_stype_index_from_str("variable"),
                };
                builder.push(
                    ident.span().line_num,
                    ident.span().position,
                    ident.span().length,
                    func,
                    0,
                );
                if let Some(expr) = expr {
                    self.recurse_expression(expr, module, scope_index, builder);
                }
            }
            Statement::Expression(e) => self.recurse_expression(e, module, scope_index, builder),
            Statement::UseStatement { token, args } => {
                if let Some(token) = token {
                    builder.push(
                        token.span().line_num,
                        token.span().position,
                        token.span().length,
                        get_stype_index_from_str("keyword"),
                        0,
                    )
                }

                module.iter_symbol(args.iter_items(), |name, val| match val.borrow().kind {
                    _ => {
                        builder.push(
                            name.span().line_num,
                            name.span().position,
                            name.span().length,
                            get_stype_index_from_str("namespace"),
                            0,
                        );
                    }
                });
            }
        }
    }

    fn bsearch_value_with_key(
        &self,
        key: &SpannedToken,
        span: &Span,
    ) -> Option<Vec<CompletionItem>> {
        None
    }

    fn bsearch_statement(
        &self,
        module: &Module,
        item: &Statement,
        span: &Span,
    ) -> Option<Vec<CompletionItem>> {
        match item {
            Statement::UseStatement { args, .. } => {
                if let Some((_, Some(SpannedToken(_, Token::Operator(Operator::Dot))))) =
                    args.iter().last()
                {
                    if let Some(sym) = module.resolve_symbol_chain(args.iter_items()) {
                        println!("Use {}", sym.borrow().name);
                        let mut comp = Vec::new();

                        return Some(comp);
                    }
                }
            }
            _ => (),
        }
        None
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _p: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    // TextDocumentSyncKind::INCREMENTAL,
                    TextDocumentSyncKind::FULL,
                )),
                // color_provider: Some(ColorProviderCapability::Simple(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions {
                                work_done_progress: None,
                            },
                            legend: SemanticTokensLegend {
                                token_types: STOKEN_TYPES.into(),
                                token_modifiers: vec![],
                            },
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: Some(vec![":".to_string(), ".".to_string()]),
                    ..Default::default()
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: None,
                    }),
                    file_operations: None,
                }),
                ..ServerCapabilities::default()
            },
            ..Default::default()
        })
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let toks = {
            let map = &*self.documents.read().unwrap();

            let Some(mods) = map.get(&params.text_document.uri) else {
                return Ok(None)
            };

            let mut builder = SemanticTokenBuilder::new();
            let mut scope = Vec::with_capacity(50);
            scope.push(0);
            for (i, tok) in mods.stmts.iter().enumerate() {
                scope[0] = i;
                self.recurse(mods, tok, &mut scope, &mut builder);
            }
            builder.build()
        };

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            data: toks,
            result_id: None,
        })))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        self.client
            .log_message(
                MessageType::INFO,
                format!("completino {:?}", params.text_document_position.position),
            )
            .await;
        let res = {
            let map = &*self.documents.read().unwrap();
            let Some(mods) = map.get(&params.text_document_position.text_document.uri) else {
                return Ok(None)
            };
            let sp = Span {
                line_num: params.text_document_position.position.line,
                position: params.text_document_position.position.character,
                ..Default::default()
            };

            let items = mods
                .stmts
                .iter()
                .find_map(|f| self.bsearch_statement(mods, f, &sp));

            if let None = items {
                if mods
                    .stmts
                    .iter()
                    .find(|f| f.get_range().contains(&sp))
                    .is_none()
                {
                    Some(
                        self.element_names
                            .iter()
                            .map(|name| CompletionItem {
                                label: name.into(),
                                kind: Some(CompletionItemKind::PROPERTY),
                                ..Default::default()
                            })
                            .collect(),
                    )
                } else {
                    items
                }
            } else {
                items
            }
        };
        self.client
            .log_message(MessageType::INFO, format!("completino {:?}", res))
            .await;

        if let Some(items) = res {
            return Ok(Some(CompletionResponse::Array(items)));
        } else {
            return Ok(None);
        }
    }

    async fn completion_resolve(&self, params: CompletionItem) -> Result<CompletionItem> {
        Ok(params)
    }

    async fn initialized(&self, _p: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let out = xlang_core::Module::parse_str(&params.text_document.text);
        println!("tree {}", out.0.format());

        for err in out.1 {
            self.client.log_message(MessageType::ERROR, err).await;
        }

        (*(self.documents.write().unwrap())).insert(params.text_document.uri, out.0);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        println!("Change {:?}", params);

        let doc = params.text_document;
        for change in params.content_changes {
            let text = change.text;

            let out = xlang_core::Module::parse_str(&text);
            println!("{}", out.0.format());

            for err in out.1 {
                self.client.log_message(MessageType::ERROR, err).await;
            }

            (*(self.documents.write().unwrap())).insert(doc.uri.clone(), out.0);

            self.client.semantic_tokens_refresh().await.unwrap();
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

pub enum CompletionType {
    Enum(Vec<String>),
    Boolean,
    Symbol(Box<CompletionType>),
    Style,
    Color,
    Rect,
    Unknown,
}

#[tokio::main]
async fn main() {
    let _read = tokio::io::stdin();
    let _write = tokio::io::stdout();

    #[cfg(feature = "runtime-agnostic")]
    use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

    let _args = std::env::args();

    let listener = TcpListener::bind("127.0.0.1:5007").await.unwrap();
    println!("cjkdsfj");
    let (stream, _) = listener.accept().await.unwrap();
    println!("Connection");

    let (read, write) = tokio::io::split(stream);
    #[cfg(feature = "runtime-agnostic")]
    let (read, write) = (read.compat(), write.compat_write());

    let (service, socket) = LspService::new(|client| {
        let client = Arc::new(client);
        let res = Backend {
            element_names: HashSet::from_iter(["style".into(), "view".into(), "setup".into()]),
            style_enum: HashMap::from([
                (
                    "direction".to_string(),
                    CompletionType::Enum(vec![
                        "Vertical".to_string(),
                        "Horizontal".to_string(),
                        "VerticalReverse".to_string(),
                        "HorizontalReverse".to_string(),
                    ]),
                ),
                ("visible".to_string(), CompletionType::Boolean),
                (
                    "class".to_string(),
                    CompletionType::Symbol(Box::new(CompletionType::Style)),
                ),
                ("backgroundColor".to_string(), CompletionType::Color),
                ("foregroundColor".to_string(), CompletionType::Color),
                ("borderColor".to_string(), CompletionType::Color),
                ("borderWidth".to_string(), CompletionType::Rect),
                ("padding".to_string(), CompletionType::Rect),
                ("radius".to_string(), CompletionType::Rect),
                ("gap".to_string(), CompletionType::Unknown),
            ]),
            documents: RwLock::new(HashMap::new()),
            client: client.clone(),
        };

        res
    });
    Server::new(read, write, socket).serve(service).await;
}

#[inline]
fn to_rng(range: &xlang_core::token::Range) -> Range {
    if range.start == range.end {
        Range::new(
            Position {
                line: range.start.line_num,
                character: range.start.position,
            },
            Position {
                line: range.start.line_num,
                character: range.start.position + range.start.length,
            },
        )
    } else {
        Range::new(
            Position {
                line: range.start.line_num,
                character: range.start.position,
            },
            Position {
                line: range.end.line_num,
                character: range.end.position + range.end.length,
            },
        )
    }
}

#[inline]
fn range_contains(inner: &Range, outer: &Range) -> bool {
    inner.start.line >= outer.start.line
        && inner.end.line <= outer.end.line
        && inner.start.character >= outer.start.character
        && inner.end.character <= outer.end.character
}
