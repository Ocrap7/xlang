#![feature(box_patterns)]

use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

use tokio::net::TcpListener;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::request::Request;
use tower_lsp::{lsp_types::*, LanguageServer};
use tower_lsp::{Client, LspService, Server};
use xlang_core::ast::{ArgList, AstNode, Expression, ParamaterList, Statement, Type};
use xlang_core::token::{Operator, Span, SpannedToken, Token};
use xlang_core::Module;
use xlang_util::format::TreeDisplay;
use xlang_util::Rf;
use xlang_vm::const_value::{ConstValue, ConstValueKind};
use xlang_vm::error::ErrorLevel;
use xlang_vm::pass::CodePass;
use xlang_vm::scope::{Scope, ScopeManager, ScopeValue};
use xlang_vm::stdlib::{fill_module, std_module};

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

#[derive(Default)]
pub struct SemanticTokenBuilder {
    tokens: Vec<SemanticToken>,
    last_line: u32,
    last_pos: u32,
}

impl SemanticTokenBuilder {
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

struct Backend {
    element_names: HashSet<String>,
    style_enum: HashMap<String, CompletionType>,

    documents: RwLock<HashMap<Url, (Arc<Module>, ScopeManager)>>,
    client: Arc<Client>,

    symbol_tree: Rf<Scope>,
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
        scope: &ScopeManager,
        scope_index: &mut Vec<usize>,
        builder: &mut SemanticTokenBuilder,
    ) {
        match value {
            Expression::Ident(tok) => {
                let mut current_scope = Vec::new();
                scope.push_scope_chain(&mut current_scope, scope_index.iter());

                println!(
                    "Scope: {} -- {}  -- {:?}",
                    tok.as_str(),
                    current_scope.format(),
                    scope_index
                );
                if let Some(sym) = scope.find_symbol_in_scope(tok.as_str(), &current_scope) {
                    println!("Found: {}", tok.as_str());
                    let sym = sym.borrow();

                    match &sym.value {
                        ScopeValue::ConstValue(ConstValue {
                            kind:
                                ConstValueKind::Function { .. } | ConstValueKind::NativeFunction { .. },
                            ..
                        }) => {
                            builder.push(
                                tok.span().line_num,
                                tok.span().position,
                                tok.span().length,
                                get_stype_index(SemanticTokenType::FUNCTION),
                                0,
                            );
                        }
                        ScopeValue::Record { .. } => {
                            builder.push(
                                tok.span().line_num,
                                tok.span().position,
                                tok.span().length,
                                get_stype_index(SemanticTokenType::TYPE),
                                0,
                            );
                        }
                        ScopeValue::ConstValue(ConstValue {
                            kind: ConstValueKind::RecordInstance { .. },
                            ..
                        }) => {
                            builder.push(
                                tok.span().line_num,
                                tok.span().position,
                                tok.span().length,
                                get_stype_index(SemanticTokenType::TYPE),
                                0,
                            );
                        }
                        _ => {
                            builder.push(
                                tok.span().line_num,
                                tok.span().position,
                                tok.span().length,
                                get_stype_index(SemanticTokenType::VARIABLE),
                                0,
                            );
                        }
                    }
                }
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
                    self.recurse(module, scope, body, scope_index, builder);
                }
            }
            Expression::FunctionCall { expr, args } => {
                self.recurse_expression(expr, module, scope, scope_index, builder);
                self.recurse_args(module, scope, args, scope_index, builder);
            }
            Expression::Tuple(_) => (),
            Expression::Array { values, .. } => values.iter_items().for_each(|item| {
                self.recurse_expression(item, module, scope, scope_index, builder)
            }),
            Expression::BinaryExpression { left, right, .. } => {
                if let Some(left) = left {
                    self.recurse_expression(left, module, scope, scope_index, builder);
                }
                if let Some(right) = right {
                    self.recurse_expression(right, module, scope, scope_index, builder);
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
        scope: &ScopeManager,
        args: &ArgList,
        scope_index: &mut Vec<usize>,
        builder: &mut SemanticTokenBuilder,
    ) {
        for item in args.iter_items() {
            self.recurse_expression(item, module, scope, scope_index, builder)
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
        _module: &Module,
        ty: &Type,
        _scope_index: &mut Vec<usize>,
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
        scope: &ScopeManager,
        stmt: &Statement,
        scope_index: &mut Vec<usize>,
        builder: &mut SemanticTokenBuilder,
    ) {
        match stmt {
            Statement::List(list) => {
                for l in list.iter_items() {
                    self.recurse(module, scope, l, scope_index, builder);
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
                    self.recurse_expression(expr, module, scope, scope_index, builder);
                }
            }
            Statement::Expression(e) => {
                self.recurse_expression(e, module, scope, scope_index, builder)
            }
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

                scope.iter_use(args.iter_items().map(|f| (f.as_str(), f)), |sym, ud| {
                    builder.push(
                        ud.span().line_num,
                        ud.span().position,
                        ud.span().length,
                        get_stype_index_from_str("namespace"),
                        0,
                    );
                });

                // module.iter_symbol(args.iter_items(), |name, val| match val.borrow().kind {
                //     _ => {
                //         builder.push(
                //             name.span().line_num,
                //             name.span().position,
                //             name.span().length,
                //             get_stype_index_from_str("namespace"),
                //             0,
                //         );
                //     }
                // });
            }
        }
    }

    fn bsearch_value_with_key(
        &self,
        _key: &SpannedToken,
        _span: &Span,
    ) -> Option<Vec<CompletionItem>> {
        None
    }

    fn bsearch_statement(
        &self,
        module: &Module,
        item: &Statement,
        _span: &Span,
    ) -> Option<Vec<CompletionItem>> {
        match item {
            Statement::UseStatement { args, .. } => {
                if let Some((_, Some(SpannedToken(_, Token::Operator(Operator::Dot))))) =
                    args.iter().last()
                {
                    // if let Some(sym) = module.resolve_symbol_chain(args.iter_items()) {
                    //     println!("Use {}", sym.borrow().name);
                    //     let comp = Vec::new();

                    //     return Some(comp);
                    // }
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

            let mut scope = Vec::with_capacity(50);
            if let Some(id) = mods.1.index_of_mod(&mods.0.name) {
                scope.push(id);
            }

            let mut builder = SemanticTokenBuilder::default();
            scope.push(0);
            for (i, tok) in mods.0.stmts.iter().enumerate() {
                scope[1] = i;
                self.recurse(&mods.0, &mods.1, tok, &mut scope, &mut builder);
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
                .0
                .stmts
                .iter()
                .find_map(|f| self.bsearch_statement(&mods.0, f, &sp));

            if items.is_none() {
                if !mods.0.stmts.iter().any(|f| f.get_range().contains(&sp)) {
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
            .log_message(MessageType::INFO, format!("completino {res:?}"))
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
        let out = xlang_core::Module::parse_str(&params.text_document.text, "mymod");
        println!("tree {}", out.0.format());

        for err in out.1 {
            self.client.log_message(MessageType::ERROR, err).await;
        }

        let module = Arc::new(out.0);

        let code_pass = CodePass::new(self.symbol_tree.clone(), module.clone(), 1);
        let code_pass_state = code_pass.run();
        println!("MMKLSDAJFLAKDSJFoij {}", self.symbol_tree.format());

        let diags: Vec<_> = code_pass_state
            .errors
            .iter()
            .map(|err| Diagnostic {
                range: to_rng(&err.range),
                severity: match err.kind.get_level() {
                    ErrorLevel::Error => Some(DiagnosticSeverity::ERROR),
                    ErrorLevel::Warning => Some(DiagnosticSeverity::WARNING),
                    ErrorLevel::Info => Some(DiagnosticSeverity::INFORMATION),
                    ErrorLevel::Hint => Some(DiagnosticSeverity::HINT),
                },
                message: err.kind.to_string(),
                ..Default::default()
            })
            .collect();
        println!("diagns: {}", diags.len());

        self.client
            .publish_diagnostics(params.text_document.uri.clone(), diags, None)
            .await;

        (*(self.documents.write().unwrap()))
            .insert(params.text_document.uri, (module, code_pass_state.scope));
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        println!("Change {params:?}");

        let doc = params.text_document;
        for change in params.content_changes {
            let text = change.text;

            let out = xlang_core::Module::parse_str(&text, "mymod");
            println!("{}", out.0.format());

            for err in out.1 {
                self.client.log_message(MessageType::ERROR, err).await;
            }

            let module = Arc::new(out.0);

            let code_pass = CodePass::new(self.symbol_tree.clone(), module.clone(), 1);
            let code_pass_state = code_pass.run();

            let diags: Vec<_> = code_pass_state
                .errors
                .iter()
                .map(|err| Diagnostic {
                    range: to_rng(&err.range),
                    severity: match err.kind.get_level() {
                        ErrorLevel::Error => Some(DiagnosticSeverity::ERROR),
                        ErrorLevel::Warning => Some(DiagnosticSeverity::WARNING),
                        ErrorLevel::Info => Some(DiagnosticSeverity::INFORMATION),
                        ErrorLevel::Hint => Some(DiagnosticSeverity::HINT),
                    },
                    message: err.kind.to_string(),
                    ..Default::default()
                })
                .collect();

            println!("diagns: {}", diags.len());
            self.client
                .publish_diagnostics(doc.uri.clone(), diags, None)
                .await;

            (*(self.documents.write().unwrap()))
                .insert(doc.uri.clone(), (module, code_pass_state.scope));

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

        let symbol_tree = Rf::new(Scope::new(ScopeValue::Root, 0));
        {
            // let std_module = std_module();

            // let code_pass = CodePass::new(symbol_tree.clone(), std_module.clone());
            // let code_pass_state = code_pass.run();
            // let std_mod_scope = code_pass_state.scope.module.clone();

            // let evaluator = Evaluator::new(std_module.clone(), code_pass_state.scope);
            // let values = evaluator.evaluate();

            // for error in &code_pass_state.errors {
            //     error.print("std.xl", &lines);
            // }

            // for error in &evaluator.state.read().unwrap().errors {
            //     error.print("std.xl", &lines);
            // }

            // for value in values {
            //     println!("{value}");
            // }
            // let std_mod_scope = Rf::new(Scope::new(ScopeValue::Root));
            let std_mod_scope = symbol_tree.borrow_mut().insert("std", ScopeValue::Root, 0);

            fill_module(std_mod_scope);
        }

        Backend {
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
            client,
            symbol_tree,
        }
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
