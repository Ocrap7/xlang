/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path'
import {
    workspace,
    ExtensionContext,
    FileType as VFileType,
    FileSystemProvider,
    FileChangeEvent,
    EventEmitter,
    Event,
    Uri,
    FileSystemError,
    languages,
    InputBoxOptions,
    window,
    ConfigurationTarget,
} from 'vscode'
import * as net from 'net'
import { exec } from 'child_process'

import {
    Disposable,
    Executable,
    LanguageClient,
    LanguageClientOptions,
    RequestType,
    ServerOptions,
    StreamInfo,
    TransportKind,
} from 'vscode-languageclient/node'
import { getVSCodeDownloadUrl } from '@vscode/test-electron/out/util'

let client: LanguageClient

export async function activate(context: ExtensionContext) {
    let serverModule = workspace.getConfiguration('xlang').get<string>('lsPath')

    if (serverModule === '' || serverModule === undefined) {
        serverModule = await promptLSPath()
    }

    if (serverModule === '' || serverModule === undefined) {
        console.error('Unable to find language server binary!')
        return;
    }

    let connectionInfo = {
        port: 5007,
        host: '127.0.0.1',
    }

    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
        // Register the server for plain text documents
        documentSelector: [{ scheme: 'file', language: 'xlang' }],
        synchronize: {
            // Notify the server about file changes to '.clientrc files contained in the workspace
            fileEvents: workspace.createFileSystemWatcher('**/.clientrc'),
        },
    }

    let serverOptions = () => {
        return new Promise<StreamInfo>((resolve, reject) => {
            let ls = exec(`${serverModule}`, (stderr, stdout, _) => {
                if (stderr) {
                    console.error(`exec error: ${stderr} ${serverModule}`);
                    return;
                }
                console.log(`stdout: ${stdout}`);
                console.error(`stderr: ${stderr}`);
            })
            console.log(ls)

            ls.stderr.on('data', data => {
                console.error(data)
            })
            ls.stderr.on('error', console.error)

            ls.stdout.on('data', data => {
                console.log('fjsdklf', data)
                let socket = net.connect(connectionInfo)
                let result: StreamInfo = {
                    writer: socket,
                    reader: socket,
                }
                resolve(result)
            })
            ls.stdout.on('error', console.error)
        })
    }

    // Create the language client and start the client.
    client = new LanguageClient(
        'languageServerExample',
        'Language Server Example',
        serverOptions,
        clientOptions
    )

    // Start the client. This will also launch the server
    client.start()

    let clientPromise = new Promise<LanguageClient>((resolve, reject) => {
        client.onReady().then(
            () => {
                resolve(client)
            },
            error => {
                reject(error)
            }
        )
    })

    workspace.registerFileSystemProvider('lsif', new LsifFS(clientPromise), {
        isCaseSensitive: true,
        isReadonly: true,
    })
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined
    }
    return client.stop()
}

async function promptLSPath(): Promise<string> {
    let options: InputBoxOptions = {
        prompt: 'Please enter path to Xlang langauage server: ',
    }

    let value = await window.showInputBox(options)
    value = path.normalize(value);
    workspace.getConfiguration('xlang').update('lsPath', value, ConfigurationTarget.Global)
    return value
}

namespace FileType {
    export const Unknown: 0 = 0
    export const File: 1 = 1
    export const Directory: 2 = 2
    export const SymbolicLink = 64
}

type FileType = 0 | 1 | 2 | 64

interface FileStat {
    type: FileType
    ctime: number
    mtime: number
    size: number
}

interface StatFileParams {
    uri: string
}

namespace StatFileRequest {
    export const type = new RequestType<StatFileParams, FileStat | null, void>(
        'lsif/statFile'
    )
}

interface ReadFileParams {
    uri: string
}

namespace ReadFileRequest {
    export const type = new RequestType<ReadFileParams, string, void>(
        'lsif/readfile'
    )
}

interface ReadDirectoryParams {
    uri: string
}

namespace ReadDirectoryRequest {
    export const type = new RequestType<
        ReadDirectoryParams,
        [string, FileType][],
        void
    >('lsif/readDirectory')
}

class LsifFS implements FileSystemProvider {
    private readonly client: Promise<LanguageClient>

    private readonly emitter: EventEmitter<FileChangeEvent[]>
    public readonly onDidChangeFile: Event<FileChangeEvent[]>

    public constructor(client: Promise<LanguageClient>) {
        this.client = client
        this.emitter = new EventEmitter<FileChangeEvent[]>()
        this.onDidChangeFile = this.emitter.event
    }

    watch(
        uri: Uri,
        options: { recursive: boolean; excludes: string[] }
    ): Disposable {
        // The LSIF file systrem never changes.
        return Disposable.create((): void => { })
    }

    async stat(uri: Uri): Promise<FileStat> {
        const client = await this.client
        return client
            .sendRequest(StatFileRequest.type, {
                uri: client.code2ProtocolConverter.asUri(uri),
            })
            .then(
                value => {
                    if (!value) {
                        throw FileSystemError.FileNotFound(uri)
                    }
                    return value
                },
                error => {
                    throw FileSystemError.FileNotFound(uri)
                }
            )
    }

    async readDirectory(uri: Uri): Promise<[string, VFileType][]> {
        const client = await this.client
        const params: ReadDirectoryParams = {
            uri: client.code2ProtocolConverter.asUri(uri),
        }
        return client
            .sendRequest(ReadDirectoryRequest.type, params)
            .then(values => {
                return values
            })
    }

    async readFile(uri: Uri): Promise<Uint8Array> {
        const client = await this.client
        const params: ReadFileParams = {
            uri: client.code2ProtocolConverter.asUri(uri),
        }
        return client.sendRequest(ReadFileRequest.type, params).then(value => {
            const result = new Uint8Array(Buffer.from(value, 'base64'))
            return result
        })
    }

    createDirectory(uri: Uri): void | Thenable<void> {
        throw new Error('File system is readonly.')
    }

    writeFile(
        uri: Uri,
        content: Uint8Array,
        options: { create: boolean; overwrite: boolean }
    ): void | Thenable<void> {
        throw new Error('File system is readonly.')
    }

    delete(uri: Uri, options: { recursive: boolean }): void | Thenable<void> {
        throw new Error('File system is readonly.')
    }

    rename(
        oldUri: Uri,
        newUri: Uri,
        options: { overwrite: boolean }
    ): void | Thenable<void> {
        throw new Error('File system is readonly.')
    }
}
