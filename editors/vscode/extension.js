const vscode = require('vscode');
const { LanguageClient } = require('vscode-languageclient/node');

let client;

async function activate(context) {
  const config = vscode.workspace.getConfiguration('riddle');
  const command = config.get('server.path', 'riddle-lsp');
  const args = config.get('server.arguments', []);

  client = new LanguageClient(
    'riddle-lsp',
    'Riddle Language Server',
    { command, args },
    {
      documentSelector: [
        { scheme: 'file', language: 'riddle' },
        { scheme: 'untitled', language: 'riddle' },
        // The manifest is registered as its own language so the server's
        // `Clue.toml` diagnostics, completions and hover are reachable.
        // Matching on the filename alone left the file to the built-in TOML
        // extension, and the server never received it.
        { scheme: 'file', language: 'clue' },
        { scheme: 'untitled', language: 'clue' },
      ],
      middleware: {
        provideInlayHints: async (document, range, token, next) => {
          if (!vscode.workspace.getConfiguration('riddle').get('inlayHints.enabled', true)) {
            return [];
          }
          return next(document, range, token);
        },
      },
    },
  );

  context.subscriptions.push(client);
  await client.start();
}

async function deactivate() {
  await client?.stop();
}

module.exports = { activate, deactivate };
