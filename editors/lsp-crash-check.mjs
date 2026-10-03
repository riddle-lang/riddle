import { spawn } from 'node:child_process';
import { mkdtempSync, realpathSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { pathToFileURL } from 'node:url';

// Feeds one source to the language server and reports whether it survives.
// Usage: node lsp-crash-check.mjs <server> [source-file]
const command = process.argv[2] ?? 'target/release/riddle-lsp.exe';
const sourcePath = process.argv[3];
const text = sourcePath
  ? (await import('node:fs')).readFileSync(sourcePath, 'utf8')
  : 'fun main() {\n    unreachable!("Fuckypi");\n    p\n}\n';

const root = realpathSync.native(mkdtempSync(join(tmpdir(), 'riddle-lsp-crash-')));
const file = join(root, 'crash.rid');
const uri = pathToFileURL(file).href;
writeFileSync(file, text);

const server = spawn(command, [], { stdio: ['pipe', 'pipe', 'pipe'] });
let stdout = Buffer.alloc(0);
let stderr = '';
const incoming = [];
const pending = new Map();
let nextId = 1;
let exit = null;

server.on('exit', (code, signal) => {
  exit = { code, signal };
});
server.stderr.on('data', (chunk) => {
  stderr += chunk.toString();
});
server.stdout.on('data', (chunk) => {
  stdout = Buffer.concat([stdout, chunk]);
  for (;;) {
    const headerEnd = stdout.indexOf('\r\n\r\n');
    if (headerEnd < 0) break;
    const match = /Content-Length: (\d+)/i.exec(stdout.subarray(0, headerEnd).toString());
    if (!match) break;
    const length = Number(match[1]);
    const start = headerEnd + 4;
    if (stdout.length < start + length) break;
    const body = stdout.subarray(start, start + length).toString();
    stdout = stdout.subarray(start + length);
    incoming.push(JSON.parse(body));
  }
});

function send(message) {
  const payload = JSON.stringify(message);
  server.stdin.write(`Content-Length: ${Buffer.byteLength(payload)}\r\n\r\n${payload}`);
}

// Resolve responses while waiting, so a reply that arrives before the caller
// awaits its promise is still matched.
async function pumpUntil(predicate, timeoutMs, label) {
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    const index = incoming.findIndex(predicate);
    if (index >= 0) return incoming.splice(index, 1)[0];
    if (exit !== null) throw new Error(`${label}: server exited ${JSON.stringify(exit)}`);
    if (Date.now() > deadline) throw new Error(`${label}: timed out`);
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
}

async function request(method, params) {
  const id = nextId++;
  send({ jsonrpc: '2.0', id, method, params });
  const message = await pumpUntil((m) => m.id === id, 30000, method);
  return message.result ?? message.error;
}

const settle = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

// Times a request the way an editor experiences it.
async function timed(label, method, params) {
  const started = Date.now();
  try {
    const result = await request(method, params);
    const items = Array.isArray(result) ? result.length : (result?.items?.length ?? '-');
    console.log(`  ${label}: ${Date.now() - started} ms (${items} items)`);
    return Date.now() - started;
  } catch (error) {
    console.log(`  ${label}: ${error.message} after ${Date.now() - started} ms`);
    return Number.POSITIVE_INFINITY;
  }
}

try {
  await request('initialize', {
    processId: process.pid,
    rootUri: pathToFileURL(root).href,
    capabilities: {},
  });
  send({ jsonrpc: '2.0', method: 'initialized', params: {} });
  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: { textDocument: { uri, languageId: 'riddle', version: 1, text } },
  });
  await settle(3000);

  const lines = text.split('\n');
  const lastLine = lines.length - 2;
  const probes = [
    ['textDocument/completion', { textDocument: { uri }, position: { line: lastLine, character: 5 } }],
    ['textDocument/hover', { textDocument: { uri }, position: { line: lastLine, character: 5 } }],
    ['textDocument/documentSymbol', { textDocument: { uri } }],
    ['textDocument/semanticTokens/full', { textDocument: { uri } }],
    ['textDocument/foldingRange', { textDocument: { uri } }],
    [
      'textDocument/inlayHint',
      {
        textDocument: { uri },
        range: { start: { line: 0, character: 0 }, end: { line: lines.length, character: 0 } },
      },
    ],
    ['textDocument/codeAction', { textDocument: { uri }, range: { start: { line: lastLine, character: 4 }, end: { line: lastLine, character: 5 } }, context: { diagnostics: [] } }],
  ];
  for (const [method, params] of probes) {
    try {
      const result = await request(method, params);
      const summary = Array.isArray(result)
        ? `${result.length} items`
        : result?.items
          ? `${result.items.length} items`
          : result?.data
            ? `${result.data.length} data`
            : result === null || result === undefined
              ? 'null'
              : 'ok';
      console.log(`  ${method}: ${summary}`);
    } catch (error) {
      console.log(`  ${method}: ${error.message}`);
      break;
    }
  }

  await settle(1500);
  const published = incoming.filter((m) => m.method === 'textDocument/publishDiagnostics');
  const items = published.flatMap((m) => m.params.diagnostics);
  console.log(`diagnostics: ${published.length} publish(es), ${items.length} item(s)`);
  for (const item of items.slice(0, 8)) console.log(`    ${JSON.stringify(item).slice(0, 220)}`);

  // What an editor actually does while the user types: ask for completion at
  // the cursor, repeatedly, in whatever state the buffer happens to be in.
  console.log('completion timing at the cursor:');
  await timed('1st', 'textDocument/completion', {
    textDocument: { uri },
    position: { line: lastLine, character: 5 },
  });
  const samples = [];
  for (let index = 0; index < 5; index++) {
    samples.push(
      await timed(`repeat ${index + 2}`, 'textDocument/completion', {
        textDocument: { uri },
        position: { line: lastLine, character: 5 },
      }),
    );
  }
  console.log(`  worst repeat: ${Math.max(...samples)} ms`);
  console.log(`  background stderr so far: ${stderr.length} bytes`);
} catch (error) {
  console.log(`FAILED: ${error.message}`);
} finally {
  console.log(`exit: ${exit === null ? 'still running' : JSON.stringify(exit)}`);
  if (stderr.trim()) {
    console.log('--- stderr ---');
    console.log(stderr.split('\n').slice(0, 30).join('\n'));
  }
  server.kill();
  await settle(200);
  try {
    rmSync(root, { recursive: true, force: true });
  } catch {}
}
