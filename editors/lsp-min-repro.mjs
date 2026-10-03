import { spawn } from 'node:child_process';
import { mkdtempSync, realpathSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { pathToFileURL } from 'node:url';

// Minimal: a buffer that is not yet a program, then a completion request.
const command = process.argv[2] ?? 'target/release/riddle-lsp.exe';
const text = process.argv[3] ?? 'f';
const root = realpathSync.native(mkdtempSync(join(tmpdir(), 'riddle-lsp-min-')));
const file = join(root, 'min.rid');
const uri = pathToFileURL(file).href;
writeFileSync(file, text);

const server = spawn(command, [], { stdio: ['pipe', 'pipe', 'pipe'] });
let stdout = Buffer.alloc(0);
let stderr = '';
const incoming = [];
let exit = null;
let nextId = 1;

server.on('exit', (code, signal) => {
  exit = { code, signal };
});
server.stderr.on('data', (chunk) => {
  stderr += chunk.toString();
  for (const line of chunk.toString().split('\n').filter(Boolean)) {
    console.log(`  [stderr] ${line.trim()}`);
  }
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
const settle = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
async function request(method, params, timeoutMs = 10000) {
  const id = nextId++;
  send({ jsonrpc: '2.0', id, method, params });
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    const index = incoming.findIndex((m) => m.id === id);
    if (index >= 0) {
      const [message] = incoming.splice(index, 1);
      return message.result ?? message.error;
    }
    if (exit !== null) throw new Error(`server exited ${JSON.stringify(exit)}`);
    if (Date.now() > deadline) throw new Error(`timed out after ${timeoutMs} ms`);
    await settle(5);
  }
}

console.log(`source: ${JSON.stringify(text)}`);
try {
  const initStarted = Date.now();
  await request('initialize', {
    processId: process.pid,
    rootUri: pathToFileURL(root).href,
    capabilities: {},
  });
  console.log(`initialize: ${Date.now() - initStarted} ms`);
  send({ jsonrpc: '2.0', method: 'initialized', params: {} });

  // Probe the server while the buffer is being opened, so a stall shows up as
  // the last request that worked rather than as an unexplained timeout.
  for (const wait of [200, 800, 2000, 5000]) {
    await settle(wait === 200 ? 200 : wait - (wait === 800 ? 200 : wait === 2000 ? 800 : 2000));
    try {
      const started = Date.now();
      await request('textDocument/documentSymbol', { textDocument: { uri } }, 4000);
      console.log(`  probe at ${wait} ms: responsive (${Date.now() - started} ms)`);
    } catch (error) {
      console.log(`  probe at ${wait} ms: ${error.message}`);
    }
  }

  send({
    jsonrpc: '2.0',
    method: 'textDocument/didOpen',
    params: { textDocument: { uri, languageId: 'riddle', version: 1, text } },
  });

  for (const wait of [500, 2000, 5000]) {
    await settle(wait === 500 ? 500 : wait === 2000 ? 1500 : 3000);
    try {
      const started = Date.now();
      await request('textDocument/documentSymbol', { textDocument: { uri } }, 4000);
      console.log(`  after didOpen ${wait} ms: responsive (${Date.now() - started} ms)`);
    } catch (error) {
      console.log(`  after didOpen ${wait} ms: ${error.message}`);
    }
  }

  const started = Date.now();
  try {
    const result = await request('textDocument/completion', {
      textDocument: { uri },
      position: { line: 0, character: text.length },
    });
    const items = Array.isArray(result) ? result.length : (result?.items?.length ?? 0);
    console.log(`completion: ${Date.now() - started} ms, ${items} items`);
  } catch (error) {
    console.log(`completion: ${error.message}`);
  }

  try {
    const hover = await request(
      'textDocument/hover',
      { textDocument: { uri }, position: { line: 0, character: 0 } },
      5000,
    );
    console.log(`hover after: ${hover === null ? 'null' : 'ok'}`);
  } catch (error) {
    console.log(`hover after: ${error.message}`);
  }
  console.log(`exit: ${exit === null ? 'still running' : JSON.stringify(exit)}`);
} catch (error) {
  console.log(`FAILED: ${error.message}`);
} finally {
  server.kill();
  await settle(200);
  try {
    rmSync(root, { recursive: true, force: true });
  } catch {}
}
