import type {
  AotExternCall,
  AotExternDescriptor,
} from './index.js';
import {
  O_CREATE,
  O_EXCL,
  O_RDWR,
  type VirtualFS,
} from './vfs.js';

const MAY_HOST_REPLAY = 1n << 4n;
const MAY_HOST_WAIT = 1n << 3n;
const MAY_EXIT = 1n << 7n;
const MAY_WAIT_IO_REPLAY = 1n << 2n;
const MODE_DIR = 1n << 31n;
const MODE_DEVICE = 1n << 26n;
const MODE_CHAR_DEVICE = 1n << 21n;
const NET_UNSUPPORTED = 'operation not supported on wasm';
const OS_UNSUPPORTED = 'operation not supported on wasm';

const OS_ERROR_MESSAGES = [
  'file does not exist',
  'file already exists',
  'permission denied',
  'invalid argument',
  'operation timed out',
  'file already closed',
  'not a directory',
  'is a directory',
] as const;

const NET_ERROR_MESSAGES = [
  'use of closed network connection',
  'i/o timeout',
  'connection refused',
  'connection reset by peer',
  'address already in use',
  'address not available',
] as const;

const HTTP_TIMEOUT_MESSAGE = 'request timeout';

const OS_OPERATIONS = new Set([
  'getOsErrors', 'getOsConsts', 'getPathSeparators',
  'fileRead', 'blocking_fileRead', 'fileWrite', 'blocking_fileWrite',
  'blocking_fileReadAt', 'blocking_fileWriteAt', 'fileSeek', 'fileClose',
  'fileSync', 'fileStat', 'fileTruncate', 'openFile',
  'nativeMkdir', 'nativeMkdirAll', 'nativeRemove', 'nativeRemoveAll',
  'nativeRename', 'nativeStat', 'nativeLstat', 'nativeReadDir', 'nativeChmod',
  'nativeChown', 'nativeSymlink', 'nativeReadlink', 'nativeLink', 'nativeTruncate',
  'nativeReadFile', 'nativeWriteFile', 'nativeGetenv', 'nativeSetenv',
  'nativeUnsetenv', 'nativeEnviron', 'nativeLookupEnv', 'nativeClearenv',
  'nativeExpandEnv', 'nativeGetwd', 'nativeChdir', 'nativeUserHomeDir',
  'nativeUserCacheDir', 'nativeUserConfigDir', 'nativeTempDir', 'nativeGetpid',
  'nativeGetppid', 'nativeGetuid', 'nativeGeteuid', 'nativeGetgid', 'nativeGetegid',
  'nativeExit', 'nativeGetArgs', 'nativeIsTerminal', 'nativeHostname',
  'nativeExecutable', 'nativeCreateTemp', 'nativeMkdirTemp', 'nativePipe',
  'nativeChtimes', 'nativeFindProcess', 'nativeKillProcess',
]);

const NET_INT_ERROR = new Set([
  'dial', 'listen', 'listenPacket', 'blocking_tcpListenerAccept', 'unixDial',
  'unixListen', 'blocking_unixListenerAccept',
]);
const NET_COUNT_ERROR = new Set([
  'blocking_tcpConnRead', 'blocking_tcpConnWrite', 'blocking_udpConnWriteTo',
  'blocking_unixConnRead', 'blocking_unixConnWrite',
]);
const NET_ERROR_ONLY = new Set([
  'tcpConnClose', 'tcpListenerClose', 'tcpConnSetDeadline', 'tcpConnSetReadDeadline',
  'tcpConnSetWriteDeadline', 'udpConnClose', 'udpConnSetDeadline',
  'udpConnSetReadDeadline', 'udpConnSetWriteDeadline', 'unixConnSetDeadline',
  'unixConnSetReadDeadline', 'unixConnSetWriteDeadline', 'unixConnClose',
  'unixListenerClose',
]);
const NET_EMPTY_STRING = new Set([
  'tcpConnLocalAddr', 'tcpConnRemoteAddr', 'tcpListenerAddr', 'udpConnLocalAddr',
]);
const NET_REF_ERROR = new Set([
  'resolveTCPAddr', 'resolveUDPAddr', 'lookupHost', 'lookupIP', 'lookupAddr',
]);

const HTTP_OPERATIONS = new Set([
  'getHttpErrors', 'nativeNewClientRequest', 'nativeCancelClientRequest',
  'nativeReleaseClientRequest', 'nativeHttpsRequest',
]);
const FILEPATH_OPERATIONS = new Set(['evalSymlinks', 'absPath']);

function canonicalExternName(packageName: string, functionName: string): string {
  const encoder = new TextEncoder();
  return `vo1:${encoder.encode(packageName).byteLength}:${packageName}`
    + `:${encoder.encode(functionName).byteLength}:${functionName}`;
}

function operationInPackage(
  descriptor: AotExternDescriptor,
  packageName: string,
  operations: ReadonlySet<string>,
): string | undefined {
  if (descriptor.source !== 1) return undefined;
  for (const operation of operations) {
    if (descriptor.name === canonicalExternName(packageName, operation)) return operation;
  }
  return undefined;
}

function operation(descriptor: AotExternDescriptor): { packageName: string; name: string } | undefined {
  const os = operationInPackage(descriptor, 'os', OS_OPERATIONS);
  if (os !== undefined) return { packageName: 'os', name: os };
  if (descriptor.source === 1
    && descriptor.name === canonicalExternName('net', 'getNetErrors')) {
    return { packageName: 'net', name: 'getNetErrors' };
  }
  for (const family of [NET_INT_ERROR, NET_COUNT_ERROR, NET_ERROR_ONLY, NET_EMPTY_STRING,
    NET_REF_ERROR]) {
    const net = operationInPackage(descriptor, 'net', family);
    if (net !== undefined) return { packageName: 'net', name: net };
  }
  const udpRead = operationInPackage(
    descriptor,
    'net',
    new Set(['blocking_udpConnReadFrom']),
  );
  if (udpRead !== undefined) return { packageName: 'net', name: udpRead };
  const http = operationInPackage(descriptor, 'net/http', HTTP_OPERATIONS);
  if (http !== undefined) return { packageName: 'net/http', name: http };
  const filepath = operationInPackage(descriptor, 'path/filepath', FILEPATH_OPERATIONS);
  if (filepath !== undefined) return { packageName: 'path/filepath', name: filepath };
  return undefined;
}

function argument(call: AotExternCall, offset: number): bigint {
  return call.readSlot(call.argumentsStart + offset);
}

function write(call: AotExternCall, offset: number, value: bigint): void {
  call.writeSlot(call.destination + offset, value);
}

function signed(value: bigint): bigint {
  return BigInt.asIntN(64, value);
}

function safeNumber(value: bigint): number | undefined {
  const number = Number(signed(value));
  return Number.isSafeInteger(number) && BigInt(number) === signed(value) ? number : undefined;
}

function decodeUtf8(bytes: Uint8Array, error: string): string {
  try {
    return new TextDecoder('utf-8', { fatal: true }).decode(bytes);
  } catch {
    throw new Error(error);
  }
}

function pathArgument(call: AotExternCall, offset: number, field: string): string {
  return decodeUtf8(call.readStringBytes(argument(call, offset)), `os: ${field} contains invalid UTF-8`);
}

interface EnvironmentEntry {
  readonly key: Uint8Array;
  value: Uint8Array;
}

interface HttpRequestState {
  readonly controller: AbortController;
}

/** Browser-safe platform providers for Core-Wasm AOT images.
 *
 * Files live in the same bounded VFS used by the wasm-bindgen runtime. Raw
 * sockets and process mutation fail closed. HTTP uses Fetch and remains an
 * explicitly replayable extern operation.
 */
export class AotPlatformHost {
  private readonly osErrors: Array<readonly [bigint, bigint]> = [];
  private readonly environment = new Map<string, EnvironmentEntry>();
  private readonly httpRequests = new Map<number, HttpRequestState | undefined>();
  private nextHttpRequest = 1;
  private tempCounter = 1;

  constructor(private readonly fileSystem: VirtualFS) {}

  static supportsDescriptor(descriptor: AotExternDescriptor): boolean {
    return operation(descriptor) !== undefined;
  }

  static supportedEffects(descriptor: AotExternDescriptor): bigint | undefined {
    const resolved = operation(descriptor);
    if (resolved === undefined) return undefined;
    if (resolved.packageName === 'net/http' && resolved.name === 'nativeHttpsRequest') {
      return MAY_WAIT_IO_REPLAY | MAY_HOST_WAIT | MAY_HOST_REPLAY;
    }
    if (resolved.packageName === 'os' && resolved.name === 'nativeExit') return MAY_EXIT;
    if (resolved.name.startsWith('blocking_')) return MAY_WAIT_IO_REPLAY;
    return 0n;
  }

  async initialize(descriptors: readonly AotExternDescriptor[]): Promise<void> {
    if (descriptors.some((descriptor) => {
      const packageName = operation(descriptor)?.packageName;
      return packageName === 'os' || packageName === 'path/filepath';
    })) {
      await this.fileSystem.init();
    }
  }

  supports(descriptor: AotExternDescriptor): boolean {
    return AotPlatformHost.supportsDescriptor(descriptor);
  }

  handle(call: AotExternCall): number | void | Promise<number | void> {
    const resolved = operation(call.descriptor);
    if (resolved === undefined) throw new Error(`unsupported AOT platform extern ${call.name}`);
    if (resolved.packageName === 'os') return this.handleOs(call, resolved.name);
    if (resolved.packageName === 'net') return this.handleNet(call, resolved.name);
    if (resolved.packageName === 'path/filepath') return this.handleFilepath(call, resolved.name);
    return this.handleHttp(call, resolved.name);
  }

  private normalizePath(path: string): string {
    if (path.length === 0) return '.';
    const rooted = path.startsWith('/');
    const parts: string[] = [];
    for (const part of path.split('/')) {
      if (part.length === 0 || part === '.') continue;
      if (part === '..') {
        if (parts.length !== 0 && parts[parts.length - 1] !== '..') parts.pop();
        else if (!rooted) parts.push(part);
      } else parts.push(part);
    }
    if (parts.length === 0) return rooted ? '/' : '.';
    return `${rooted ? '/' : ''}${parts.join('/')}`;
  }

  private handleFilepath(call: AotExternCall, name: string): void {
    let path: string;
    try {
      path = decodeUtf8(
        call.readStringBytes(argument(call, 0)),
        'filepath: path contains invalid UTF-8',
      );
    } catch (error) {
      write(call, 0, 0n);
      call.writeError(call.destination + 1, (error as Error).message);
      return;
    }
    if (name === 'absPath') {
      const absolute = path.startsWith('/') ? path : `/${path}`;
      write(call, 0, call.allocateString(this.normalizePath(absolute)));
      call.clearError(call.destination + 1);
      return;
    }
    const normalized = this.normalizePath(path);
    const [, , , , , error] = this.fileSystem.stat(normalized);
    if (error !== null) {
      write(call, 0, 0n);
      call.writeError(call.destination + 1, 'no such file or directory');
      return;
    }
    write(call, 0, call.allocateString(normalized));
    call.clearError(call.destination + 1);
  }

  private rememberError(call: AotExternCall, slot: number, message: string): readonly [bigint, bigint] {
    call.writeError(slot, message);
    return [call.readSlot(slot), call.readSlot(slot + 1)];
  }

  private writeOsError(call: AotExternCall, slot: number, message: string | null): void {
    if (message === null) {
      call.clearError(slot);
      return;
    }
    const index = OS_ERROR_MESSAGES.findIndex((candidate) => (
      message === candidate || message.startsWith(`${candidate}:`)
    ));
    const cause = index >= 0 ? this.osErrors[index] : undefined;
    if (cause !== undefined && message === OS_ERROR_MESSAGES[index]) {
      call.writeSlot(slot, cause[0]);
      call.writeSlot(slot + 1, cause[1]);
    } else {
      call.writeError(slot, message, cause);
    }
  }

  private writeFileInfo(
    call: AotExternCall,
    name: Uint8Array,
    size: number,
    mode: number,
    modTimeMilliseconds: number,
    isDirectory: boolean,
    error: string | null,
  ): void {
    if (error !== null) {
      for (let index = 0; index < 5; index += 1) write(call, index, 0n);
      this.writeOsError(call, call.destination + 5, error);
      return;
    }
    write(call, 0, call.allocateStringBytes(name));
    write(call, 1, BigInt(size));
    write(call, 2, BigInt(mode) | (isDirectory ? MODE_DIR : 0n));
    write(call, 3, BigInt(Math.trunc(modTimeMilliseconds / 1000)));
    write(call, 4, isDirectory ? 1n : 0n);
    call.clearError(call.destination + 5);
  }

  private readCompletion(
    call: AotExternCall,
    requested: number,
    data: Uint8Array | null,
    error: string | null,
    requireFull: boolean,
  ): void {
    if (data === null || data.byteLength > requested) {
      write(call, 0, 0n);
      this.writeOsError(call, call.destination + 1, error ?? 'invalid browser VFS host response');
      return;
    }
    const count = call.writeByteSlice(argument(call, 1), data);
    write(call, 0, BigInt(count));
    if (error !== null) this.writeOsError(call, call.destination + 1, error);
    else if (requested > 0 && (count === 0 || (requireFull && count < requested))) {
      call.writeError(call.destination + 1, 'EOF');
    } else call.clearError(call.destination + 1);
  }

  private writeCompletion(
    call: AotExternCall,
    requested: number,
    written: number,
    error: string | null,
  ): void {
    if (!Number.isSafeInteger(written) || written < 0 || written > requested) {
      write(call, 0, 0n);
      this.writeOsError(call, call.destination + 1, 'invalid browser VFS host response');
      return;
    }
    write(call, 0, BigInt(written));
    if (error !== null) this.writeOsError(call, call.destination + 1, error);
    else if (written < requested) call.writeError(call.destination + 1, 'short write');
    else call.clearError(call.destination + 1);
  }

  private handleOs(call: AotExternCall, name: string): number | void {
    if (name === 'getOsErrors') {
      this.osErrors.length = 0;
      OS_ERROR_MESSAGES.forEach((message, index) => {
        this.osErrors.push(this.rememberError(call, call.destination + index * 2, message));
      });
      return;
    }
    if (name === 'getOsConsts') {
      [0n, 1n, 2n, 8n, 16n, 32n, 64n, 128n]
        .forEach((value, index) => write(call, index, value));
      return;
    }
    if (name === 'getPathSeparators') {
      write(call, 0, 47n);
      write(call, 1, 58n);
      return;
    }
    if (name === 'nativeExit') return call.exit(Number(BigInt.asIntN(32, argument(call, 0))));
    if (name === 'nativeGetArgs') {
      write(call, 0, call.allocateStringSlice(call.args));
      return;
    }
    if (name === 'nativeUserHomeDir' || name === 'nativeUserCacheDir'
      || name === 'nativeUserConfigDir') {
      const value = name === 'nativeUserHomeDir' ? '/home'
        : (name === 'nativeUserCacheDir' ? '/tmp/cache' : '/home/config');
      write(call, 0, call.allocateString(value));
      call.clearError(call.destination + 1);
      return;
    }
    if (name === 'nativeTempDir') {
      write(call, 0, call.allocateString('/tmp'));
      return;
    }
    if (name === 'nativeGetpid' || name === 'nativeGetuid' || name === 'nativeGeteuid'
      || name === 'nativeGetgid' || name === 'nativeGetegid') {
      write(call, 0, name === 'nativeGetpid' ? 1n : 1000n);
      return;
    }
    if (name === 'nativeGetppid' || name === 'nativeIsTerminal') {
      write(call, 0, 0n);
      return;
    }
    if (name === 'nativeHostname' || name === 'nativeExecutable') {
      write(call, 0, call.allocateString(name === 'nativeHostname' ? 'wasm' : '/wasm'));
      call.clearError(call.destination + 1);
      return;
    }

    if (name === 'openFile') {
      try {
        const path = pathArgument(call, 0, 'path');
        const flags = safeNumber(argument(call, 1));
        const mode = safeNumber(argument(call, 2));
        if (flags === undefined || mode === undefined) throw new Error('invalid argument: OpenFile arguments are out of range');
        const [fd, error] = this.fileSystem.openFile(path, flags, mode);
        write(call, 0, BigInt(error === null ? fd : 0));
        this.writeOsError(call, call.destination + 1, error);
      } catch (error) {
        write(call, 0, 0n);
        this.writeOsError(call, call.destination + 1, (error as Error).message);
      }
      return;
    }
    if (name === 'fileRead' || name === 'blocking_fileRead'
      || name === 'blocking_fileReadAt') {
      const fd = safeNumber(argument(call, 0));
      const buffer = call.readByteSlice(argument(call, 1));
      if (fd === undefined) {
        this.readCompletion(call, buffer.byteLength, null, 'invalid argument: file descriptor is out of range', false);
      } else if (fd === 0) {
        this.readCompletion(call, buffer.byteLength, new Uint8Array(), null, name.endsWith('ReadAt'));
      } else if (fd === 1 || fd === 2) {
        this.readCompletion(call, buffer.byteLength, null, 'permission denied', false);
      } else if (name === 'blocking_fileReadAt') {
        const offset = safeNumber(argument(call, 2));
        const [data, error] = offset === undefined
          ? [null, 'invalid argument: file offset is out of range'] as const
          : this.fileSystem.readAt(fd, buffer.byteLength, offset);
        this.readCompletion(call, buffer.byteLength, data, error, true);
      } else {
        const [data, error] = this.fileSystem.read(fd, buffer.byteLength);
        this.readCompletion(call, buffer.byteLength, data, error, false);
      }
      return;
    }
    if (name === 'fileWrite' || name === 'blocking_fileWrite'
      || name === 'blocking_fileWriteAt') {
      const fd = safeNumber(argument(call, 0));
      const bytes = call.readByteSlice(argument(call, 1));
      if (fd === undefined) {
        this.writeCompletion(call, bytes.byteLength, 0, 'invalid argument: file descriptor is out of range');
      } else if (fd === 0) {
        this.writeCompletion(call, bytes.byteLength, 0, 'permission denied');
      } else if (fd === 1 || fd === 2) {
        call.writeOutput(fd, bytes);
        this.writeCompletion(call, bytes.byteLength, bytes.byteLength, null);
      } else if (name === 'blocking_fileWriteAt') {
        const offset = safeNumber(argument(call, 2));
        const [written, error] = offset === undefined
          ? [0, 'invalid argument: file offset is out of range'] as const
          : this.fileSystem.writeAt(fd, bytes, offset);
        this.writeCompletion(call, bytes.byteLength, written, error);
      } else {
        const [written, error] = this.fileSystem.write(fd, bytes);
        this.writeCompletion(call, bytes.byteLength, written, error);
      }
      return;
    }
    if (name === 'fileSeek') {
      const fd = safeNumber(argument(call, 0));
      const offset = safeNumber(argument(call, 1));
      const whence = safeNumber(argument(call, 2));
      const [position, error] = fd === undefined || offset === undefined || whence === undefined
        ? [-1, 'invalid argument: seek arguments are out of range'] as const
        : this.fileSystem.seek(fd, offset, whence);
      write(call, 0, BigInt(error === null ? position : 0));
      this.writeOsError(call, call.destination + 1, error);
      return;
    }
    if (name === 'fileClose' || name === 'fileSync') {
      const fd = safeNumber(argument(call, 0));
      const error = fd === undefined ? 'invalid argument: file descriptor is out of range'
        : (fd >= 0 && fd <= 2 ? null
          : (name === 'fileClose' ? this.fileSystem.close(fd) : this.fileSystem.sync(fd)));
      this.writeOsError(call, call.destination, error);
      return;
    }
    if (name === 'fileTruncate') {
      const fd = safeNumber(argument(call, 0));
      const size = safeNumber(argument(call, 1));
      const error = fd === undefined || size === undefined
        ? 'invalid argument: truncate arguments are out of range'
        : this.fileSystem.ftruncate(fd, size);
      this.writeOsError(call, call.destination, error);
      return;
    }
    if (name === 'fileStat') {
      const fd = safeNumber(argument(call, 0));
      const path = call.readStringBytes(argument(call, 1));
      let start = path.byteLength;
      while (start > 0 && path[start - 1] === 47) start -= 1;
      const end = start;
      while (start > 0 && path[start - 1] !== 47) start -= 1;
      const base = end === 0 ? (path.byteLength === 0 ? new TextEncoder().encode('.') : new TextEncoder().encode('/'))
        : path.slice(start, end);
      if (fd === undefined) this.writeFileInfo(call, base, 0, 0, 0, false, 'invalid argument: file descriptor is out of range');
      else if (fd >= 0 && fd <= 2) this.writeFileInfo(
        call, base, 0, Number(MODE_DEVICE | MODE_CHAR_DEVICE | 0o666n), 0, false, null,
      );
      else {
        const [size, mode, modTime, isDirectory, error] = this.fileSystem.fstat(fd);
        this.writeFileInfo(call, base, size, mode, modTime, isDirectory, error);
      }
      return;
    }

    if (name === 'nativeStat' || name === 'nativeLstat') {
      try {
        const path = pathArgument(call, 0, 'path');
        const [base, size, mode, modTime, isDirectory, error] = this.fileSystem.stat(path);
        this.writeFileInfo(call, new TextEncoder().encode(base), size, mode, modTime, isDirectory, error);
      } catch (error) {
        this.writeFileInfo(call, new Uint8Array(), 0, 0, 0, false, (error as Error).message);
      }
      return;
    }
    if (name === 'nativeReadDir') {
      try {
        const path = pathArgument(call, 0, 'path');
        const [entries, error] = this.fileSystem.readDir(path);
        if (error !== null) {
          write(call, 0, 0n);
          this.writeOsError(call, call.destination + 1, error);
          return;
        }
        const records = entries.map(([entryName, isDirectory, mode]) => ({
          name: call.allocateString(entryName),
          isDir: isDirectory ? 1n : 0n,
          mode: BigInt(mode) | (isDirectory ? MODE_DIR : 0n),
        }));
        write(call, 0, call.allocateNamedStructSlice('os.DirEntry', records));
        call.clearError(call.destination + 1);
      } catch (error) {
        write(call, 0, 0n);
        this.writeOsError(call, call.destination + 1, (error as Error).message);
      }
      return;
    }

    const singlePathError = (
      field: string,
      action: (path: string) => string | null,
    ): void => {
      try {
        this.writeOsError(call, call.destination, action(pathArgument(call, 0, field)));
      } catch (error) {
        this.writeOsError(call, call.destination, (error as Error).message);
      }
    };
    if (name === 'nativeMkdir' || name === 'nativeMkdirAll' || name === 'nativeChmod') {
      const mode = safeNumber(argument(call, 1));
      if (mode === undefined) this.writeOsError(call, call.destination, 'invalid argument: file mode is out of range');
      else singlePathError('path', (path) => (name === 'nativeMkdir'
        ? this.fileSystem.mkdir(path, mode)
        : (name === 'nativeMkdirAll' ? this.fileSystem.mkdirAll(path, mode)
          : this.fileSystem.chmod(path, mode))));
      return;
    }
    if (name === 'nativeRemove' || name === 'nativeRemoveAll') {
      singlePathError('path', (path) => (name === 'nativeRemove'
        ? this.fileSystem.remove(path) : this.fileSystem.removeAll(path)));
      return;
    }
    if (name === 'nativeTruncate') {
      const size = safeNumber(argument(call, 1));
      if (size === undefined) this.writeOsError(call, call.destination, 'invalid argument: file size is out of range');
      else singlePathError('path', (path) => this.fileSystem.truncate(path, size));
      return;
    }
    if (name === 'nativeChdir') {
      singlePathError('working directory', (path) => this.fileSystem.chdir(path));
      return;
    }
    if (name === 'nativeRename') {
      try {
        const oldPath = pathArgument(call, 0, 'old path');
        const newPath = pathArgument(call, 1, 'new path');
        this.writeOsError(call, call.destination, this.fileSystem.rename(oldPath, newPath));
      } catch (error) {
        this.writeOsError(call, call.destination, (error as Error).message);
      }
      return;
    }
    if (name === 'nativeReadFile') {
      try {
        const [data, error] = this.fileSystem.readFile(pathArgument(call, 0, 'path'));
        write(call, 0, data === null ? 0n : call.allocateByteSlice(data));
        this.writeOsError(call, call.destination + 1, error);
      } catch (error) {
        write(call, 0, 0n);
        this.writeOsError(call, call.destination + 1, (error as Error).message);
      }
      return;
    }
    if (name === 'nativeWriteFile') {
      try {
        const path = pathArgument(call, 0, 'path');
        const mode = safeNumber(argument(call, 2));
        const error = mode === undefined ? 'invalid argument: file mode is out of range'
          : this.fileSystem.writeFile(path, call.readByteSlice(argument(call, 1)), mode);
        this.writeOsError(call, call.destination, error);
      } catch (error) {
        this.writeOsError(call, call.destination, (error as Error).message);
      }
      return;
    }
    if (name === 'nativeGetwd') {
      const [path, error] = this.fileSystem.getwd();
      write(call, 0, call.allocateString(error === null ? path : ''));
      this.writeOsError(call, call.destination + 1, error);
      return;
    }

    if (name === 'nativeGetenv' || name === 'nativeLookupEnv' || name === 'nativeSetenv'
      || name === 'nativeUnsetenv' || name === 'nativeEnviron' || name === 'nativeClearenv'
      || name === 'nativeExpandEnv') {
      this.handleEnvironment(call, name);
      return;
    }
    if (name === 'nativeCreateTemp' || name === 'nativeMkdirTemp') {
      this.handleTemporary(call, name === 'nativeCreateTemp');
      return;
    }

    if (name === 'nativeReadlink') {
      write(call, 0, 0n);
      this.writeOsError(call, call.destination + 1, OS_UNSUPPORTED);
    } else if (name === 'nativePipe') {
      write(call, 0, 0n);
      write(call, 1, 0n);
      this.writeOsError(call, call.destination + 2, OS_UNSUPPORTED);
    } else {
      this.writeOsError(call, call.destination, OS_UNSUPPORTED);
    }
  }

  private environmentKey(bytes: Uint8Array): string {
    return [...bytes].map((byte) => byte.toString(16).padStart(2, '0')).join('');
  }

  private validateEnvironmentKey(bytes: Uint8Array): string | undefined {
    if (bytes.byteLength === 0) return 'invalid argument: environment variable name must not be empty';
    if (bytes.includes(61)) return "invalid argument: environment variable name must not contain '='";
    if (bytes.includes(0)) return 'invalid argument: environment variable name must not contain NUL';
    if (bytes.byteLength > 4096) return 'invalid argument: environment variable name is too long';
    return undefined;
  }

  private handleEnvironment(call: AotExternCall, name: string): void {
    if (name === 'nativeClearenv') {
      this.environment.clear();
      return;
    }
    if (name === 'nativeEnviron') {
      const values = [...this.environment.values()].map(({ key, value }) => {
        const bytes = new Uint8Array(key.byteLength + value.byteLength + 1);
        bytes.set(key);
        bytes[key.byteLength] = 61;
        bytes.set(value, key.byteLength + 1);
        return decodeUtf8(bytes, 'os: environment contains invalid UTF-8');
      });
      write(call, 0, call.allocateStringSlice(values));
      return;
    }
    const key = call.readStringBytes(argument(call, 0));
    const keyError = this.validateEnvironmentKey(key);
    const entry = keyError === undefined ? this.environment.get(this.environmentKey(key)) : undefined;
    if (name === 'nativeGetenv' || name === 'nativeLookupEnv') {
      write(call, 0, call.allocateStringBytes(entry?.value ?? new Uint8Array()));
      if (name === 'nativeLookupEnv') write(call, 1, entry === undefined ? 0n : 1n);
      return;
    }
    if (name === 'nativeExpandEnv') {
      write(call, 0, call.allocateStringBytes(this.expandEnvironment(key)));
      return;
    }
    if (keyError !== undefined) {
      this.writeOsError(call, call.destination, keyError);
      return;
    }
    if (name === 'nativeUnsetenv') {
      this.environment.delete(this.environmentKey(key));
      call.clearError(call.destination);
      return;
    }
    const value = call.readStringBytes(argument(call, 1));
    if (value.includes(0)) {
      this.writeOsError(call, call.destination, 'invalid argument: environment variable value must not contain NUL');
      return;
    }
    if (value.byteLength > 1024 * 1024) {
      this.writeOsError(call, call.destination, 'invalid argument: environment variable value is too large');
      return;
    }
    this.environment.set(this.environmentKey(key), { key: key.slice(), value: value.slice() });
    call.clearError(call.destination);
  }

  private expandEnvironment(input: Uint8Array): Uint8Array {
    const output: number[] = [];
    for (let index = 0; index < input.byteLength;) {
      if (input[index] !== 36 || index + 1 >= input.byteLength) {
        output.push(input[index]);
        index += 1;
        continue;
      }
      let end = index + 1;
      let key: Uint8Array;
      if (input[end] === 123) {
        const close = input.indexOf(125, end + 1);
        if (close < 0) {
          output.push(36);
          index += 1;
          continue;
        }
        key = input.slice(end + 1, close);
        end = close + 1;
      } else {
        while (end < input.byteLength
          && (input[end] === 95 || (input[end] >= 48 && input[end] <= 57)
            || (input[end] >= 65 && input[end] <= 90)
            || (input[end] >= 97 && input[end] <= 122))) end += 1;
        key = input.slice(index + 1, end);
      }
      const value = this.environment.get(this.environmentKey(key))?.value;
      if (value !== undefined) output.push(...value);
      index = end;
    }
    return Uint8Array.from(output);
  }

  private temporaryPath(directory: string, pattern: string): string {
    if (pattern.includes('/') || pattern.includes('\0')) {
      throw new Error('invalid argument: temporary file pattern must not contain a path separator');
    }
    const random = new Uint32Array(2);
    if (globalThis.crypto?.getRandomValues) globalThis.crypto.getRandomValues(random);
    else {
      random[0] = this.tempCounter >>> 0;
      random[1] = Date.now() >>> 0;
    }
    this.tempCounter += 1;
    const token = [...random].map((value) => value.toString(16).padStart(8, '0')).join('');
    const star = pattern.lastIndexOf('*');
    const name = star < 0 ? `${pattern}${token}`
      : `${pattern.slice(0, star)}${token}${pattern.slice(star + 1)}`;
    return `${directory === '/' ? '' : directory.replace(/\/+$/, '')}/${name}`;
  }

  private handleTemporary(call: AotExternCall, file: boolean): void {
    try {
      const directory = pathArgument(call, 0, 'temporary directory') || '/tmp';
      const pattern = pathArgument(call, 1, 'temporary file pattern');
      let finalError = 'file already exists';
      for (let attempt = 0; attempt < 10_000; attempt += 1) {
        const path = this.temporaryPath(directory, pattern);
        if (file) {
          const [fd, error] = this.fileSystem.openFile(path, O_RDWR | O_CREATE | O_EXCL, 0o600);
          if (error === null) {
            write(call, 0, BigInt(fd));
            write(call, 1, call.allocateString(path));
            call.clearError(call.destination + 2);
            return;
          }
          finalError = error;
        } else {
          const error = this.fileSystem.mkdir(path, 0o700);
          if (error === null) {
            write(call, 0, call.allocateString(path));
            call.clearError(call.destination + 1);
            return;
          }
          finalError = error;
        }
        if (finalError !== 'file already exists') break;
      }
      if (file) {
        write(call, 0, 0n);
        write(call, 1, 0n);
        this.writeOsError(call, call.destination + 2, finalError);
      } else {
        write(call, 0, 0n);
        this.writeOsError(call, call.destination + 1, finalError);
      }
    } catch (error) {
      if (file) {
        write(call, 0, 0n);
        write(call, 1, 0n);
        this.writeOsError(call, call.destination + 2, (error as Error).message);
      } else {
        write(call, 0, 0n);
        this.writeOsError(call, call.destination + 1, (error as Error).message);
      }
    }
  }

  private handleNet(call: AotExternCall, name: string): void {
    if (name === 'getNetErrors') {
      NET_ERROR_MESSAGES.forEach((message, index) => {
        call.writeError(call.destination + index * 2, message);
      });
    } else if (NET_INT_ERROR.has(name)) {
      write(call, 0, BigInt.asUintN(64, -1n));
      call.writeError(call.destination + 1, NET_UNSUPPORTED);
    } else if (NET_COUNT_ERROR.has(name)) {
      write(call, 0, 0n);
      call.writeError(call.destination + 1, NET_UNSUPPORTED);
    } else if (NET_ERROR_ONLY.has(name)) {
      call.writeError(call.destination, NET_UNSUPPORTED);
    } else if (NET_EMPTY_STRING.has(name)) {
      write(call, 0, 0n);
    } else if (name === 'blocking_udpConnReadFrom') {
      write(call, 0, 0n);
      write(call, 1, 0n);
      call.writeError(call.destination + 2, NET_UNSUPPORTED);
    } else if (NET_REF_ERROR.has(name)) {
      write(call, 0, 0n);
      call.writeError(call.destination + 1, NET_UNSUPPORTED);
    }
  }

  private handleHttp(call: AotExternCall, name: string): number | void | Promise<void> {
    if (name === 'getHttpErrors') {
      call.writeError(call.destination, HTTP_TIMEOUT_MESSAGE);
      return;
    }
    if (name === 'nativeNewClientRequest') {
      if (this.nextHttpRequest > Number.MAX_SAFE_INTEGER) {
        write(call, 0, 0n);
        call.writeError(call.destination + 1, 'http: request id space exhausted');
        return;
      }
      const id = this.nextHttpRequest;
      this.nextHttpRequest += 1;
      this.httpRequests.set(id, undefined);
      write(call, 0, BigInt(id));
      call.clearError(call.destination + 1);
      return;
    }
    const id = safeNumber(argument(call, 0));
    if (name === 'nativeCancelClientRequest') {
      if (id !== undefined) this.httpRequests.get(id)?.controller.abort();
      return;
    }
    if (name === 'nativeReleaseClientRequest') {
      if (id !== undefined) {
        this.httpRequests.get(id)?.controller.abort();
        this.httpRequests.delete(id);
      }
      return;
    }
    return this.performHttpRequest(call, id);
  }

  private writeHttpError(call: AotExternCall, message: string): void {
    for (let index = 0; index < 5; index += 1) write(call, index, 0n);
    call.writeError(call.destination + 5, message);
  }

  private async performHttpRequest(call: AotExternCall, id: number | undefined): Promise<void> {
    if (id === undefined || !this.httpRequests.has(id)) {
      this.writeHttpError(call, `http: unknown request id ${id ?? 0}`);
      return;
    }
    let method: string;
    let url: string;
    let headers: readonly string[];
    try {
      method = decodeUtf8(call.readStringBytes(argument(call, 1)), 'http: method contains invalid UTF-8');
      url = decodeUtf8(call.readStringBytes(argument(call, 2)), 'http: URL contains invalid UTF-8');
      headers = call.readStringSlice(argument(call, 3));
    } catch (error) {
      this.writeHttpError(call, (error as Error).message);
      return;
    }
    if (typeof fetch !== 'function') {
      this.writeHttpError(call, 'http: Fetch API is unavailable');
      return;
    }
    const controller = new AbortController();
    this.httpRequests.set(id, { controller });
    const requestHeaders = new Headers();
    for (const line of headers) {
      const separator = line.indexOf(':');
      if (separator > 0) requestHeaders.append(line.slice(0, separator).trim(), line.slice(separator + 1).trimStart());
    }
    const body = call.readByteSlice(argument(call, 4));
    const timeoutNanoseconds = signed(argument(call, 5));
    let timeout: ReturnType<typeof setTimeout> | undefined;
    let timedOut = false;
    if (timeoutNanoseconds > 0n) {
      const milliseconds = Number((timeoutNanoseconds + 999_999n) / 1_000_000n);
      timeout = setTimeout(() => {
        timedOut = true;
        controller.abort();
      }, Math.min(milliseconds, 2_147_483_647));
    }
    try {
      const response = await fetch(url, {
        method,
        headers: requestHeaders,
        body: body.byteLength === 0 ? undefined : body.slice().buffer as ArrayBuffer,
        signal: controller.signal,
      });
      const responseBody = new Uint8Array(await response.arrayBuffer());
      const responseHeaders: string[] = [];
      response.headers.forEach((value, key) => responseHeaders.push(`${key}: ${value}`));
      write(call, 0, BigInt(response.status));
      write(call, 1, call.allocateString(`HTTP/1.1 ${response.status} ${response.statusText}`));
      write(call, 2, call.allocateString('HTTP/1.1'));
      write(call, 3, call.allocateStringSlice(responseHeaders));
      write(call, 4, call.allocateByteSlice(responseBody));
      call.clearError(call.destination + 5);
    } catch (error) {
      const canceled = controller.signal.aborted && !timedOut;
      this.writeHttpError(
        call,
        timedOut ? HTTP_TIMEOUT_MESSAGE : (canceled ? 'request canceled' : String(error)),
      );
    } finally {
      if (timeout !== undefined) clearTimeout(timeout);
    }
  }
}
