/** Public contracts for generated Core Wasm execution. */
/** Memory guarantees advertised by the compiler's versioned target contract. */
export type AotMemoryContract = 'island-span-heap' | 'instance-tracing';

export interface AotManifest {
  readonly abiVersion: number;
  readonly target: string;
  readonly semanticModuleLength: number;
  readonly memoryPages: number;
  readonly memoryContract: AotMemoryContract;
  readonly moduleSha256: Uint8Array;
}

export interface AotExecutionResult {
  readonly status: 'ok' | 'error';
  readonly stdout: string;
  readonly stderr: string;
  readonly exitCode?: number;
}

export interface AotRunResult {
  readonly memoryStats: ReturnType<import('./aot_memory.js').AotMemoryRuntime['stats']>;
  readonly schedulerStats: ReturnType<import('./aot_memory.js').AotMemoryRuntime['schedulerStats']>;
  readonly instance: WebAssembly.Instance;
  readonly manifest: AotManifest;
  readonly result: AotExecutionResult;
  readonly exitCode: number;
}

export interface AotExternCall {
  readonly descriptor: AotExternDescriptor;
  readonly name: string;
  readonly externId: number;
  readonly memory: WebAssembly.Memory;
  readonly frame: number;
  readonly destination: number;
  readonly argumentsStart: number;
  readonly argumentSlots: number;
  readonly args: readonly string[];
  readSlot(slot: number): bigint;
  writeSlot(slot: number, value: bigint): void;
  readFloat64(slot: number): number;
  writeFloat64(slot: number, value: number): void;
  readString(reference: bigint): string;
  readStringBytes(reference: bigint): Uint8Array;
  readStringSlice(reference: bigint): readonly string[];
  readByteSlice(reference: bigint): Uint8Array;
  writeByteSlice(reference: bigint, bytes: Uint8Array): number;
  allocate(bytes: number): number;
  lease(reference: bigint): import('./aot_memory.js').AotGcLease;
  allocateSequence(bytes: number, elementMeta: number): number;
  allocateString(value: string): bigint;
  allocateStringBytes(value: Uint8Array): bigint;
  allocateStringSlice(values: readonly string[]): bigint;
  allocateStringBytesSlice(values: readonly Uint8Array[]): bigint;
  allocateByteSlice(value: Uint8Array): bigint;
  allocateIntSlice(values: readonly bigint[]): bigint;
  allocateInterfaceSlice(values: readonly (readonly [bigint, bigint])[]): bigint;
  allocateNamedStructSlice(
    typeName: string,
    values: readonly Readonly<Record<string, bigint>>[],
  ): bigint;
  writeError(slot: number, message: string, cause?: readonly [bigint, bigint]): void;
  clearError(slot: number): void;
  writeOutput(fd: number, bytes: Uint8Array): void;
  exit(code: number): number;
  panic(message: string): number;
}

export type AotExternHandler = (
  call: AotExternCall,
) => number | void | Promise<number | void>;

export interface AotExternDescriptor {
  readonly id: number;
  readonly name: string;
  readonly required: boolean;
  readonly paramSlots?: number;
  readonly returnSlots: number;
  readonly allowedEffects: bigint;
  readonly effectiveEffects: bigint;
  readonly abiFingerprint: bigint;
  readonly providerIdentity: bigint;
  readonly source: number;
  readonly returnSlotTypes: Uint8Array;
}

export interface AotExternProvider {
  readonly handler: AotExternHandler;
  readonly abiFingerprint?: bigint;
  readonly supportedEffects?: bigint;
}

export interface AotRunOptions {
  readonly memory?: import('./aot_memory.js').AotMemoryOptions;
  /** Called after admission, before guest execution. Retain controls for host turns. */
  readonly onMemory?: (controls: import('./aot_memory.js').AotMemoryControl) => void;
  readonly args?: readonly string[];
  /** Complete process-standard-input byte stream. Reads consume one line at a time. */
  readonly stdin?: string | Uint8Array;
  readonly externs?: Readonly<Record<string, AotExternHandler | AotExternProvider>>;
  /** Ceiling for the entire instance, including all Islands and runtime storage. */
  readonly memoryLimitPages?: number;
  /** Reject incompatible images before instantiation or guest execution. */
  readonly requireMemoryContract?: AotMemoryContract;
  /** Guest basic-block budget. Omit for unlimited execution. */
  readonly fuel?: number | bigint;
  /** Optional precompiled vo-aot-support-wasm module for non-browser hosts. */
  readonly supportModule?: BufferSource | WebAssembly.Module;
}
