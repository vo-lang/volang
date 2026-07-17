export interface BoundedJsonOptions {
  maxBytes?: number;
  maxDepth?: number;
  maxTokens?: number;
  maxObjectKeys?: number;
  maxObjectKeyBytes?: number;
}

export declare function parseBoundedJsonBytes<T = unknown>(
  bytes: Uint8Array,
  label?: string,
  options?: BoundedJsonOptions,
): T;
