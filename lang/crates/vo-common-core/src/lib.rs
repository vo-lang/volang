//! # vo-common-core
//!
//! Core types for Vo that are `no_std` compatible.
//!
//! This crate provides foundational types used by the VM runtime:
//! - `ValueKind` - Runtime type classification
//! - `symbol` - Symbol type (no_std) and SymbolInterner (std feature)
//! - `runtime_type` - Runtime type representation for type identity
//! - `instruction` - Bytecode instruction format and opcodes
//! - `bytecode` - Module and function definitions

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

pub mod bytecode;
pub mod debug_info;
pub mod extern_key;
pub mod identifier;
pub mod instruction;
pub mod log_record;
pub mod runtime_type;
pub mod serialize;
pub mod source_provider;
pub mod struct_tag;
pub mod symbol;
pub mod types;
pub mod verifier;

pub use bytecode::{
    Constant, ExternDef, ExternEffects, ExternJitRoute, FunctionDef, GlobalDef, InterfaceMeta,
    Itab, JitInstructionMetadata, Module, ParamShape, ProviderTrust, RegisteredExternSource,
    ResolvedExtern, ResolvedExternAbi, ResolvedExternTable, ResolvedExternTableError, ReturnShape,
    StructMeta, TransferType,
};
pub use debug_info::{DebugInfo, DebugLoc, FuncDebugInfo, SourceLoc};
pub use extern_key::{
    classify_extern_name, decode_extern_name, is_portable_package_component,
    is_vm_internal_extern_name, is_vm_variable_shape_extern_name,
    validate_canonical_extern_identity, validate_canonical_module_owner,
    validate_canonical_package_path, wasm_extension_export_key, CanonicalExternIdentityError,
    CanonicalModuleOwnerError, CanonicalPackagePathError, ExternKey, ExternKeyError,
    ExternKeyField, ExternKeyRef, ExternNameClass, ExternNameError, EXTERN_NAME_CODEC_VERSION,
    EXTERN_NAME_PREFIX, MAX_CANONICAL_MODULE_OWNER_BYTES, MAX_CANONICAL_PACKAGE_PATH_BYTES,
    MAX_EXTERN_NAME_BYTES, MAX_PORTABLE_PACKAGE_COMPONENT_BYTES, VM_INTERNAL_EXTERN_NAMES,
    VM_VARIABLE_SHAPE_EXTERN_NAMES, WASM_EXTENSION_EXPORT_PREFIX, WASM_EXTENSION_PROTOCOL_VERSION,
};
pub use identifier::{
    build_local_type_identity, has_unicode_white_space_boundary, is_exported_name, is_identifier,
    is_identifier_continue, is_identifier_start, is_keyword, is_local_type_identity,
    is_named_declaration_identifier, is_unicode_control, is_unicode_white_space,
    LocalTypeIdentityError, LOCAL_TYPE_IDENTITY_MARKER, MAX_NAMED_TYPE_IDENTITY_BYTES,
    UNICODE_PROFILE_VERSION, VO_KEYWORDS,
};
pub use instruction::{Instruction, Opcode};
pub use log_record::LogRecordCore;
pub use runtime_type::{ChanDir, InterfaceMethod, RuntimeType, StructField};
pub use source_provider::{NoSource, SourceProvider};
pub use struct_tag::lookup_struct_tag_value;
pub use symbol::Symbol;
#[cfg(feature = "std")]
pub use symbol::SymbolInterner;
pub use types::{
    elem_flags, MetaId, SlotType, ValueKind, ValueMeta, ELEM_FLAG_BYTES_MASK, ELEM_FLAG_FLOAT32,
    ELEM_FLAG_FLOAT_BIT, ELEM_FLAG_INT16, ELEM_FLAG_INT32, ELEM_FLAG_INT8, ELEM_FLAG_SIGN_BIT,
};
pub use verifier::{ModuleVerificationError, ModuleVerifier, VerifiedModule};
