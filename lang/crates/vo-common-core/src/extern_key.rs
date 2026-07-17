//! Canonical identity and wire encoding for external functions.
//!
//! An extern is identified by the exact pair `(package, function)`.  The wire
//! representation uses UTF-8 byte lengths, so punctuation, Unicode, path
//! boundaries, and the boundary between the two tuple fields all remain
//! distinct.

#[cfg(not(feature = "std"))]
use alloc::{
    collections::{BTreeMap, BTreeSet},
    string::String,
};
use core::borrow::Borrow;
use core::fmt;
use core::fmt::Write as _;
#[cfg(feature = "std")]
use std::{
    collections::{BTreeMap, BTreeSet},
    string::String,
};

use icu_normalizer::ComposingNormalizerBorrowed;

/// Semantic epoch of the canonical extern-name codec.
pub const EXTERN_NAME_CODEC_VERSION: u32 = 1;

/// Prefix of every canonical encoded extern name.
pub const EXTERN_NAME_PREFIX: &str = "vo1:";

/// Protocol epoch for browser-loaded WASM extensions.
///
/// This value is shared by SDK-generated artifacts and every browser host.
/// Bump it whenever the required exported ABI changes incompatibly.
pub const WASM_EXTENSION_PROTOCOL_VERSION: u32 = 3;

/// Prefix of every browser WASM extension function export.
///
/// The suffix is the lowercase hexadecimal encoding of every UTF-8 byte in
/// the complete canonical extern name.  It is deliberately untruncated and
/// unhashed so routing remains injective and independently reproducible.
pub const WASM_EXTENSION_EXPORT_PREFIX: &str = "__vo_ext_";

/// Maximum encoded extern name accepted across bytecode and provider ABIs.
///
/// This matches the current native entry-table bound.  Keeping the limit in
/// the no-std common layer lets compilers, bytecode verifiers, and providers
/// reject an oversized identity before it reaches a platform-specific loader.
pub const MAX_EXTERN_NAME_BYTES: usize = 4 * 1024;

/// Maximum byte length of a canonical published module path.
///
/// Module paths are also encoded as one portable cache component by replacing
/// `/` with `@`, so this matches the cross-platform component ceiling.
pub const MAX_CANONICAL_MODULE_OWNER_BYTES: usize = 255;

/// Maximum UTF-8 byte length of one portable descendant package component.
pub const MAX_PORTABLE_PACKAGE_COMPONENT_BYTES: usize = 255;

/// Maximum UTF-8 byte length of a canonical package import path.
pub const MAX_CANONICAL_PACKAGE_PATH_BYTES: usize = 4 * 1024;

/// VM-owned extern helpers whose compact names are part of the bytecode ABI.
///
/// Source-level and extension-provided externs use the canonical length-coded
/// representation. These names are the complete exception set for helpers
/// synthesized by the compiler and implemented inside the VM runtime.
pub const VM_INTERNAL_EXTERN_NAMES: &[&str] = &[
    "dyn_call",
    "dyn_field",
    "dyn_index",
    "dyn_method",
    "dyn_pack_any_slice",
    "dyn_set_field",
    "dyn_set_index_unified",
    "dyn_type_assert_error",
    "panic_with_error",
    "vo_assert",
    "vo_conv_bytes_str",
    "vo_conv_int_str",
    "vo_conv_runes_str",
    "vo_conv_str_bytes",
    "vo_conv_str_runes",
    "vo_copy",
    "vo_iface_eq",
    "vo_print",
    "vo_println",
    "vo_slice_append_slice",
];

/// VM-owned helpers whose ABI shape is specialized per compiler call site.
///
/// These names may occur at multiple extern IDs with distinct parameter or
/// return layouts. Every other same-name extern must keep one fixed ABI shape.
pub const VM_VARIABLE_SHAPE_EXTERN_NAMES: &[&str] =
    &["dyn_call", "dyn_field", "dyn_index", "dyn_method"];

/// Classification of a bytecode-visible extern name.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternNameClass<'a> {
    /// A source/provider identity encoded by the canonical extern codec.
    Canonical(ExternKeyRef<'a>),
    /// A compiler-synthesized helper owned by the VM runtime.
    VmInternal,
}

/// Failure to validate a source-level package identity carried by bytecode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalPackagePathError {
    Empty,
    TooLong { len: usize, max: usize },
    InvalidComponent { index: usize },
    InvalidExternalRoot,
    NonCanonicalExternalRoot(CanonicalModuleOwnerError),
    ReservedStdPrefix,
    InvalidLocalIdentity,
}

impl fmt::Display for CanonicalPackagePathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => f.write_str("canonical package path must not be empty"),
            Self::TooLong { len, max } => write!(
                f,
                "canonical package path is {len} bytes, exceeding the {max}-byte limit"
            ),
            Self::InvalidComponent { index } => write!(
                f,
                "canonical package path component {index} is not a normalized portable component"
            ),
            Self::InvalidExternalRoot => f.write_str(
                "external package path must contain github.com/<owner>/<repository>",
            ),
            Self::NonCanonicalExternalRoot(error) => {
                write!(f, "external package has a non-canonical module root: {error}")
            }
            Self::ReservedStdPrefix => f.write_str(
                "canonical package path must not use the reserved 'std' import prefix",
            ),
            Self::InvalidLocalIdentity => f.write_str(
                "ephemeral package identity must be exactly local/<name> with a canonical local name",
            ),
        }
    }
}

impl core::error::Error for CanonicalPackagePathError {}

/// Failure to validate the language meaning of a decoded extern identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalExternIdentityError {
    Package(CanonicalPackagePathError),
    InvalidFunctionIdentifier,
    BlankFunctionIdentifier,
    KeywordFunctionIdentifier,
}

impl fmt::Display for CanonicalExternIdentityError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Package(error) => write!(f, "invalid extern package: {error}"),
            Self::InvalidFunctionIdentifier => {
                f.write_str("extern function must be one complete Vo identifier")
            }
            Self::BlankFunctionIdentifier => {
                f.write_str("extern function cannot be the blank identifier '_'")
            }
            Self::KeywordFunctionIdentifier => {
                f.write_str("extern function cannot be a Vo keyword")
            }
        }
    }
}

impl core::error::Error for CanonicalExternIdentityError {}

/// Failure to classify a bytecode-visible extern name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExternNameError {
    /// The name selected the canonical namespace but its wire encoding failed.
    MalformedCanonical(ExternKeyError),
    /// The wire name decoded, but its fields cannot identify a source extern.
    InvalidCanonicalIdentity(CanonicalExternIdentityError),
    /// The name is outside the canonical namespace and VM helper whitelist.
    UnrecognizedNonCanonical,
}

impl fmt::Display for ExternNameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MalformedCanonical(error) => {
                write!(f, "malformed canonical extern name: {error}")
            }
            Self::InvalidCanonicalIdentity(error) => {
                write!(f, "invalid canonical extern identity: {error}")
            }
            Self::UnrecognizedNonCanonical => f.write_str(
                "extern name must use the canonical codec or an exact VM-internal helper name",
            ),
        }
    }
}

impl core::error::Error for ExternNameError {}

/// Return whether `name` is one of the exact compiler/VM helper identities.
#[inline]
pub fn is_vm_internal_extern_name(name: &str) -> bool {
    VM_INTERNAL_EXTERN_NAMES.contains(&name)
}

/// Return whether `name` is a VM helper with a call-site-specialized ABI.
#[inline]
pub fn is_vm_variable_shape_extern_name(name: &str) -> bool {
    VM_VARIABLE_SHAPE_EXTERN_NAMES.contains(&name)
}

/// Classify a bytecode-visible extern name using the complete ABI policy.
pub fn classify_extern_name(name: &str) -> Result<ExternNameClass<'_>, ExternNameError> {
    match decode_extern_name(name) {
        Ok(key) => {
            validate_canonical_extern_identity(key)
                .map_err(ExternNameError::InvalidCanonicalIdentity)?;
            Ok(ExternNameClass::Canonical(key))
        }
        Err(error) if name.starts_with(EXTERN_NAME_PREFIX) => {
            Err(ExternNameError::MalformedCanonical(error))
        }
        Err(_) if is_vm_internal_extern_name(name) => Ok(ExternNameClass::VmInternal),
        Err(_) => Err(ExternNameError::UnrecognizedNonCanonical),
    }
}

/// Failure to validate a canonical published module owner.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CanonicalModuleOwnerError {
    Empty,
    InvalidBoundary,
    InvalidRoot,
    MissingOwnerOrRepository,
    TooLong { len: usize, max: usize },
    EmptySegment { index: usize },
    InvalidSegment { index: usize },
    NonPortableSegment { index: usize },
    GitRefIncompatibleSegment { index: usize },
    InvalidMajorVersion,
    MajorVersionOverflow,
}

impl fmt::Display for CanonicalModuleOwnerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Empty => f.write_str("canonical module owner must not be empty"),
            Self::InvalidBoundary => {
                f.write_str("canonical module owner must not start or end with '/'")
            }
            Self::InvalidRoot => {
                f.write_str("canonical module owner must begin with 'github.com/'")
            }
            Self::MissingOwnerOrRepository => {
                f.write_str("canonical module owner must contain github.com/<owner>/<repository>")
            }
            Self::TooLong { len, max } => write!(
                f,
                "canonical module owner is {len} bytes, exceeding the {max}-byte portable limit"
            ),
            Self::EmptySegment { index } => {
                write!(f, "canonical module owner segment {index} is empty")
            }
            Self::InvalidSegment { index } => write!(
                f,
                "canonical module owner segment {index} uses an invalid spelling"
            ),
            Self::NonPortableSegment { index } => write!(
                f,
                "canonical module owner segment {index} is not a portable path component"
            ),
            Self::GitRefIncompatibleSegment { index } => write!(
                f,
                "canonical module owner subdirectory segment {index} cannot be represented in a Git release tag"
            ),
            Self::InvalidMajorVersion => {
                f.write_str("canonical module owner major suffix must be unpadded vN with N >= 2")
            }
            Self::MajorVersionOverflow => {
                f.write_str("canonical module owner major suffix overflows u64")
            }
        }
    }
}

impl core::error::Error for CanonicalModuleOwnerError {}

/// Validate the canonical published `ModulePath` spelling used by `vo.mod`.
///
/// Keeping this no-std predicate beside extern identity lets compilers,
/// caches, native loaders, and WASM routers enforce one owner boundary without
/// linking the module resolver into runtime artifacts.
pub fn validate_canonical_module_owner(owner: &str) -> Result<(), CanonicalModuleOwnerError> {
    if owner.is_empty() {
        return Err(CanonicalModuleOwnerError::Empty);
    }
    if owner.starts_with('/') || owner.ends_with('/') {
        return Err(CanonicalModuleOwnerError::InvalidBoundary);
    }
    if !owner.starts_with("github.com/") {
        return Err(CanonicalModuleOwnerError::InvalidRoot);
    }
    if owner.len() > MAX_CANONICAL_MODULE_OWNER_BYTES {
        return Err(CanonicalModuleOwnerError::TooLong {
            len: owner.len(),
            max: MAX_CANONICAL_MODULE_OWNER_BYTES,
        });
    }

    let mut segment_count = 0usize;
    let mut last = "";
    for (index, segment) in owner.split('/').enumerate() {
        segment_count += 1;
        last = segment;
        if segment.is_empty() {
            return Err(CanonicalModuleOwnerError::EmptySegment { index });
        }
        if !is_canonical_module_segment(segment) {
            return Err(CanonicalModuleOwnerError::InvalidSegment { index });
        }
        if !is_portable_ascii_module_segment(segment) {
            return Err(CanonicalModuleOwnerError::NonPortableSegment { index });
        }
        // Repository-root releases use a plain `vX.Y.Z` tag. Subdirectory
        // releases embed every segment after the repository in the Git ref,
        // where `..` and a `.lock` component suffix are forbidden.
        if index >= 3 && (segment.contains("..") || segment.ends_with(".lock")) {
            return Err(CanonicalModuleOwnerError::GitRefIncompatibleSegment { index });
        }
    }
    if segment_count < 3 {
        return Err(CanonicalModuleOwnerError::MissingOwnerOrRepository);
    }

    if segment_count > 3 {
        if let Some(digits) = last.strip_prefix('v') {
            if !digits.is_empty() && digits.bytes().all(|byte| byte.is_ascii_digit()) {
                if (digits.len() > 1 && digits.starts_with('0')) || digits == "0" || digits == "1" {
                    return Err(CanonicalModuleOwnerError::InvalidMajorVersion);
                }
                digits
                    .parse::<u64>()
                    .map_err(|_| CanonicalModuleOwnerError::MajorVersionOverflow)?;
            }
        }
    }
    Ok(())
}

fn is_canonical_module_segment(segment: &str) -> bool {
    let mut bytes = segment.bytes();
    let Some(first) = bytes.next() else {
        return false;
    };
    (first.is_ascii_lowercase() || first.is_ascii_digit())
        && bytes.all(|byte| {
            byte.is_ascii_lowercase() || byte.is_ascii_digit() || matches!(byte, b'.' | b'_' | b'-')
        })
}

fn is_portable_ascii_module_segment(segment: &str) -> bool {
    if segment.len() > MAX_CANONICAL_MODULE_OWNER_BYTES || segment.ends_with('.') {
        return false;
    }
    let stem = segment.split('.').next().unwrap_or(segment);
    if matches!(stem, "con" | "prn" | "aux" | "nul" | "conin$" | "conout$") {
        return false;
    }
    !is_numbered_windows_device_stem(stem, "com") && !is_numbered_windows_device_stem(stem, "lpt")
}

fn is_numbered_windows_device_stem(stem: &str, prefix: &str) -> bool {
    stem.strip_prefix(prefix)
        .is_some_and(|suffix| suffix.len() == 1 && matches!(suffix.as_bytes()[0], b'1'..=b'9'))
}

/// Return whether one package-path component has the canonical portable
/// spelling required by source imports and extension ownership.
pub fn is_portable_package_component(segment: &str) -> bool {
    if segment.is_empty()
        || segment.len() > MAX_PORTABLE_PACKAGE_COMPONENT_BYTES
        || segment == "."
        || segment == ".."
        || crate::identifier::has_unicode_white_space_boundary(segment)
        || segment.ends_with('.')
        || segment.contains('/')
        || segment.contains('\\')
        || segment.contains('@')
        || segment.chars().any(|character| {
            crate::identifier::is_unicode_control(character) || r#"<>:"|?*"#.contains(character)
        })
    {
        return false;
    }
    if !ComposingNormalizerBorrowed::new_nfc().is_normalized(segment) {
        return false;
    }
    let stem = segment.split('.').next().unwrap_or(segment);
    let stem_key = crate::identifier::portable_case_key(stem);
    if ["con", "prn", "aux", "nul", "conin$", "conout$"].contains(&stem_key.as_str()) {
        return false;
    }
    !is_numbered_windows_device_package_stem(&stem_key, "com")
        && !is_numbered_windows_device_package_stem(&stem_key, "lpt")
}

fn is_numbered_windows_device_package_stem(stem: &str, prefix: &str) -> bool {
    let Some(suffix) = stem.strip_prefix(prefix) else {
        return false;
    };
    let mut suffix = suffix.chars();
    matches!(suffix.next(), Some('1'..='9' | '¹' | '²' | '³')) && suffix.next().is_none()
}

fn is_canonical_local_name(name: &str) -> bool {
    let mut bytes = name.bytes();
    let Some(first) = bytes.next() else {
        return false;
    };
    is_portable_package_component(name)
        && (first.is_ascii_lowercase() || first.is_ascii_digit())
        && bytes.all(|byte| {
            byte.is_ascii_lowercase() || byte.is_ascii_digit() || matches!(byte, b'.' | b'_' | b'-')
        })
}

/// Validate the canonical package identity accepted by the source frontend.
///
/// Published imports use a canonical `github.com/<owner>/<repository>` root;
/// stdlib-style paths have no dotted first component and cannot use the
/// reserved `std/` spelling; ephemeral roots use exactly `local/<name>`.
pub fn validate_canonical_package_path(path: &str) -> Result<(), CanonicalPackagePathError> {
    if path.is_empty() {
        return Err(CanonicalPackagePathError::Empty);
    }
    if path.len() > MAX_CANONICAL_PACKAGE_PATH_BYTES {
        return Err(CanonicalPackagePathError::TooLong {
            len: path.len(),
            max: MAX_CANONICAL_PACKAGE_PATH_BYTES,
        });
    }

    if let Some(name) = path.strip_prefix("local/") {
        if name.contains('/') || !is_canonical_local_name(name) {
            return Err(CanonicalPackagePathError::InvalidLocalIdentity);
        }
        return Ok(());
    }
    if path == "local" {
        return Err(CanonicalPackagePathError::InvalidLocalIdentity);
    }

    let mut component_count = 0usize;
    let mut first_component = "";
    for (index, component) in path.split('/').enumerate() {
        if index == 0 {
            first_component = component;
        }
        component_count += 1;
        if !is_portable_package_component(component) {
            return Err(CanonicalPackagePathError::InvalidComponent { index });
        }
    }

    if first_component.contains('.') {
        if component_count < 3 {
            return Err(CanonicalPackagePathError::InvalidExternalRoot);
        }
        let mut slashes = path.match_indices('/').map(|(index, _)| index);
        let _host_boundary = slashes
            .next()
            .ok_or(CanonicalPackagePathError::InvalidExternalRoot)?;
        let _owner_boundary = slashes
            .next()
            .ok_or(CanonicalPackagePathError::InvalidExternalRoot)?;
        let root_end = slashes.next().unwrap_or(path.len());
        validate_canonical_module_owner(&path[..root_end])
            .map_err(CanonicalPackagePathError::NonCanonicalExternalRoot)?;
    } else if first_component == "std" {
        return Err(CanonicalPackagePathError::ReservedStdPrefix);
    }

    Ok(())
}

/// Validate the source-language meaning of a decoded extern identity.
pub fn validate_canonical_extern_identity(
    key: ExternKeyRef<'_>,
) -> Result<(), CanonicalExternIdentityError> {
    validate_canonical_package_path(key.package())
        .map_err(CanonicalExternIdentityError::Package)?;
    if key.function() == "_" {
        return Err(CanonicalExternIdentityError::BlankFunctionIdentifier);
    }
    if crate::identifier::is_keyword(key.function()) {
        return Err(CanonicalExternIdentityError::KeywordFunctionIdentifier);
    }
    if !crate::identifier::is_identifier(key.function()) {
        return Err(CanonicalExternIdentityError::InvalidFunctionIdentifier);
    }
    Ok(())
}

/// One field of an [`ExternKey`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternKeyField {
    Package,
    Function,
}

impl fmt::Display for ExternKeyField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Package => f.write_str("package"),
            Self::Function => f.write_str("function"),
        }
    }
}

/// Failure to construct or decode a canonical extern key.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExternKeyError {
    EmptyField(ExternKeyField),
    EncodedNameTooLong { len: usize, max: usize },
    InvalidPrefix,
    MissingLength(ExternKeyField),
    InvalidLengthDigit(ExternKeyField),
    NonCanonicalLength(ExternKeyField),
    LengthOverflow(ExternKeyField),
    TruncatedField(ExternKeyField),
    LengthSplitsUtf8(ExternKeyField),
    MissingSeparatorAfterPackage,
    TrailingBytes,
}

impl fmt::Display for ExternKeyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyField(field) => write!(f, "extern {field} must not be empty"),
            Self::EncodedNameTooLong { len, max } => write!(
                f,
                "encoded extern name is {len} bytes, exceeding the {max}-byte limit"
            ),
            Self::InvalidPrefix => write!(
                f,
                "extern name must begin with canonical prefix '{EXTERN_NAME_PREFIX}'"
            ),
            Self::MissingLength(field) => {
                write!(f, "extern {field} length is missing or unterminated")
            }
            Self::InvalidLengthDigit(field) => {
                write!(f, "extern {field} length contains a non-decimal byte")
            }
            Self::NonCanonicalLength(field) => write!(
                f,
                "extern {field} length must be a non-zero decimal without leading zeroes"
            ),
            Self::LengthOverflow(field) => {
                write!(f, "extern {field} length overflows the supported size")
            }
            Self::TruncatedField(field) => {
                write!(f, "extern {field} is shorter than its declared byte length")
            }
            Self::LengthSplitsUtf8(field) => {
                write!(f, "extern {field} length splits a UTF-8 scalar value")
            }
            Self::MissingSeparatorAfterPackage => {
                f.write_str("extern package must be followed by ':'")
            }
            Self::TrailingBytes => {
                f.write_str("extern name contains bytes after the declared function")
            }
        }
    }
}

impl core::error::Error for ExternKeyError {}

/// Borrowed logical identity of one external function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExternKeyRef<'a> {
    package: &'a str,
    function: &'a str,
}

impl<'a> ExternKeyRef<'a> {
    pub const fn new(package: &'a str, function: &'a str) -> Self {
        Self { package, function }
    }

    pub const fn package(self) -> &'a str {
        self.package
    }

    pub const fn function(self) -> &'a str {
        self.function
    }

    /// Returns whether this key's package belongs to `module`.
    ///
    /// Ownership follows canonical import-path boundaries: a module owns its
    /// exact root package and portable descendants beginning with `module/`.
    /// Both the published module owner and every descendant package segment
    /// must have canonical path spelling; lexical traversal and platform path
    /// aliases never establish ownership.
    pub fn is_owned_by_module(self, module: &str) -> bool {
        if validate_canonical_module_owner(module).is_err()
            || self.package.len() > MAX_EXTERN_NAME_BYTES
        {
            return false;
        }
        if self.package == module {
            return true;
        }
        let Some(suffix) = self
            .package
            .strip_prefix(module)
            .and_then(|suffix| suffix.strip_prefix('/'))
        else {
            return false;
        };
        !suffix.is_empty() && suffix.split('/').all(is_portable_package_component)
    }

    /// Encode as `vo1:<pkg_len>:<pkg>:<func_len>:<func>`.
    pub fn encode(self) -> Result<String, ExternKeyError> {
        validate_non_empty(self.package, ExternKeyField::Package)?;
        validate_non_empty(self.function, ExternKeyField::Function)?;
        let len = encoded_len(self.package.len(), self.function.len())?;
        if len > MAX_EXTERN_NAME_BYTES {
            return Err(ExternKeyError::EncodedNameTooLong {
                len,
                max: MAX_EXTERN_NAME_BYTES,
            });
        }

        let mut encoded = String::with_capacity(len);
        write!(
            encoded,
            "{EXTERN_NAME_PREFIX}{}:{}:{}:{}",
            self.package.len(),
            self.package,
            self.function.len(),
            self.function
        )
        .expect("writing an extern key to String cannot fail");
        debug_assert_eq!(encoded.len(), len);
        Ok(encoded)
    }

    /// Encode the collision-free browser WASM export key for this extern.
    pub fn wasm_extension_export_key(self) -> Result<String, ExternKeyError> {
        let encoded = self.encode()?;
        Ok(wasm_extension_export_key_from_canonical(&encoded))
    }

    pub fn to_owned(self) -> Result<ExternKey, ExternKeyError> {
        ExternKey::try_new(self.package, self.function)
    }
}

/// Select the most specific loaded module owner for an extern key.
///
/// A package can be structurally owned by both a parent module and a nested
/// module. Language routing assigns the complete package to the longest
/// matching canonical owner boundary before looking up a function in that
/// provider. Consequently, a missing function in the nested provider never
/// falls back to the parent provider. The deduplicated ordered index is probed
/// only at package boundaries from deepest to shallowest, avoiding a scan of
/// every loaded owner.
/// Ordered exact-owner index used by [`deepest_owning_module`].
///
/// Both sets and maps implement this contract, allowing callers to retain
/// artifact metadata beside each owner without rebuilding a second index.
mod owner_index_sealed {
    use super::{BTreeMap, BTreeSet, Borrow};

    pub trait Sealed {}

    impl<T> Sealed for BTreeSet<T> where T: Borrow<str> + Ord {}
    impl<T, V> Sealed for BTreeMap<T, V> where T: Borrow<str> + Ord {}
}

#[doc(hidden)]
pub trait ModuleOwnerIndex: owner_index_sealed::Sealed {
    fn exact_owner<'owner>(&'owner self, candidate: &str) -> Option<&'owner str>;
}

impl<T> ModuleOwnerIndex for BTreeSet<T>
where
    T: Borrow<str> + Ord,
{
    fn exact_owner<'owner>(&'owner self, candidate: &str) -> Option<&'owner str> {
        self.get(candidate).map(Borrow::borrow)
    }
}

impl<T, V> ModuleOwnerIndex for BTreeMap<T, V>
where
    T: Borrow<str> + Ord,
{
    fn exact_owner<'owner>(&'owner self, candidate: &str) -> Option<&'owner str> {
        self.get_key_value(candidate)
            .map(|(owner, _)| owner.borrow())
    }
}

pub fn deepest_owning_module<'owner, I>(
    key: ExternKeyRef<'_>,
    owners: &'owner I,
) -> Option<&'owner str>
where
    I: ModuleOwnerIndex + ?Sized,
{
    let mut candidate = key.package();
    loop {
        if let Some(owner) = owners.exact_owner(candidate) {
            if key.is_owned_by_module(owner) {
                return Some(owner);
            }
        }
        candidate = candidate.rsplit_once('/')?.0;
    }
}

/// Owned logical identity of one external function.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternKey {
    package: String,
    function: String,
}

impl ExternKey {
    pub fn try_new(
        package: impl Into<String>,
        function: impl Into<String>,
    ) -> Result<Self, ExternKeyError> {
        let key = Self {
            package: package.into(),
            function: function.into(),
        };
        // Validate the complete boundary contract, including its encoded size.
        key.as_ref().encode()?;
        Ok(key)
    }

    pub fn decode(encoded: &str) -> Result<Self, ExternKeyError> {
        decode_extern_name(encoded)?.to_owned()
    }

    pub fn as_ref(&self) -> ExternKeyRef<'_> {
        ExternKeyRef::new(&self.package, &self.function)
    }

    pub fn package(&self) -> &str {
        &self.package
    }

    pub fn function(&self) -> &str {
        &self.function
    }

    pub fn encode(&self) -> Result<String, ExternKeyError> {
        self.as_ref().encode()
    }

    pub fn into_parts(self) -> (String, String) {
        (self.package, self.function)
    }
}

/// Strictly decode one canonical extern name while borrowing its two fields.
pub fn decode_extern_name(encoded: &str) -> Result<ExternKeyRef<'_>, ExternKeyError> {
    if encoded.len() > MAX_EXTERN_NAME_BYTES {
        return Err(ExternKeyError::EncodedNameTooLong {
            len: encoded.len(),
            max: MAX_EXTERN_NAME_BYTES,
        });
    }
    if !encoded.starts_with(EXTERN_NAME_PREFIX) {
        return Err(ExternKeyError::InvalidPrefix);
    }
    let mut cursor = EXTERN_NAME_PREFIX.len();

    let package_len = parse_length(encoded, &mut cursor, ExternKeyField::Package)?;
    let package_end = cursor
        .checked_add(package_len)
        .ok_or(ExternKeyError::LengthOverflow(ExternKeyField::Package))?;
    if package_end >= encoded.len() {
        return Err(ExternKeyError::TruncatedField(ExternKeyField::Package));
    }
    let package = encoded
        .get(cursor..package_end)
        .ok_or(ExternKeyError::LengthSplitsUtf8(ExternKeyField::Package))?;
    if encoded.as_bytes()[package_end] != b':' {
        return Err(ExternKeyError::MissingSeparatorAfterPackage);
    }
    cursor = package_end + 1;

    let function_len = parse_length(encoded, &mut cursor, ExternKeyField::Function)?;
    let function_end = cursor
        .checked_add(function_len)
        .ok_or(ExternKeyError::LengthOverflow(ExternKeyField::Function))?;
    if function_end > encoded.len() {
        return Err(ExternKeyError::TruncatedField(ExternKeyField::Function));
    }
    if function_end < encoded.len() {
        return Err(ExternKeyError::TrailingBytes);
    }
    let function = encoded
        .get(cursor..function_end)
        .ok_or(ExternKeyError::LengthSplitsUtf8(ExternKeyField::Function))?;

    Ok(ExternKeyRef::new(package, function))
}

/// Derive the collision-free browser WASM export key from one canonical
/// encoded extern name.
///
/// The input is validated through [`decode_extern_name`] before any key is
/// returned.  Hosts and extension generators therefore share both the extern
/// codec and the final export namespace contract.
pub fn wasm_extension_export_key(encoded: &str) -> Result<String, ExternKeyError> {
    decode_extern_name(encoded)?;
    Ok(wasm_extension_export_key_from_canonical(encoded))
}

fn wasm_extension_export_key_from_canonical(encoded: &str) -> String {
    const LOWER_HEX: &[u8; 16] = b"0123456789abcdef";

    let mut export =
        String::with_capacity(WASM_EXTENSION_EXPORT_PREFIX.len() + encoded.len().saturating_mul(2));
    export.push_str(WASM_EXTENSION_EXPORT_PREFIX);
    for byte in encoded.bytes() {
        export.push(LOWER_HEX[(byte >> 4) as usize] as char);
        export.push(LOWER_HEX[(byte & 0x0f) as usize] as char);
    }
    export
}

fn validate_non_empty(value: &str, field: ExternKeyField) -> Result<(), ExternKeyError> {
    if value.is_empty() {
        Err(ExternKeyError::EmptyField(field))
    } else {
        Ok(())
    }
}

fn encoded_len(package_len: usize, function_len: usize) -> Result<usize, ExternKeyError> {
    EXTERN_NAME_PREFIX
        .len()
        .checked_add(decimal_digits(package_len))
        .and_then(|len| len.checked_add(1))
        .and_then(|len| len.checked_add(package_len))
        .and_then(|len| len.checked_add(1))
        .and_then(|len| len.checked_add(decimal_digits(function_len)))
        .and_then(|len| len.checked_add(1))
        .and_then(|len| len.checked_add(function_len))
        .ok_or(ExternKeyError::LengthOverflow(ExternKeyField::Function))
}

fn decimal_digits(mut value: usize) -> usize {
    let mut digits = 1;
    while value >= 10 {
        value /= 10;
        digits += 1;
    }
    digits
}

fn parse_length(
    encoded: &str,
    cursor: &mut usize,
    field: ExternKeyField,
) -> Result<usize, ExternKeyError> {
    let start = *cursor;
    let Some(relative_end) = encoded.as_bytes()[start..]
        .iter()
        .position(|byte| *byte == b':')
    else {
        return Err(ExternKeyError::MissingLength(field));
    };
    let end = start + relative_end;
    let digits = &encoded.as_bytes()[start..end];
    if digits.is_empty() {
        return Err(ExternKeyError::MissingLength(field));
    }
    if digits.len() > 1 && digits[0] == b'0' {
        return Err(ExternKeyError::NonCanonicalLength(field));
    }
    if digits.iter().any(|byte| !byte.is_ascii_digit()) {
        return Err(ExternKeyError::InvalidLengthDigit(field));
    }

    let mut value = 0usize;
    for byte in digits {
        value = value
            .checked_mul(10)
            .and_then(|value| value.checked_add((byte - b'0') as usize))
            .ok_or(ExternKeyError::LengthOverflow(field))?;
    }
    if value == 0 {
        return Err(ExternKeyError::NonCanonicalLength(field));
    }
    *cursor = end + 1;
    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extern_key_round_trips_exact_utf8_fields() {
        let key = ExternKeyRef::new("github.com/acme/图形:core", "Δelta_方法");
        let encoded = key.encode().unwrap();
        assert_eq!(encoded, "vo1:27:github.com/acme/图形:core:13:Δelta_方法");
        assert_eq!(decode_extern_name(&encoded).unwrap(), key);
        assert_eq!(ExternKey::decode(&encoded).unwrap().as_ref(), key);
    }

    #[test]
    fn extern_key_decoder_preserves_field_leading_bom() {
        let package_bom = "vo1:27:\u{feff}github.com/acme/graphics:4:Draw";
        let package_key = decode_extern_name(package_bom).unwrap();
        assert_eq!(package_key.package(), "\u{feff}github.com/acme/graphics");
        assert_eq!(package_key.function(), "Draw");
        assert!(!package_key.is_owned_by_module("github.com/acme/graphics"));

        let function_bom = "vo1:24:github.com/acme/graphics:7:\u{feff}Draw";
        let function_key = decode_extern_name(function_bom).unwrap();
        assert_eq!(function_key.package(), "github.com/acme/graphics");
        assert_eq!(function_key.function(), "\u{feff}Draw");
        assert!(function_key.is_owned_by_module("github.com/acme/graphics"));
    }

    #[test]
    fn canonical_package_paths_match_frontend_identity_classes() {
        for path in [
            "math",
            "encoding/json",
            "github.com/acme/graphics",
            "github.com/acme/graphics/图形/é",
            "local/scratch-1",
            "local/demo_name.v1",
        ] {
            validate_canonical_package_path(path)
                .unwrap_or_else(|error| panic!("valid package {path:?} was rejected: {error}"));
        }

        for path in [
            "",
            "std",
            "std/math",
            "local",
            "local/Upper",
            "local/name/child",
            "local/trailing.",
            "local/con",
            "local/com1.txt",
            "example.com/acme/graphics",
            "github.com/acme",
            "github.com/Acme/graphics",
            "github.com/acme/v1",
            "github.com/acme/graphics/pkg@v2",
            "github.com/acme/graphics/e\u{301}",
            "github.com/acme/graphics/COM¹.txt",
            "github.com/acme/graphics/\u{00a0}render",
            "github.com/acme/graphics/render\u{0085}",
        ] {
            assert!(
                validate_canonical_package_path(path).is_err(),
                "noncanonical package was accepted: {path:?}"
            );
        }
        assert!(validate_canonical_package_path(&format!("local/{}", "a".repeat(255))).is_ok());
        assert!(validate_canonical_package_path(&format!("local/{}", "a".repeat(256))).is_err());
    }

    #[test]
    fn extern_semantics_reject_wire_valid_source_impossibilities() {
        for function in ["Draw", "draw２", "绘制"] {
            let key = ExternKeyRef::new("github.com/acme/graphics", function);
            validate_canonical_extern_identity(key)
                .unwrap_or_else(|error| panic!("valid extern {key:?} was rejected: {error}"));
            assert_eq!(
                classify_extern_name(&key.encode().unwrap()),
                Ok(ExternNameClass::Canonical(key))
            );
        }

        for (package, function) in [
            ("example.com/acme/graphics", "Draw"),
            ("github.com/acme/graphics/e\u{301}", "Draw"),
            ("github.com/acme/graphics", "_"),
            ("github.com/acme/graphics", "func"),
            ("github.com/acme/graphics", "2Draw"),
            ("github.com/acme/graphics", "Draw-Now"),
            ("github.com/acme/graphics", "\u{feff}Draw"),
            ("github.com/acme/graphics", "\u{1e6c0}"),
        ] {
            let encoded = ExternKeyRef::new(package, function)
                .encode()
                .expect("wire codec deliberately accepts opaque non-empty fields");
            assert!(decode_extern_name(&encoded).is_ok());
            assert!(matches!(
                classify_extern_name(&encoded),
                Err(ExternNameError::InvalidCanonicalIdentity(_))
            ));
        }
    }

    #[test]
    fn wasm_extension_export_key_has_stable_ascii_and_unicode_vectors() {
        let ascii = ExternKeyRef::new("github.com/acme/graphics", "Draw")
            .encode()
            .unwrap();
        assert_eq!(ascii, "vo1:24:github.com/acme/graphics:4:Draw");
        assert_eq!(
            wasm_extension_export_key(&ascii).unwrap(),
            "__vo_ext_766f313a32343a6769746875622e636f6d2f61636d652f67726170686963733a343a44726177"
        );

        let unicode = ExternKeyRef::new("github.com/acme/图形", "绘制")
            .encode()
            .unwrap();
        assert_eq!(unicode, "vo1:22:github.com/acme/图形:6:绘制");
        assert_eq!(
            wasm_extension_export_key(&unicode).unwrap(),
            "__vo_ext_766f313a32323a6769746875622e636f6d2f61636d652fe59bbee5bda23a363ae7bb98e588b6"
        );
    }

    #[test]
    fn wasm_extension_export_key_keeps_root_and_child_same_name_distinct() {
        let root = ExternKeyRef::new("github.com/acme/graphics", "Draw")
            .wasm_extension_export_key()
            .unwrap();
        let child = ExternKeyRef::new("github.com/acme/graphics/render", "Draw")
            .wasm_extension_export_key()
            .unwrap();

        assert_ne!(root, child);
        assert!(root.starts_with(WASM_EXTENSION_EXPORT_PREFIX));
        assert!(child.starts_with(WASM_EXTENSION_EXPORT_PREFIX));
        assert!(root[WASM_EXTENSION_EXPORT_PREFIX.len()..]
            .bytes()
            .all(|byte| byte.is_ascii_digit() || matches!(byte, b'a'..=b'f')));
        assert!(child[WASM_EXTENSION_EXPORT_PREFIX.len()..]
            .bytes()
            .all(|byte| byte.is_ascii_digit() || matches!(byte, b'a'..=b'f')));
    }

    #[test]
    fn wasm_extension_export_key_rejects_noncanonical_input() {
        for malformed in [
            "Draw",
            "vo1:024:github.com/acme/graphics:4:Draw",
            "vo1:24:github.com/acme/graphics:4:Drawx",
        ] {
            assert!(
                wasm_extension_export_key(malformed).is_err(),
                "noncanonical extern acquired an export key: {malformed:?}"
            );
        }
    }

    #[test]
    fn extern_key_encoding_separates_every_legacy_collision_boundary() {
        let pairs = [
            (("x/a/b", "F"), ("x/a_b", "F")),
            (("x/a-b", "F"), ("x/a_b", "F")),
            (("x/a.b", "F"), ("x/a_b", "F")),
            (("x/a", "b_C"), ("x/a_b", "C")),
        ];
        for ((left_package, left_function), (right_package, right_function)) in pairs {
            let left = ExternKeyRef::new(left_package, left_function)
                .encode()
                .unwrap();
            let right = ExternKeyRef::new(right_package, right_function)
                .encode()
                .unwrap();
            assert_ne!(left, right);
            assert_eq!(
                decode_extern_name(&left).unwrap(),
                ExternKeyRef::new(left_package, left_function)
            );
            assert_eq!(
                decode_extern_name(&right).unwrap(),
                ExternKeyRef::new(right_package, right_function)
            );
        }
    }

    #[test]
    fn extern_key_decoder_rejects_every_noncanonical_shape() {
        let cases = [
            ("x_foo", ExternKeyError::InvalidPrefix),
            (
                "vo1::x:1:F",
                ExternKeyError::MissingLength(ExternKeyField::Package),
            ),
            (
                "vo1:01:x:1:F",
                ExternKeyError::NonCanonicalLength(ExternKeyField::Package),
            ),
            (
                "vo1:a:x:1:F",
                ExternKeyError::InvalidLengthDigit(ExternKeyField::Package),
            ),
            ("vo1:2:x:1:F", ExternKeyError::MissingSeparatorAfterPackage),
            (
                "vo1:1:x:2:F",
                ExternKeyError::TruncatedField(ExternKeyField::Function),
            ),
            ("vo1:1:x:1:Fx", ExternKeyError::TrailingBytes),
            (
                "vo1:1:é:1:F",
                ExternKeyError::LengthSplitsUtf8(ExternKeyField::Package),
            ),
        ];
        for (encoded, expected) in cases {
            assert_eq!(decode_extern_name(encoded), Err(expected), "{encoded}");
        }
    }

    #[test]
    fn extern_key_codec_enforces_the_shared_wire_limit() {
        let oversized_component = "x".repeat(MAX_EXTERN_NAME_BYTES);
        assert!(matches!(
            ExternKeyRef::new(&oversized_component, "F").encode(),
            Err(ExternKeyError::EncodedNameTooLong {
                max: MAX_EXTERN_NAME_BYTES,
                ..
            })
        ));
        let oversized_wire = "x".repeat(MAX_EXTERN_NAME_BYTES + 1);
        assert!(matches!(
            decode_extern_name(&oversized_wire),
            Err(ExternKeyError::EncodedNameTooLong {
                max: MAX_EXTERN_NAME_BYTES,
                ..
            })
        ));
    }

    #[test]
    fn extern_name_classification_has_one_explicit_vm_exception_set() {
        let canonical = ExternKeyRef::new("github.com/acme/demo", "Run")
            .encode()
            .unwrap();
        assert_eq!(
            classify_extern_name(&canonical),
            Ok(ExternNameClass::Canonical(ExternKeyRef::new(
                "github.com/acme/demo",
                "Run"
            )))
        );
        for name in VM_INTERNAL_EXTERN_NAMES {
            assert_eq!(
                classify_extern_name(name),
                Ok(ExternNameClass::VmInternal),
                "{name}"
            );
        }
        assert_eq!(
            classify_extern_name("github.com_acme_demo_Run"),
            Err(ExternNameError::UnrecognizedNonCanonical)
        );
        assert!(matches!(
            classify_extern_name("vo1:01:x:1:F"),
            Err(ExternNameError::MalformedCanonical(
                ExternKeyError::NonCanonicalLength(ExternKeyField::Package)
            ))
        ));
    }

    #[test]
    fn vm_internal_extern_whitelist_is_sorted_and_unique() {
        for pair in VM_INTERNAL_EXTERN_NAMES.windows(2) {
            assert!(
                pair[0] < pair[1],
                "VM extern whitelist order/duplicate: {pair:?}"
            );
        }
        for pair in VM_VARIABLE_SHAPE_EXTERN_NAMES.windows(2) {
            assert!(
                pair[0] < pair[1],
                "VM variable-shape extern order/duplicate: {pair:?}"
            );
        }
        for name in VM_VARIABLE_SHAPE_EXTERN_NAMES {
            assert!(
                is_vm_internal_extern_name(name),
                "variable-shape extern must also be VM-internal: {name}"
            );
        }
    }

    #[test]
    fn module_ownership_uses_import_path_boundaries() {
        let root = ExternKeyRef::new("github.com/acme/graphics", "Root");
        let child = ExternKeyRef::new("github.com/acme/graphics/render/v2", "Draw");
        let sibling = ExternKeyRef::new("github.com/acme/graphics_extra", "Draw");
        let prefix_collision = ExternKeyRef::new("github.com/acme/graphic", "Draw");

        assert!(root.is_owned_by_module("github.com/acme/graphics"));
        assert!(child.is_owned_by_module("github.com/acme/graphics"));
        assert!(!sibling.is_owned_by_module("github.com/acme/graphics"));
        assert!(!prefix_collision.is_owned_by_module("github.com/acme/graphics"));
    }

    #[test]
    fn deepest_module_owner_selection_is_order_independent() {
        let key = ExternKeyRef::new("github.com/acme/mono/graphics/render", "Draw");
        let parent = "github.com/acme/mono";
        let child = "github.com/acme/mono/graphics";
        let unrelated = "github.com/acme/other";

        let forward = BTreeSet::from([parent, child, unrelated]);
        let reverse = BTreeSet::from([unrelated, child, parent]);
        assert_eq!(deepest_owning_module(key, &forward), Some(child));
        assert_eq!(deepest_owning_module(key, &reverse), Some(child));
        assert_eq!(
            deepest_owning_module(key, &BTreeSet::from([parent])),
            Some(parent)
        );
        assert_eq!(
            deepest_owning_module(key, &BTreeSet::from([unrelated])),
            None
        );
        assert_eq!(deepest_owning_module(key, &BTreeSet::<&str>::new()), None);

        let owners_with_metadata = BTreeMap::from([(parent, 1_u64), (child, 2_u64)]);
        assert_eq!(
            deepest_owning_module(key, &owners_with_metadata),
            Some(child)
        );
    }

    #[test]
    fn deepest_module_owner_lookup_scales_through_a_deduplicated_index() {
        let mut owners = (0..8_192)
            .map(|index| format!("github.com/acme/module{index}"))
            .collect::<BTreeSet<_>>();
        owners.insert("github.com/acme/mono".to_string());
        owners.insert("github.com/acme/mono/graphics".to_string());
        let key = ExternKeyRef::new("github.com/acme/mono/graphics/render/v2", "Draw");

        assert_eq!(
            deepest_owning_module(key, &owners),
            Some("github.com/acme/mono/graphics")
        );
    }

    #[test]
    fn module_ownership_rejects_noncanonical_descendant_paths() {
        const OWNER: &str = "github.com/acme/graphics";
        for package in [
            "github.com/acme/graphics/../evil",
            "github.com/acme/graphics/./evil",
            "github.com/acme/graphics//evil",
            "github.com/acme/graphics/evil/",
            "github.com/acme/graphics/evil\\child",
            "github.com/acme/graphics/evil\0child",
            "github.com/acme/graphics/com1",
            "github.com/acme/graphics/COM¹.txt",
            "github.com/acme/graphics/pkg@v2",
            "github.com/acme/graphics/trailing.",
            "github.com/acme/graphics/ leading",
            "github.com/acme/graphics/a:b",
            "github.com/acme/graphics/e\u{301}",
        ] {
            assert!(
                !ExternKeyRef::new(package, "Run").is_owned_by_module(OWNER),
                "noncanonical package acquired ownership: {package:?}"
            );
        }

        let oversized_segment = format!("{OWNER}/{}", "x".repeat(256));
        assert!(!ExternKeyRef::new(&oversized_segment, "Run").is_owned_by_module(OWNER));
        let oversized_path = format!("{OWNER}/{}", "a/".repeat(MAX_EXTERN_NAME_BYTES));
        assert!(!ExternKeyRef::new(&oversized_path, "Run").is_owned_by_module(OWNER));

        for package in [
            "github.com/acme/graphics/图形/é",
            "github.com/acme/graphics/Render/V2",
            "github.com/acme/graphics/数据.json",
        ] {
            assert!(
                ExternKeyRef::new(package, "Run").is_owned_by_module(OWNER),
                "portable Unicode/case package lost ownership: {package:?}"
            );
        }

        assert!(is_portable_package_component("é"));
        assert!(!is_portable_package_component("e\u{301}"));
    }

    #[test]
    fn invalid_module_owners_match_nothing() {
        let key = ExternKeyRef::new("github.com/acme/graphics", "Draw");

        for owner in ["", "/github.com/acme/graphics", "github.com/acme/graphics/"] {
            assert!(!key.is_owned_by_module(owner), "invalid owner {owner:?}");
        }
    }

    #[test]
    fn canonical_module_owner_matches_published_module_path_contract() {
        for owner in [
            "github.com/acme/graphics",
            "github.com/acme/graphics/render-core",
            "github.com/acme/graphics/v2",
            "github.com/acme/v0",
            "github.com/acme/v1",
            "github.com/acme/v2",
            "github.com/acme/v02",
            "github.com/a1/r_2.x",
            "github.com/acme/foo..bar",
            "github.com/acme/foo.lock",
        ] {
            validate_canonical_module_owner(owner).unwrap_or_else(|error| {
                panic!("valid canonical module owner {owner:?} was rejected: {error}")
            });
        }

        let oversized = format!("github.com/acme/{}", "x".repeat(240));
        for owner in [
            "",
            "local/demo",
            "example.com/acme/demo",
            "github.com/acme",
            "github.com//demo",
            "github.com/Acme/demo",
            "github.com/acme/demo/",
            "github.com/acme/demo.",
            "github.com/con/demo",
            "github.com/acme/com1.txt",
            "github.com/acme/demo/v0",
            "github.com/acme/demo/v1",
            "github.com/acme/demo/v02",
            "github.com/acme/demo/v18446744073709551616",
            "github.com/acme/demo/foo..bar",
            "github.com/acme/demo/foo.lock",
            oversized.as_str(),
        ] {
            assert!(
                validate_canonical_module_owner(owner).is_err(),
                "non-canonical module owner {owner:?} was accepted"
            );
        }
    }
}
