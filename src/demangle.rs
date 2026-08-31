use crate::error::Result;
use eyre::eyre;
use std::fmt;

// ── AST types ───────────────────────────────────────────────────────────────

/// A fully demangled v0 symbol name, retaining the individual pieces.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolName {
    pub path: Path,
    pub instantiating_crate: Option<Path>,
    pub vendor_suffix: Option<String>,
}

/// A named or structural path component.
///
/// The `Path` enum distinguishes, among other things, free-standing items
/// (crates / nested paths), inherent impls (`M`), trait impls (`X`),
/// trait definitions (`Y`) and generic instantiations (`I`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Path {
    /// A crate root: `C <identifier>`.
    Crate(Identifier),
    /// An inherent impl root: `M <impl-path> <type>` → `<T> ::...`.
    Inherent {
        /// The impl's own (usually hidden) path, with an optional disambiguator.
        impl_path: InherentImplPath,
        /// The Self type.
        self_type: Box<Type>,
    },
    /// A trait impl root: `X <impl-path> <type> <path>` → `<T as Trait> ::...`.
    Trait {
        impl_path: InherentImplPath,
        self_type: Box<Type>,
        trait_path: Box<Path>,
    },
    /// A trait-definition root (for items inside default methods): `Y <type> <path>`.
    TraitDef {
        self_type: Box<Type>,
        trait_path: Box<Path>,
    },
    /// A nested path component: `N <namespace> <path> <identifier>`.
    Nested {
        namespace: Namespace,
        parent: Box<Path>,
        name: Identifier,
    },
    /// Generic arguments: `I <path> {<generic-arg>} E`.
    ///
    /// `in_value` records whether this path appears in value position (a
    /// function/static) vs type position, which changes how the generics are
    /// printed (`foo::<T>` vs `Foo<T>`).
    Generic {
        base: Box<Path>,
        args: Vec<GenericArg>,
        in_value: bool,
    },
    /// A back-reference to another byte position: `B <base-62-number>`.
    /// The referenced node could not be inlined (e.g. would recurse infinitely).
    Backref(usize),
}

/// The parent path of an impl, augmented with an optional disambiguator
/// (`[<disambiguator>] <path>`). Demanglers usually omit this.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InherentImplPath {
    pub disambiguator: Option<Disambiguator>,
    pub path: Option<Box<Path>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GenericArg {
    Lifetime(Lifetime),
    Type(Type),
    Const(Const),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Disambiguator(pub u64);

/// A namespace tag in a `<path>` component.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Namespace {
    /// Closures (`C`).
    Closure,
    /// Shims (`S`).
    Shim,
    /// Other special namespaces (`A`-`Z`).
    Special(char),
    /// Implementation-specific/unspecified namespaces (`a`-`z`).
    Internal(char),
}

impl Namespace {
    pub fn is_internal(&self) -> bool {
        matches!(self, Namespace::Internal(_))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Identifier {
    pub name: String,
    pub punycode: bool,
    pub disambiguator: Option<Disambiguator>,
}

impl Identifier {
    pub fn new(name: impl Into<String>, punycode: bool) -> Self {
        Self {
            name: name.into(),
            punycode,
            disambiguator: None,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.name.is_empty()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Lifetime(pub u64);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Basic(BasicType),
    Path(Box<Path>),
    Array(Box<Type>, Const),
    Slice(Box<Type>),
    Tuple(Vec<Type>),
    Ref(Option<Lifetime>, Box<Type>),
    RefMut(Option<Lifetime>, Box<Type>),
    PtrConst(Box<Type>),
    PtrMut(Box<Type>),
    Fn(FnSig),
    Dyn(DynBounds),
    Pat(Pat),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BasicType {
    Bool,
    Char,
    Str,
    Unit,
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    F32,
    F64,
    Never,
    Placeholder,
    VarArgs,
}

impl BasicType {
    fn from_tag(tag: u8) -> Option<Self> {
        Some(match tag {
            b'b' => BasicType::Bool,
            b'c' => BasicType::Char,
            b'e' => BasicType::Str,
            b'u' => BasicType::Unit,
            b'a' => BasicType::I8,
            b's' => BasicType::I16,
            b'l' => BasicType::I32,
            b'x' => BasicType::I64,
            b'n' => BasicType::I128,
            b'i' => BasicType::Isize,
            b'h' => BasicType::U8,
            b't' => BasicType::U16,
            b'm' => BasicType::U32,
            b'y' => BasicType::U64,
            b'o' => BasicType::U128,
            b'j' => BasicType::Usize,
            b'f' => BasicType::F32,
            b'd' => BasicType::F64,
            b'z' => BasicType::Never,
            b'p' => BasicType::Placeholder,
            b'v' => BasicType::VarArgs,
            _ => return None,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FnSig {
    pub binder: Option<u64>,
    pub unsafe_: bool,
    pub abi: Option<String>,
    pub params: Vec<Type>,
    pub ret: Option<Box<Type>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DynBounds {
    pub binder: Option<u64>,
    pub traits: Vec<DynTrait>,
    pub lifetime: Option<Lifetime>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DynTrait {
    pub path: Path,
    /// Existential projections (`Assoc = Ty`).
    pub assoc: Vec<(Identifier, DynValue)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DynValue {
    Type(Type),
    Const(Const),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pat {
    pub ty: Box<Type>,
    pub ranges: Vec<(Const, Const)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Const {
    Placeholder,
    Bool(bool),
    Char(char),
    StrLiteral(String),
    Int {
        ty: BasicType,
        negative: bool,
        value: u128,
        /// `None` iff the value fit in a u64.
        fits_u64: bool,
        /// Raw hex (used when it does not fit in u64).
        raw_hex: String,
    },
    Uint {
        ty: BasicType,
        value: u128,
    },
    RefStrLiteral(String),
    Ref(Box<Const>),
    RefMut(Box<Const>),
    Array(Vec<Const>),
    Tuple(Vec<Const>),
    Adt {
        path: Path,
        variant: ConstVariant,
    },
    Backref(usize),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConstVariant {
    Unit,
    Tuple(Vec<Const>),
    Struct(Vec<(Identifier, Const)>),
}

// ── Parser ──────────────────────────────────────────────────────────────────

const MAX_DEPTH: u32 = 200;

struct Parser<'a> {
    sym: &'a [u8],
    next: usize,
    depth: u32,
}

impl<'a> Parser<'a> {
    fn new(sym: &'a [u8]) -> Self {
        Self {
            sym,
            next: 0,
            depth: 0,
        }
    }

    fn push_depth(&mut self) -> Result<()> {
        self.depth += 1;
        if self.depth > MAX_DEPTH {
            Err(eyre!("recursion limit reached at pos {}", self.next))
        } else {
            Ok(())
        }
    }

    fn pop_depth(&mut self) {
        self.depth -= 1;
    }

    fn peek(&self) -> Option<u8> {
        self.sym.get(self.next).copied()
    }

    fn eat(&mut self, b: u8) -> bool {
        if self.peek() == Some(b) {
            self.next += 1;
            true
        } else {
            false
        }
    }

    fn next(&mut self) -> Result<u8> {
        let b = self
            .peek()
            .ok_or_else(|| eyre!("unexpected end of input at pos {}", self.next))?;
        self.next += 1;
        Ok(b)
    }

    // ── number/identifier parsing ────────────────────────────────────────

    fn hex_nibbles(&mut self) -> Result<String> {
        let start = self.next;
        loop {
            match self.next()? {
                b'0'..=b'9' | b'a'..=b'f' => {}
                b'_' => break,
                _ => return Err(eyre!("expected hex digit at pos {}", self.next - 1)),
            }
        }
        Ok(String::from_utf8_lossy(&self.sym[start..self.next - 1]).to_string())
    }

    fn digit_10(&mut self) -> Result<u8> {
        match self.peek() {
            Some(d @ b'0'..=b'9') => {
                self.next += 1;
                Ok(d - b'0')
            }
            _ => Err(eyre!("expected decimal digit at pos {}", self.next)),
        }
    }

    fn digit_62(&mut self) -> Result<u8> {
        match self.peek() {
            Some(d @ b'0'..=b'9') => {
                self.next += 1;
                Ok(d - b'0')
            }
            Some(d @ b'a'..=b'z') => {
                self.next += 1;
                Ok(10 + (d - b'a'))
            }
            Some(d @ b'A'..=b'Z') => {
                self.next += 1;
                Ok(10 + 26 + (d - b'A'))
            }
            _ => Err(eyre!("expected base-62 digit at pos {}", self.next)),
        }
    }

    /// `<base-62-number>`: digits (0-9a-zA-Z) then `_`. Empty digit run = 0,
    /// any other value is offset by 1.
    fn integer_62(&mut self) -> Result<u64> {
        if self.eat(b'_') {
            return Ok(0);
        }
        let mut x: u64 = 0;
        while !self.eat(b'_') {
            let d = self.digit_62()? as u64;
            x = x.checked_mul(62).ok_or_else(|| eyre!("base-62 overflow"))?;
            x = x.checked_add(d).ok_or_else(|| eyre!("base-62 overflow"))?;
        }
        x.checked_add(1).ok_or_else(|| eyre!("base-62 overflow"))
    }

    fn opt_integer_62(&mut self, tag: u8) -> Result<u64> {
        if !self.eat(tag) {
            return Ok(0);
        }
        self.integer_62()?
            .checked_add(1)
            .ok_or_else(|| eyre!("base-62 overflow"))
    }

    fn disambiguator(&mut self) -> Result<u64> {
        self.opt_integer_62(b's')
    }

    fn namespace(&mut self) -> Result<Namespace> {
        match self.next()? {
            ns @ b'A'..=b'Z' => Ok(if ns == b'C' {
                Namespace::Closure
            } else if ns == b'S' {
                Namespace::Shim
            } else {
                Namespace::Special(ns as char)
            }),
            ns @ b'a'..=b'z' => Ok(Namespace::Internal(ns as char)),
            _ => Err(eyre!("expected namespace tag at pos {}", self.next - 1)),
        }
    }

    fn ident(&mut self) -> Result<Identifier> {
        let dis = self.disambiguator()?;
        let is_punycode = self.eat(b'u');

        let mut len = self.digit_10()? as usize;
        if len != 0 {
            while let Ok(d) = self.digit_10() {
                len = len
                    .checked_mul(10)
                    .ok_or_else(|| eyre!("identifier length overflow"))?;
                len = len
                    .checked_add(d as usize)
                    .ok_or_else(|| eyre!("identifier length overflow"))?;
            }
        }

        // Skip the optional `_` separator.
        self.eat(b'_');

        let start = self.next;
        self.next = self
            .next
            .checked_add(len)
            .ok_or_else(|| eyre!("identifier length overflow"))?;
        if self.next > self.sym.len() {
            return Err(eyre!(
                "identifier length {} exceeds remaining input at pos {}",
                len,
                start
            ));
        }
        let ident = String::from_utf8_lossy(&self.sym[start..self.next]).to_string();

        Ok(Identifier {
            name: ident,
            punycode: is_punycode,
            disambiguator: if dis != 0 {
                Some(Disambiguator(dis))
            } else {
                None
            },
        })
    }

    // ── back references ──────────────────────────────────────────────────

    /// Parse a back-reference. `self.next` must already be positioned past the
    /// `B` tag. Returns a fresh parser positioned at the referenced byte offset.
    fn backref(&mut self) -> Result<Parser<'a>> {
        let i = self.integer_62()? as usize;
        if i >= self.sym.len() {
            return Err(eyre!("backref offset {} out of range", i));
        }
        let mut new_parser = Parser {
            sym: self.sym,
            next: i,
            depth: self.depth,
        };
        new_parser.push_depth()?;
        Ok(new_parser)
    }

    // ── paths ────────────────────────────────────────────────────────────

    fn parse_path(&mut self, in_value: bool) -> Result<Path> {
        self.push_depth()?;
        let result = self.parse_path_inner(in_value);
        self.pop_depth();
        result
    }

    fn parse_path_inner(&mut self, in_value: bool) -> Result<Path> {
        let tag = self.next()?;
        match tag {
            b'C' => {
                let dis = self.disambiguator()?;
                let name = self.ident()?;
                Ok(Path::Crate(Identifier {
                    name: name.name,
                    punycode: name.punycode,
                    disambiguator: if dis != 0 {
                        Some(Disambiguator(dis))
                    } else {
                        None
                    },
                }))
            }
            b'N' => {
                let ns = self.namespace()?;
                let parent = self.parse_path(in_value)?;
                let name = self.ident()?;
                Ok(Path::Nested {
                    namespace: ns,
                    parent: Box::new(parent),
                    name,
                })
            }
            b'M' => {
                let impl_dis = self.disambiguator()?;
                let impl_path = self.parse_path_skip_print()?.map(Box::new);
                let self_type = self.parse_type()?;
                Ok(Path::Inherent {
                    impl_path: InherentImplPath {
                        disambiguator: if impl_dis != 0 {
                            Some(Disambiguator(impl_dis))
                        } else {
                            None
                        },
                        path: impl_path,
                    },
                    self_type: Box::new(self_type),
                })
            }
            b'X' => {
                let impl_dis = self.disambiguator()?;
                let impl_path = self.parse_path_skip_print()?.map(Box::new);
                let self_type = self.parse_type()?;
                let trait_path = self.parse_path(false)?;
                Ok(Path::Trait {
                    impl_path: InherentImplPath {
                        disambiguator: if impl_dis != 0 {
                            Some(Disambiguator(impl_dis))
                        } else {
                            None
                        },
                        path: impl_path,
                    },
                    self_type: Box::new(self_type),
                    trait_path: Box::new(trait_path),
                })
            }
            b'Y' => {
                let self_type = self.parse_type()?;
                let trait_path = self.parse_path(false)?;
                Ok(Path::TraitDef {
                    self_type: Box::new(self_type),
                    trait_path: Box::new(trait_path),
                })
            }
            b'I' => {
                let base = self.parse_path(in_value)?;
                let args = self.parse_generic_arg_list()?;
                Ok(Path::Generic {
                    base: Box::new(base),
                    args,
                    in_value,
                })
            }
            b'B' => self.backref().and_then(|mut p| p.parse_path(in_value)),
            _ => Err(eyre!(
                "unknown path tag '{}' at pos {}",
                (tag as char).escape_default(),
                self.next - 1
            )),
        }
    }

    /// Parse a path such that the produced structure is correct, but used for
    /// the (usually hidden) impl-path which demanglers don't print. We return
    /// `None` when the path is a back-reference whose target would only recurse
    /// (kept as-is otherwise).
    fn parse_path_skip_print(&mut self) -> Result<Option<Path>> {
        Ok(Some(self.parse_path_backref_guard(false)?))
    }

    /// Parses a path, but resolves back-references that point *derived* from
    /// previously-consumed bytes. To avoid infinite recursion through
    /// self-referential backrefs, if the backref target position is `>=` the
    /// current position we store the backref as a leaf.
    fn parse_path_backref_guard(&mut self, in_value: bool) -> Result<Path> {
        self.push_depth()?;
        let result = (|| {
            if self.peek() == Some(b'B') {
                self.next += 1; // skip 'B'
                let target = self.integer_62()? as usize;
                if target >= self.next {
                    // Self- or forward-referential: keep as leaf.
                    return Ok(Path::Backref(target));
                }
                let mut p = Parser {
                    sym: self.sym,
                    next: target,
                    depth: self.depth,
                };
                p.push_depth()?;
                p.parse_path(in_value)
            } else {
                self.parse_path_inner(in_value)
            }
        })();
        self.pop_depth();
        result
    }

    fn parse_generic_arg_list(&mut self) -> Result<Vec<GenericArg>> {
        let mut args = Vec::new();
        loop {
            if self.eat(b'E') {
                return Ok(args);
            }
            if self.eat(b'L') {
                let lt = self.integer_62()?;
                args.push(GenericArg::Lifetime(Lifetime(lt)));
            } else if self.eat(b'K') {
                let c = self.parse_const()?;
                args.push(GenericArg::Const(c));
            } else {
                let ty = self.parse_type()?;
                args.push(GenericArg::Type(ty));
            }
        }
    }

    // ── types ────────────────────────────────────────────────────────────

    fn parse_type(&mut self) -> Result<Type> {
        let tag = self.next()?;

        if let Some(basic) = BasicType::from_tag(tag) {
            return Ok(Type::Basic(basic));
        }

        self.push_depth()?;
        let result = self.parse_type_inner(tag);
        self.pop_depth();
        result
    }

    fn parse_type_inner(&mut self, tag: u8) -> Result<Type> {
        match tag {
            b'R' | b'Q' => {
                let lt = if self.eat(b'L') {
                    let i = self.integer_62()?;
                    Some(Lifetime(i))
                } else {
                    None
                };
                let inner = self.parse_type()?;
                if tag == b'R' {
                    Ok(Type::Ref(lt, Box::new(inner)))
                } else {
                    Ok(Type::RefMut(lt, Box::new(inner)))
                }
            }
            b'P' | b'O' => {
                let inner = self.parse_type()?;
                if tag == b'P' {
                    Ok(Type::PtrConst(Box::new(inner)))
                } else {
                    Ok(Type::PtrMut(Box::new(inner)))
                }
            }
            b'A' | b'S' => {
                let elem = self.parse_type()?;
                if tag == b'A' {
                    let len = self.parse_const()?;
                    Ok(Type::Array(Box::new(elem), len))
                } else {
                    Ok(Type::Slice(Box::new(elem)))
                }
            }
            b'T' => {
                let mut types = Vec::new();
                while !self.eat(b'E') {
                    types.push(self.parse_type()?);
                }
                Ok(Type::Tuple(types))
            }
            b'F' => {
                let sig = self.parse_fn_sig()?;
                Ok(Type::Fn(sig))
            }
            b'D' => {
                let bounds = self.parse_dyn_bounds()?;
                Ok(Type::Dyn(bounds))
            }
            b'W' => {
                let ty = self.parse_type()?;
                let ranges = self.parse_pat()?;
                Ok(Type::Pat(Pat {
                    ty: Box::new(ty),
                    ranges,
                }))
            }
            b'B' => {
                let mut p = self.backref()?;
                p.parse_type()
            }
            _ => {
                // A path begins a named type. Re-consume the tag.
                self.next -= 1;
                let path = self.parse_path(false)?;
                Ok(Type::Path(Box::new(path)))
            }
        }
    }

    fn parse_fn_sig(&mut self) -> Result<FnSig> {
        let binder_bound = self.opt_integer_62(b'G')?;
        let is_unsafe = self.eat(b'U');
        let abi = if self.eat(b'K') {
            if self.eat(b'C') {
                Some("C".to_string())
            } else {
                let abi = self.ident()?;
                if abi.name.is_empty() || abi.punycode {
                    return Err(eyre!("invalid ABI"));
                }
                Some(abi.name)
            }
        } else {
            None
        };

        let mut params = Vec::new();
        while !self.eat(b'E') {
            params.push(self.parse_type()?);
        }

        let ret = if self.eat(b'u') {
            None // () return type is elided
        } else {
            Some(Box::new(self.parse_type()?))
        };

        Ok(FnSig {
            binder: (binder_bound > 0).then_some(binder_bound),
            unsafe_: is_unsafe,
            abi,
            params,
            ret,
        })
    }

    fn parse_dyn_bounds(&mut self) -> Result<DynBounds> {
        let binder_bound = self.opt_integer_62(b'G')?;
        let mut traits = Vec::new();
        while !self.eat(b'E') {
            traits.push(self.parse_dyn_trait()?);
        }
        let lifetime = if self.eat(b'L') {
            let i = self.integer_62()?;
            Some(Lifetime(i))
        } else {
            return Err(eyre!("dyn bounds missing lifetime"));
        };
        Ok(DynBounds {
            binder: (binder_bound > 0).then_some(binder_bound),
            traits,
            lifetime,
        })
    }

    fn parse_dyn_trait(&mut self) -> Result<DynTrait> {
        if self.eat(b'B') {
            let mut p = self.backref()?;
            return p.parse_dyn_trait();
        }
        if self.eat(b'I') {
            let base = self.parse_path(false)?;
            let args = self.parse_generic_arg_list()?;
            let path = Path::Generic {
                base: Box::new(base),
                args,
                in_value: false,
            };
            let mut assoc = Vec::new();
            while self.eat(b'p') {
                let name = self.ident()?;
                let value = if self.eat(b'K') {
                    DynValue::Const(self.parse_const()?)
                } else {
                    DynValue::Type(self.parse_type()?)
                };
                assoc.push((name, value));
            }
            Ok(DynTrait { path, assoc })
        } else {
            let path = self.parse_path(false)?;
            let mut assoc = Vec::new();
            while self.eat(b'p') {
                let name = self.ident()?;
                let value = if self.eat(b'K') {
                    DynValue::Const(self.parse_const()?)
                } else {
                    DynValue::Type(self.parse_type()?)
                };
                assoc.push((name, value));
            }
            Ok(DynTrait { path, assoc })
        }
    }

    fn parse_pat(&mut self) -> Result<Vec<(Const, Const)>> {
        let mut ranges = Vec::new();
        match self.next()? {
            b'R' => {
                let lo = self.parse_const()?;
                let hi = self.parse_const()?;
                ranges.push((lo, hi));
                Ok(ranges)
            }
            b'O' => {
                self.push_depth()?;
                while !self.eat(b'E') {
                    ranges.push(self.parse_pat_range()?);
                }
                self.pop_depth();
                Ok(ranges)
            }
            b'N' => Ok(ranges), // "!" / null pattern
            _ => Err(eyre!("unknown pattern tag")),
        }
    }

    fn parse_pat_range(&mut self) -> Result<(Const, Const)> {
        match self.next()? {
            b'R' => {
                let lo = self.parse_const()?;
                let hi = self.parse_const()?;
                Ok((lo, hi))
            }
            b'N' => Err(eyre!("null pattern in or-pattern not supported")),
            other => Err(eyre!("unknown pattern range tag '{}'", other as char)),
        }
    }

    // ── consts ───────────────────────────────────────────────────────────

    fn parse_const(&mut self) -> Result<Const> {
        let tag = self.next()?;

        // Guard against recursion depth issues from nested consts.
        self.push_depth()?;
        let result = (|| match tag {
            b'p' => Ok(Const::Placeholder),
            b'h' | b't' | b'm' | b'y' | b'o' | b'j' => {
                let hex = self.hex_nibbles()?;
                let ty = BasicType::from_tag(tag).unwrap();
                let (value, _raw) = parse_u128(&hex);
                Ok(Const::Uint { ty, value })
            }
            b'a' | b's' | b'l' | b'x' | b'n' | b'i' => {
                let negative = self.eat(b'n');
                let hex = self.hex_nibbles()?;
                let ty = BasicType::from_tag(tag).unwrap();
                let (value, raw) = parse_u128(&hex);
                Ok(Const::Int {
                    ty,
                    negative,
                    value,
                    fits_u64: value <= u64::MAX as u128 && hex.len() <= 16,
                    raw_hex: raw,
                })
            }
            b'b' => {
                let hex = self.hex_nibbles()?;
                match hex.as_str() {
                    "0" => Ok(Const::Bool(false)),
                    "1" => Ok(Const::Bool(true)),
                    _ => Err(eyre!("invalid bool const '{}'", hex)),
                }
            }
            b'c' => {
                let hex = self.hex_nibbles()?;
                let v = u32::from_str_radix(&hex, 16)
                    .map_err(|_| eyre!("invalid char const '{}'", hex))?;
                let c = char::from_u32(v).ok_or_else(|| eyre!("invalid char const"))?;
                Ok(Const::Char(c))
            }
            b'e' => {
                let hex = self.hex_nibbles()?;
                let s = hex_to_str(&hex).ok_or_else(|| eyre!("invalid str const"))?;
                Ok(Const::StrLiteral(s))
            }
            b'R' | b'Q' => {
                if tag == b'R' && self.eat(b'e') {
                    let hex = self.hex_nibbles()?;
                    let s = hex_to_str(&hex).ok_or_else(|| eyre!("invalid &str const"))?;
                    Ok(Const::RefStrLiteral(s))
                } else {
                    let inner = self.parse_const()?;
                    if tag == b'R' {
                        Ok(Const::Ref(Box::new(inner)))
                    } else {
                        Ok(Const::RefMut(Box::new(inner)))
                    }
                }
            }
            b'A' => {
                let mut elems = Vec::new();
                while !self.eat(b'E') {
                    elems.push(self.parse_const()?);
                }
                Ok(Const::Array(elems))
            }
            b'T' => {
                let mut elems = Vec::new();
                while !self.eat(b'E') {
                    elems.push(self.parse_const()?);
                }
                Ok(Const::Tuple(elems))
            }
            b'V' => {
                let path = self.parse_path(true)?;
                match self.next()? {
                    b'U' => Ok(Const::Adt {
                        path,
                        variant: ConstVariant::Unit,
                    }),
                    b'T' => {
                        let mut args = Vec::new();
                        while !self.eat(b'E') {
                            args.push(self.parse_const()?);
                        }
                        Ok(Const::Adt {
                            path,
                            variant: ConstVariant::Tuple(args),
                        })
                    }
                    b'S' => {
                        let mut fields = Vec::new();
                        while !self.eat(b'E') {
                            let _ = self.disambiguator()?;
                            let name = self.ident()?;
                            let value = self.parse_const()?;
                            fields.push((name, value));
                        }
                        Ok(Const::Adt {
                            path,
                            variant: ConstVariant::Struct(fields),
                        })
                    }
                    _ => Err(eyre!("unknown const ADT tag")),
                }
            }
            b'B' => {
                let mut p = self.backref()?;
                p.parse_const()
            }
            _ => Err(eyre!(
                "unknown const tag '{}' at pos {}",
                (tag as char).escape_default(),
                self.next - 1
            )),
        })();
        self.pop_depth();
        result
    }
}

/// Parse a hex string to its u128 value, also returning the raw hex.
fn parse_u128(hex: &str) -> (u128, String) {
    let value = u128::from_str_radix(hex, 16).unwrap_or(0);
    (value, hex.to_string())
}

/// Decode a UTF-8 byte sequence expressed as pairs of hex nibbles into a String.
fn hex_to_str(hex: &str) -> Option<String> {
    if hex.len() % 2 != 0 {
        return None;
    }
    let mut bytes = Vec::with_capacity(hex.len() / 2);
    for i in (0..hex.len()).step_by(2) {
        let b = u8::from_str_radix(&hex[i..i + 2], 16).ok()?;
        bytes.push(b);
    }
    String::from_utf8(bytes).ok()
}

// ── Entry point ────────────────────────────────────────────────────────────

pub fn demangle_v0(input: &str) -> Result<SymbolName> {
    let bytes = input.as_bytes();

    if !bytes.starts_with(b"_R") {
        return Err(eyre!("not a v0 mangled symbol: {}", input));
    }

    // Only ASCII is allowed.
    if bytes.iter().any(|&c| c & 0x80 != 0) {
        return Err(eyre!("non-ASCII v0 symbol: {}", input));
    }

    let inner = &bytes[2..];
    if !inner
        .first()
        .map(|b| b.is_ascii_uppercase())
        .unwrap_or(false)
    {
        return Err(eyre!("v0 symbol does not start with a path: {}", input));
    }

    let mut parser = Parser::new(inner);
    let path = parser.parse_path(true)?;

    // Instantiating crate: paths always start with an uppercase char.
    let instantiating_crate = if parser
        .peek()
        .map(|b| (b'A'..=b'Z').contains(&b) || b == b'B')
        .unwrap_or(false)
    {
        Some(parser.parse_path(true)?)
    } else {
        None
    };

    // Vendor-specific suffix: `.` or `$` followed by anything.
    let vendor_suffix = if parser
        .peek()
        .map(|b| b == b'.' || b == b'$')
        .unwrap_or(false)
    {
        let suffix = String::from_utf8_lossy(&inner[parser.next..]).to_string();
        parser.next = inner.len();
        Some(suffix)
    } else {
        None
    };

    if parser.next != inner.len() {
        return Err(eyre!(
            "unexpected trailing input at pos {}: {:?}",
            parser.next,
            &inner[parser.next..]
        ));
    }

    Ok(SymbolName {
        path,
        instantiating_crate,
        vendor_suffix,
    })
}

// ── Display ─────────────────────────────────────────────────────────────────

impl fmt::Display for SymbolName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.path)?;
        if self.instantiating_crate.is_some() {
            write!(f, " [in {}]", self.instantiating_crate.as_ref().unwrap())?;
        }
        if let Some(suffix) = &self.vendor_suffix {
            write!(f, "{}", suffix)?;
        }
        Ok(())
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Path::Crate(id) => write!(f, "{}", id),
            Path::Inherent { self_type, .. } => write!(f, "<{}>", self_type),
            Path::Trait {
                self_type,
                trait_path,
                ..
            } => write!(f, "<{} as {}>", self_type, trait_path),
            Path::TraitDef {
                self_type,
                trait_path,
            } => write!(f, "<{} as {}>", self_type, trait_path),
            Path::Nested {
                namespace,
                parent,
                name,
            } => {
                if let Namespace::Internal(_) = namespace {
                    write!(f, "{}::{}", parent, name)?;
                } else {
                    write!(f, "{}::{{", parent)?;
                    match namespace {
                        Namespace::Closure => write!(f, "closure")?,
                        Namespace::Shim => write!(f, "shim")?,
                        Namespace::Special(c) => write!(f, "{}", c)?,
                        Namespace::Internal(_) => unreachable!(),
                    }
                    if !name.name.is_empty() {
                        write!(f, ":{}", name)?;
                    }
                    write!(
                        f,
                        "#{}}}",
                        name.disambiguator.as_ref().map(|d| d.0).unwrap_or(0)
                    )?;
                }
                Ok(())
            }
            Path::Generic {
                base,
                args,
                in_value,
            } => {
                write!(f, "{}", base)?;
                if *in_value {
                    write!(f, "::")?;
                }
                write!(f, "<")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ">")
            }
            Path::Backref(off) => write!(f, "{{backref#{}}}", off),
        }
    }
}

impl fmt::Display for InherentImplPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(path) = &self.path {
            write!(f, "{}", path)
        } else {
            Ok(())
        }
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl fmt::Display for GenericArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenericArg::Lifetime(l) => write!(f, "{}", l),
            GenericArg::Type(t) => write!(f, "{}", t),
            GenericArg::Const(c) => write!(f, "{}", c),
        }
    }
}

impl fmt::Display for Lifetime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 == 0 {
            write!(f, "'_")
        } else {
            // Simple dh: print the index.
            write!(f, "'_{}", self.0)
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Basic(b) => write!(f, "{}", b),
            Type::Path(p) => write!(f, "{}", p),
            Type::Array(elem, len) => write!(f, "[{}; {}]", elem, len),
            Type::Slice(elem) => write!(f, "[{}]", elem),
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", ty)?;
                }
                if types.len() == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
            Type::Ref(lt, inner) => {
                write!(f, "&")?;
                if let Some(lt) = lt {
                    if lt.0 != 0 {
                        write!(f, "{} ", lt)?;
                    }
                }
                write!(f, "{}", inner)
            }
            Type::RefMut(lt, inner) => {
                write!(f, "&")?;
                if let Some(lt) = lt {
                    if lt.0 != 0 {
                        write!(f, "{} ", lt)?;
                    }
                }
                write!(f, "mut {}", inner)
            }
            Type::PtrConst(inner) => write!(f, "*const {}", inner),
            Type::PtrMut(inner) => write!(f, "*mut {}", inner),
            Type::Fn(sig) => write!(f, "{}", sig),
            Type::Dyn(bounds) => write!(f, "dyn {}", bounds),
            Type::Pat(p) => {
                write!(f, "{} is ", p.ty)?;
                for (i, (lo, hi)) in p.ranges.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}..={}", lo, hi)?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for BasicType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BasicType::Bool => "bool",
            BasicType::Char => "char",
            BasicType::Str => "str",
            BasicType::Unit => "()",
            BasicType::I8 => "i8",
            BasicType::I16 => "i16",
            BasicType::I32 => "i32",
            BasicType::I64 => "i64",
            BasicType::I128 => "i128",
            BasicType::Isize => "isize",
            BasicType::U8 => "u8",
            BasicType::U16 => "u16",
            BasicType::U32 => "u32",
            BasicType::U64 => "u64",
            BasicType::U128 => "u128",
            BasicType::Usize => "usize",
            BasicType::F32 => "f32",
            BasicType::F64 => "f64",
            BasicType::Never => "!",
            BasicType::Placeholder => "_",
            BasicType::VarArgs => "...",
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for FnSig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.unsafe_ {
            write!(f, "unsafe ")?;
        }
        if let Some(abi) = &self.abi {
            // `_` in the ABI was substituted for `-`.
            let abi = abi.replace('_', "-");
            write!(f, "extern \"{}\" ", abi)?;
        }
        write!(f, "fn(")?;
        for (i, p) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", p)?;
        }
        write!(f, ")")?;
        if let Some(ret) = &self.ret {
            write!(f, " -> {}", ret)?;
        }
        Ok(())
    }
}

impl fmt::Display for DynBounds {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, t) in self.traits.iter().enumerate() {
            if i > 0 {
                write!(f, " + ")?;
            }
            write!(f, "{}", t)?;
        }
        if let Some(lt) = &self.lifetime {
            if lt.0 != 0 {
                write!(f, " + {}", lt)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for DynTrait {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.path)?;
        if !self.assoc.is_empty() {
            write!(f, "<")?;
            let mut parts = Vec::new();
            for (name, value) in &self.assoc {
                parts.push(format!("{} = {}", name, value));
            }
            write!(f, "{}", parts.join(", "))?;
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl fmt::Display for DynValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DynValue::Type(t) => write!(f, "{}", t),
            DynValue::Const(c) => write!(f, "{}", c),
        }
    }
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Const::Placeholder => write!(f, "_"),
            Const::Bool(b) => write!(f, "{}", b),
            Const::Char(c) => write!(f, "'{}'", c.escape_default()),
            Const::StrLiteral(s) => write!(f, "{}", quote_str(s)),
            Const::RefStrLiteral(s) => write!(f, "{}", quote_str(s)),
            Const::Int {
                negative,
                value,
                fits_u64,
                raw_hex,
                ..
            } => {
                if *fits_u64 {
                    if *negative {
                        write!(f, "-{}", value)
                    } else {
                        write!(f, "{}", value)
                    }
                } else {
                    if *negative {
                        write!(f, "-")?;
                    }
                    write!(f, "0x{}", raw_hex)
                }
            }
            Const::Uint { value, .. } => {
                if *value <= u64::MAX as u128 {
                    write!(f, "{}", value)
                } else {
                    write!(f, "0x{:x}", value)
                }
            }
            Const::Ref(c) => write!(f, "&{}", c),
            Const::RefMut(c) => write!(f, "&mut {}", c),
            Const::Array(elems) => {
                write!(f, "[")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, "]")
            }
            Const::Tuple(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", e)?;
                }
                if elems.len() == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
            Const::Adt { path, variant } => {
                write!(f, "{}", path)?;
                match variant {
                    ConstVariant::Unit => Ok(()),
                    ConstVariant::Tuple(args) => {
                        write!(f, "(")?;
                        for (i, a) in args.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", a)?;
                        }
                        write!(f, ")")
                    }
                    ConstVariant::Struct(fields) => {
                        write!(f, " {{ ")?;
                        for (i, (name, value)) in fields.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}: {}", name, value)?;
                        }
                        write!(f, " }}")
                    }
                }
            }
            Const::Backref(off) => write!(f, "{{backref#{}}}", off),
        }
    }
}

fn quote_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        for esc in c.escape_debug() {
            out.push(esc);
        }
    }
    out.push('"');
    out
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_path(input: &str, expected: &str) {
        let sym = demangle_v0(input).unwrap();
        assert_eq!(
            sym.path.to_string(),
            expected,
            "failed demangling '{}'",
            input
        );
        assert!(
            sym.instantiating_crate.is_none(),
            "unexpected instantiating crate for '{}'",
            input
        );
    }

    fn assert_path_in_crate(input: &str, expected_path: &str, expected_crate: &str) {
        let sym = demangle_v0(input).unwrap();
        assert_eq!(sym.path.to_string(), expected_path, "for '{}'", input);
        assert_eq!(
            sym.instantiating_crate.as_ref().unwrap().to_string(),
            expected_crate,
            "for '{}'",
            input
        );
    }

    // ── Binary symbols: xmc1200 ─────────────────────────────────────────
    // Source: `cargo run --example testing -- ../xmc1200/target/thumbv6m-none-eabi/release/xmc1200`

    #[test]
    fn binary_simple_paths() {
        // All these `xmc1200` symbols demangle to the plain function path. The
        // `transmit::`/`enable_usic::` segments that show up in the DWARF labels
        // come from namespace tracking in the binary, not from the symbol itself.
        assert_path("_RNvCsgAPbTcwAsCu_7xmc12004blub", "xmc1200::blub");
        assert_path(
            "_RNvCsgAPbTcwAsCu_7xmc120011enable_usic",
            "xmc1200::enable_usic",
        );
        assert_path(
            "_RNvCsgAPbTcwAsCu_7xmc120018___cortex_m_rt_main",
            "xmc1200::__cortex_m_rt_main",
        );
        assert_path(
            "_RNvCsgAPbTcwAsCu_7xmc120019set_normal_baudrate",
            "xmc1200::set_normal_baudrate",
        );
        // The symbol for `transmit::wait` is just `xmc1200::wait`.
        assert_path("_RNvCsgAPbTcwAsCu_7xmc12004wait", "xmc1200::wait");
        // The symbol for `transmit::transmit` is just `xmc1200::transmit`.
        assert_path("_RNvCsgAPbTcwAsCu_7xmc12008transmit", "xmc1200::transmit");
    }

    #[test]
    fn binary_nested_paths() {
        // cortex_m::interrupt::enable
        assert_path(
            "_RNvNtCs5bpO7ggFLBd_8cortex_m9interrupt6enable",
            "cortex_m::interrupt::enable",
        );
        // cortex_m::interrupt::disable
        assert_path(
            "_RNvNtCs5bpO7ggFLBd_8cortex_m9interrupt7disable",
            "cortex_m::interrupt::disable",
        );
        // cortex_m::register::primask::read
        assert_path(
            "_RNvNtNtCs5bpO7ggFLBd_8cortex_m8register7primask4read",
            "cortex_m::register::primask::read",
        );
    }

    #[test]
    fn binary_generic_functions() {
        // core::ptr::read_volatile::<u32> [in xmc1200]
        assert_path_in_crate(
            "_RINvNtCs3Ql2Z2BQrSG_4core3ptr13read_volatilemECsgAPbTcwAsCu_7xmc1200",
            "core::ptr::read_volatile::<u32>",
            "xmc1200",
        );
        // core::ptr::write_volatile::<u32> [in xmc1200]
        assert_path_in_crate(
            "_RINvNtCs3Ql2Z2BQrSG_4core3ptr14write_volatilemECsgAPbTcwAsCu_7xmc1200",
            "core::ptr::write_volatile::<u32>",
            "xmc1200",
        );
    }

    #[test]
    fn binary_inherent_impl_methods() {
        // <*mut u32>::read_volatile [in xmc1200]
        // impl-path core::ptr::mut_ptr is hidden; self-type is *mut u32.
        assert_path_in_crate(
            "_RNvMNtNtCs3Ql2Z2BQrSG_4core3ptr7mut_ptrOm13read_volatileCsgAPbTcwAsCu_7xmc1200",
            "<*mut u32>::read_volatile",
            "xmc1200",
        );
        // <*mut u32>::write_volatile [in xmc1200]
        assert_path_in_crate(
            "_RNvMNtNtCs3Ql2Z2BQrSG_4core3ptr7mut_ptrOm14write_volatileCsgAPbTcwAsCu_7xmc1200",
            "<*mut u32>::write_volatile",
            "xmc1200",
        );
    }

    #[test]
    fn binary_trait_impl_with_backref() {
        // The `B2_` backref reuses the cortex_m self-type path defined earlier.
        assert_path(
            "_RNvXNtNtCs5bpO7ggFLBd_8cortex_m16critical_section28single_core_critical_sectionNtB2_25SingleCoreCriticalSectionNtCs4YIIJfm9zgo_16critical_section4Impl7acquire",
            "<cortex_m::critical_section::single_core_critical_section::SingleCoreCriticalSection as critical_section::Impl>::acquire",
        );
        // Same but for `release`; the backref and trait path are identical.
        assert_path(
            "_RNvXNtNtCs5bpO7ggFLBd_8cortex_m16critical_section28single_core_critical_sectionNtB2_25SingleCoreCriticalSectionNtCs4YIIJfm9zgo_16critical_section4Impl7release",
            "<cortex_m::critical_section::single_core_critical_section::SingleCoreCriticalSection as critical_section::Impl>::release",
        );
    }

    #[test]
    fn binary_trait_impl_with_disambiguator() {
        // <u32 as core::ops::bit::BitOrAssign>::bitor_assign
        assert_path(
            "_RNvXslZ_NtNtCs3Ql2Z2BQrSG_4core3ops3bitmNtB6_11BitOrAssign12bitor_assign",
            "<u32 as core::ops::bit::BitOrAssign>::bitor_assign",
        );
        // <u32 as core::ops::bit::BitAndAssign>::bitand_assign
        assert_path(
            "_RNvXslz_NtNtCs3Ql2Z2BQrSG_4core3ops3bitmNtB6_12BitAndAssign13bitand_assign",
            "<u32 as core::ops::bit::BitAndAssign>::bitand_assign",
        );
    }

    #[test]
    fn binary_closures() {
        // xmc1200::blub::{closure#0}
        let sym = demangle_v0("_RNCNvCsgAPbTcwAsCu_7xmc12004blub0B3_").unwrap();
        assert_eq!(sym.path.to_string(), "xmc1200::blub::{closure#0}");

        // xmc1200::blub::{closure#1}
        let sym = demangle_v0("_RNCNvCsgAPbTcwAsCu_7xmc12004blubs_0B3_").unwrap();
        assert_eq!(sym.path.to_string(), "xmc1200::blub::{closure#1}");

        // xmc1200::blub::{closure#2}
        let sym = demangle_v0("_RNCNvCsgAPbTcwAsCu_7xmc12004blubs0_0B3_").unwrap();
        assert_eq!(sym.path.to_string(), "xmc1200::blub::{closure#2}");

        // xmc1200::enable_usic::{closure#2}
        let sym = demangle_v0("_RNCNvCsgAPbTcwAsCu_7xmc120011enable_usics0_0B3_").unwrap();
        assert_eq!(sym.path.to_string(), "xmc1200::enable_usic::{closure#2}");
    }

    // ── RFC Appendix B examples ──────────────────────────────────────────

    #[test]
    fn rfc_free_standing_item() {
        assert_path(
            "_RNvNtNtCs1234_7mycrate3foo3bar3baz",
            "mycrate::foo::bar::baz",
        );
    }

    #[test]
    fn rfc_item_in_inherent_method() {
        // <mycrate::Foo<_>>::bar::QUUX
        assert_path(
            "_RNvNvMCs1234_7mycrateINtCs1234_7mycrate3FoopE3bar4QUUX",
            "<mycrate::Foo<_>>::bar::QUUX",
        );
    }

    #[test]
    fn rfc_item_in_trait_method() {
        // <mycrate::Foo<_> as std::clone::Clone>::clone::QUUX
        assert_path(
            "_RNvNvXCs1234_7mycrateINtCs1234_7mycrate3FoopENtNtC3std5clone5Clone5clone4QUUX",
            "<mycrate::Foo<_> as std::clone::Clone>::clone::QUUX",
        );
    }

    #[test]
    fn rfc_item_in_static_initializer() {
        assert_path("_RNvNvCs1234_7mycrate4QUUX3FOO", "mycrate::QUUX::FOO");
    }

    #[test]
    fn rfc_generic_align_of() {
        // std::mem::align_of::<f64>
        assert_path("_RINvNtC3std3mem8align_ofdE", "std::mem::align_of::<f64>");
        // std::mem::align_of::<usize>
        assert_path("_RINvNtC3std3mem8align_ofjE", "std::mem::align_of::<usize>");
        // std::mem::align_of::<&char>
        assert_path(
            "_RINvNtC3std3mem8align_ofRcE",
            "std::mem::align_of::<&char>",
        );
        // std::mem::align_of::<std::mem::Discriminant>
        assert_path(
            "_RINvNtC3std3mem8align_ofNtNtC3std3mem12DiscriminantE",
            "std::mem::align_of::<std::mem::Discriminant>",
        );
        // std::mem::align_of::<&mut (&str, ())>
        assert_path(
            "_RINvNtC3std3mem8align_ofQTReuEE",
            "std::mem::align_of::<&mut (&str, ())>",
        );
    }

    #[test]
    fn rfc_generic_with_instantiating_crate() {
        // std::mem::align_of::<usize> [in crate "foo"]
        assert_path_in_crate(
            "_RINvNtC3std3mem8align_ofjEC3foo",
            "std::mem::align_of::<usize>",
            "foo",
        );
    }

    #[test]
    fn rfc_progressive_compression() {
        // NOTE: This RFC Appendix B example predates the current compiler's
        // byte-offset back-reference scheme and no longer round-trips.
        // `cargo run --example testing -- <binary>` does not produce it.
        // The reference `rustc-demangle` also rejects it:
        //   `_RINxC3std3fooTNyB4_3BarBe_EBd_E`
        let sym = demangle_v0("_RINxC3std3fooTNyB4_3BarBe_EBd_E");
        assert!(
            sym.is_err(),
            "expected stale RFC compression example to fail with current compiler, got {sym:?}"
        );
    }

    #[test]
    fn rfc_compressed_prefix_stale() {
        // Same staleness as above; this RFC example uses the old substitution
        // table semantics and does not parse with the current compiler.
        assert!(demangle_v0("_RINvCs1234_7mycrate3fooNvB4_3barNvBn_3bazE").is_err());
    }

    #[test]
    fn backref_from_binary() {
        // The `B2_` in the following real compiler output refers to the byte
        // offset of the self-type path, and must be expanded correctly.
        let sym = demangle_v0("_RNvXNtNtCs5bpO7ggFLBd_8cortex_m16critical_section28single_core_critical_sectionNtB2_25SingleCoreCriticalSectionNtCs4YIIJfm9zgo_16critical_section4Impl7acquire").unwrap();
        assert_eq!(
            sym.path.to_string(),
            "<cortex_m::critical_section::single_core_critical_section::SingleCoreCriticalSection \
              as critical_section::Impl>::acquire"
        );
    }

    // ── Structural tests (AST piece retention) ───────────────────────────

    #[test]
    fn structure_free_standing() {
        let sym = demangle_v0("_RNvCsgAPbTcwAsCu_7xmc12004blub").unwrap();
        match &sym.path {
            Path::Nested {
                namespace,
                parent,
                name,
            } => {
                assert!(namespace.is_internal());
                assert_eq!(name.name, "blub");
                assert_eq!(parent.to_string(), "xmc1200");
            }
            other => panic!("expected Nested path, got {other:?}"),
        }
    }

    #[test]
    fn structure_generic() {
        let sym = demangle_v0("_RINvNtC3std3mem8align_ofdE").unwrap();
        match &sym.path {
            Path::Generic { base, args, .. } => {
                assert_eq!(base.to_string(), "std::mem::align_of");
                assert_eq!(args.len(), 1);
                match &args[0] {
                    GenericArg::Type(Type::Basic(BasicType::F64)) => {}
                    other => panic!("expected f64 arg, got {other:?}"),
                }
            }
            other => panic!("expected Generic path, got {other:?}"),
        }
    }

    #[test]
    fn structure_inherent_impl() {
        let sym = demangle_v0(
            "_RNvMNtNtCs3Ql2Z2BQrSG_4core3ptr7mut_ptrOm13read_volatileCsgAPbTcwAsCu_7xmc1200",
        )
        .unwrap();
        match &sym.path {
            Path::Nested { parent, name, .. } => {
                assert_eq!(name.name, "read_volatile");
                match parent.as_ref() {
                    Path::Inherent {
                        self_type,
                        impl_path,
                        ..
                    } => {
                        assert_eq!(self_type.to_string(), "*mut u32");
                        // The impl-path is retained but hidden from display.
                        assert!(impl_path.path.is_some());
                        assert_eq!(
                            impl_path.path.as_ref().unwrap().to_string(),
                            "core::ptr::mut_ptr"
                        );
                    }
                    other => panic!("expected Inherent parent, got {other:?}"),
                }
            }
            other => panic!("expected Nested path, got {other:?}"),
        }
    }

    #[test]
    fn structure_trait_impl() {
        let sym = demangle_v0("_RNvXNtNtCs5bpO7ggFLBd_8cortex_m16critical_section28single_core_critical_sectionNtB2_25SingleCoreCriticalSectionNtCs4YIIJfm9zgo_16critical_section4Impl7acquire").unwrap();
        match &sym.path {
            Path::Nested { parent, name, .. } => {
                assert_eq!(name.name, "acquire");
                match parent.as_ref() {
                    Path::Trait {
                        self_type,
                        trait_path,
                        ..
                    } => {
                        assert!(
                            self_type.to_string().contains("SingleCoreCriticalSection"),
                            "got self type {}",
                            self_type
                        );
                        assert!(trait_path.to_string().contains("Impl"));
                    }
                    other => panic!("expected Trait parent, got {other:?}"),
                }
            }
            other => panic!("expected Nested path, got {other:?}"),
        }
    }

    #[test]
    fn structure_closure() {
        let sym = demangle_v0("_RNCNvCsgAPbTcwAsCu_7xmc12004blubs0_0B3_").unwrap();
        match &sym.path {
            Path::Nested {
                namespace: Namespace::Closure,
                name,
                ..
            } => {
                assert_eq!(
                    name.disambiguator.as_ref().map(|d| d.0),
                    Some(2),
                    "closure index should be 2"
                );
            }
            other => panic!("expected closure Nested path, got {other:?}"),
        }
    }

    #[test]
    fn rfc_unicode_ident_parses() {
        // From the RFC: Unicode identifiers are Punycode-encoded with a `u` prefix.
        let sym = demangle_v0("_RNvNtNtC7mycrateu8gdel_5qa6escher4bach");
        // We retain the raw (punycode) identifier; the important thing is it parses.
        assert!(sym.is_ok());
    }

    // ── RFC / reference-derived example: vendor suffix & extra suffix ────

    #[test]
    fn vendor_suffix() {
        let sym = demangle_v0("_RC3foo.llvm.9D1C9369").unwrap();
        assert_eq!(sym.path.to_string(), "foo");
        assert_eq!(sym.vendor_suffix.as_deref(), Some(".llvm.9D1C9369"));
    }

    #[test]
    fn extra_suffix_dynamic_symbol() {
        // From rustc-demangle:
        let sym = demangle_v0("_RNvNtNtNtNtCs92dm3009vxr_4rand4rngs7adapter9reseeding4fork23FORK_HANDLER_REGISTERED.0.0").unwrap();
        assert_eq!(
            sym.path.to_string(),
            "rand::rngs::adapter::reseeding::fork::FORK_HANDLER_REGISTERED"
        );
    }

    // ── Every unique symbol from the binary must parse ───────────────────

    #[test]
    fn all_binary_symbols_parse() {
        // Every unique symbol extracted from the binary, with its expected
        // demangled path (crate hashes are omitted by this module's Display).
        let symbols: &[(&str, &str)] = &[
            ("_RNvCsgAPbTcwAsCu_7xmc12004blub", "xmc1200::blub"),
            (
                "_RNvCsgAPbTcwAsCu_7xmc120011enable_usic",
                "xmc1200::enable_usic",
            ),
            (
                "_RNvCsgAPbTcwAsCu_7xmc120018___cortex_m_rt_main",
                "xmc1200::__cortex_m_rt_main",
            ),
            (
                "_RNvCsgAPbTcwAsCu_7xmc120019set_normal_baudrate",
                "xmc1200::set_normal_baudrate",
            ),
            ("_RNvCsgAPbTcwAsCu_7xmc12004wait", "xmc1200::wait"),
            ("_RNvCsgAPbTcwAsCu_7xmc12008transmit", "xmc1200::transmit"),
            (
                "_RNvNtCs5bpO7ggFLBd_8cortex_m9interrupt6enable",
                "cortex_m::interrupt::enable",
            ),
            (
                "_RNvNtCs5bpO7ggFLBd_8cortex_m9interrupt7disable",
                "cortex_m::interrupt::disable",
            ),
            (
                "_RNvNtNtCs5bpO7ggFLBd_8cortex_m8register7primask4read",
                "cortex_m::register::primask::read",
            ),
            (
                "_RINvNtCs3Ql2Z2BQrSG_4core3ptr13read_volatilemECsgAPbTcwAsCu_7xmc1200",
                "core::ptr::read_volatile::<u32>",
            ),
            (
                "_RINvNtCs3Ql2Z2BQrSG_4core3ptr14write_volatilemECsgAPbTcwAsCu_7xmc1200",
                "core::ptr::write_volatile::<u32>",
            ),
            (
                "_RNvMNtNtCs3Ql2Z2BQrSG_4core3ptr7mut_ptrOm13read_volatileCsgAPbTcwAsCu_7xmc1200",
                "<*mut u32>::read_volatile",
            ),
            (
                "_RNvMNtNtCs3Ql2Z2BQrSG_4core3ptr7mut_ptrOm14write_volatileCsgAPbTcwAsCu_7xmc1200",
                "<*mut u32>::write_volatile",
            ),
            (
                "_RNvXNtNtCs5bpO7ggFLBd_8cortex_m16critical_section28single_core_critical_sectionNtB2_25SingleCoreCriticalSectionNtCs4YIIJfm9zgo_16critical_section4Impl7acquire",
                "<cortex_m::critical_section::single_core_critical_section::SingleCoreCriticalSection as critical_section::Impl>::acquire",
            ),
            (
                "_RNvXNtNtCs5bpO7ggFLBd_8cortex_m16critical_section28single_core_critical_sectionNtB2_25SingleCoreCriticalSectionNtCs4YIIJfm9zgo_16critical_section4Impl7release",
                "<cortex_m::critical_section::single_core_critical_section::SingleCoreCriticalSection as critical_section::Impl>::release",
            ),
            (
                "_RNvXslZ_NtNtCs3Ql2Z2BQrSG_4core3ops3bitmNtB6_11BitOrAssign12bitor_assign",
                "<u32 as core::ops::bit::BitOrAssign>::bitor_assign",
            ),
            (
                "_RNvXslz_NtNtCs3Ql2Z2BQrSG_4core3ops3bitmNtB6_12BitAndAssign13bitand_assign",
                "<u32 as core::ops::bit::BitAndAssign>::bitand_assign",
            ),
            (
                "_RNCNvCsgAPbTcwAsCu_7xmc12004blub0B3_",
                "xmc1200::blub::{closure#0}",
            ),
            (
                "_RNCNvCsgAPbTcwAsCu_7xmc12004blubs_0B3_",
                "xmc1200::blub::{closure#1}",
            ),
            (
                "_RNCNvCsgAPbTcwAsCu_7xmc12004blubs0_0B3_",
                "xmc1200::blub::{closure#2}",
            ),
            (
                "_RNCNvCsgAPbTcwAsCu_7xmc120011enable_usics0_0B3_",
                "xmc1200::enable_usic::{closure#2}",
            ),
        ];
        let mut failures = Vec::new();
        for (sym_str, expected) in symbols {
            match demangle_v0(sym_str) {
                Ok(sym) => {
                    let actual = sym.path.to_string();
                    if &actual != expected {
                        failures.push(format!(
                            "'{}': expected '{}', got '{}'",
                            sym_str, expected, actual
                        ));
                    }
                }
                Err(e) => failures.push(format!("'{}': {}", sym_str, e)),
            }
        }
        assert!(failures.is_empty(), "mismatches:\n{}", failures.join("\n"));
    }

    #[test]
    fn not_v0_symbols() {
        assert!(demangle_v0("_ZN3foo3bar").is_err());
        assert!(demangle_v0("hello").is_err());
        assert!(demangle_v0("_Rno").is_err());
    }
}
