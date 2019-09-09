//! `cur_macro` - Procedural macros for [`cur`]
#![warn(
    absolute_paths_not_starting_with_crate,
    anonymous_parameters,
    bare_trait_objects,
    deprecated_in_future,
    elided_lifetimes_in_paths,
    ellipsis_inclusive_range_patterns,
    explicit_outlives_requirements,
    keyword_idents,
    macro_use_extern_crate,
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    missing_doc_code_examples,
    private_doc_tests,
    trivial_casts,
    trivial_numeric_casts,
    unreachable_pub,
    unsafe_code,
    unstable_features,
    unused_extern_crates,
    unused_import_braces,
    unused_labels,
    unused_lifetimes,
    unused_qualifications,
    unused_results,
    variant_size_differences,
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::restriction
)]
// Rustc lints that are not warned:
// box_pointers: Boxes are generally okay.
// single_use_lifetimes: There are issues with derived traits.
#![allow(
    clippy::fallible_impl_from, // Above lints assume a given use; issues should be detected by tests or other lints.
    clippy::implicit_return, // Omitting the return keyword is idiomatic Rust code.
    clippy::missing_inline_in_public_items, // There are issues with derived traits.
    clippy::multiple_crate_versions, // Not always possible to resolve.
    clippy::suspicious_arithmetic_impl, // Assumes a specific use; issues should be detected by tests.
    clippy::suspicious_op_assign_impl, // Assumes a specific use; issues should be detected by tests.
)]
#![no_std]

extern crate alloc;
extern crate proc_macro;

use alloc::{boxed::Box, vec, vec::Vec};
use core::{
    convert::{TryFrom, TryInto},
    ops::Deref,
};
use proc_macro::TokenStream;
use proc_macro2::{
    Delimiter, Group, Literal, Punct, Spacing, Span, TokenStream as TokenStream2,
    TokenTree,
};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parse::{Parse, ParseStream, Result as ParseResult},
    parse_macro_input, Ident, ItemConst,
};
use syn::{
    BinOp, Error, Expr, ExprBinary, ExprPath, ExprRange, ExprRepeat, ExprTry, ExprUnary, Lit,
    RangeLimits,
};

/// Converts `item` such that its expression is a `cur::Scent`.
///
/// Creating `cur::Scent`s can quickly become complex and error-prone. It is intended that a user
/// can use this procedural macro to build a `cur::Scent` that is clearly understandable using
/// valid rust syntax.
#[proc_macro_attribute]
pub fn scent(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ScentInput { ident, scent } = parse_macro_input!(item as ScentInput);

    TokenStream::from(quote! {
        const #ident: Scent = #scent;
    })
}

/// Information required to create a const [`Scent`] definition.
struct ScentInput {
    /// Identifier of the [`Scent`].
    ident: Ident,
    /// Information required to create the [`Scent`] tokens.
    scent: ScentBuilder,
}

impl Parse for ScentInput {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        input.parse().and_then(|item: ItemConst| {
            Ok(Self {
                ident: item.ident,
                scent: (*item.expr).try_into()?,
            })
        })
    }
}

/// Maps to [`Scent`] definitions.
///
/// Using `ScentBuilder` instead of [`Scent`] removes need for adding [`cur`] as a dependency,
/// which would be a circular dependency.
#[derive(Clone, Debug)]
enum ScentBuilder {
    /// Maps to [`Scent::Clear`].
    Clear,
    /// Maps to [`Scent::Atom`].
    Atom(char),
    /// Maps to [`Scent::Union`].
    Union(Vec<ScentBuilder>),
    /// Maps to [`Scent::Sequence`].
    Sequence(Vec<ScentBuilder>),
    /// Maps to [`Scent::Repetition`].
    Repetition(Box<ScentBuilder>, CastBuilder),
}

impl ScentBuilder {
    /// Creates a `ScentBuilder` that repeats `self` as specified by `repeater`.
    fn repeat(self, repeater: &ScentRepeater) -> Self {
        if repeater.minimum == 0 && repeater.maximum == usize::max_value() {
            Self::Repetition(Box::new(self), CastBuilder::Maximum)
        } else {
            let mut sequence = Vec::new();

            for _ in 0..repeater.minimum {
                sequence.extend(self.clone().into_elements());
            }

            if repeater.maximum == usize::max_value() {
                sequence.push(Self::Repetition(Box::new(self), CastBuilder::Maximum));
            } else {
                for _ in repeater.minimum..repeater.maximum {
                    sequence.push(self.clone().branch(Self::Clear));
                }
            }

            Self::Sequence(sequence)
        }
    }

    /// Creates a `ScentBuilder` with alternate `branch` if `self` fails.
    fn branch(self, branch: Self) -> Self {
        Self::Union(if let ScentBuilder::Union(mut branches) = self {
            branches.push(branch);
            branches
        } else {
            vec![self, branch]
        })
    }

    /// Converts `self` into a sequence of `ScentBuilder`s.
    fn into_elements(self) -> Vec<Self> {
        if let Self::Sequence(elements) = self {
            elements
        } else {
            vec![self]
        }
    }

    /// Creates a `ScentBuilder` with the minimal cast of `self`.
    ///
    /// `ScentBuilder`s that cannot be cast (ex: [`Scent::Atom`]) just return `self`. Any
    /// inner casts within `self` are not changed.
    fn minimize_cast(self) -> Self {
        match self {
            Self::Repetition(scent, ..) => Self::Repetition(scent, CastBuilder::Minimum),
            Self::Sequence(elements) => {
                Self::Sequence(elements.into_iter().map(Self::minimize_cast).collect())
            }
            Self::Union(mut branches) => {
                if let Some(Self::Clear) = branches.last() {
                    let _ = branches.pop();
                    branches.insert(0, Self::Clear);
                }

                Self::Union(branches)
            }
            Self::Atom(..) | Self::Clear => self,
        }
    }
}

impl ToTokens for ScentBuilder {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append(Ident::new("Scent", Span::call_site()));
        tokens.append(Punct::new(':', Spacing::Joint));
        tokens.append(Punct::new(':', Spacing::Alone));

        match self {
            Self::Clear => {
                tokens.append(Ident::new("Clear", Span::call_site()));
            }
            Self::Atom(c) => {
                tokens.append(Ident::new("Atom", Span::call_site()));
                tokens.append(Group::new(
                    Delimiter::Parenthesis,
                    TokenTree::from(Literal::character(*c)).into(),
                ));
            }
            Self::Sequence(elements) => {
                let mut element_list = TokenStream2::new();
                let mut element_array = TokenStream2::new();

                tokens.append(Ident::new("Sequence", Span::call_site()));

                element_array.append(Punct::new('&', Spacing::Alone));

                if let Some(first_element) = elements.first() {
                    first_element.to_tokens(&mut element_list);
                }

                for element in elements.iter().skip(1) {
                    element_list.append(Punct::new(',', Spacing::Alone));
                    element.to_tokens(&mut element_list);
                }

                element_array.append(Group::new(Delimiter::Bracket, element_list));
                tokens.append(Group::new(Delimiter::Parenthesis, element_array));
            }
            Self::Union(branches) => {
                let mut branch_array = TokenStream2::new();
                let mut branch_list = TokenStream2::new();

                tokens.append(Ident::new("Union", Span::call_site()));

                branch_array.append(Punct::new('&', Spacing::Alone));

                if let Some(first_branch) = branches.first() {
                    first_branch.to_tokens(&mut branch_list);
                }

                for branch in branches.iter().skip(1) {
                    branch_list.append(Punct::new(',', Spacing::Alone));
                    branch.to_tokens(&mut branch_list);
                }

                branch_array.append(Group::new(Delimiter::Bracket, branch_list));
                tokens.append(Group::new(Delimiter::Parenthesis, branch_array));
            }
            Self::Repetition(scent, desire) => {
                let mut repetition_args = TokenStream2::new();

                tokens.append(Ident::new("Repetition", Span::call_site()));

                repetition_args.append(Punct::new('&', Spacing::Alone));
                scent.to_tokens(&mut repetition_args);
                repetition_args.append(Punct::new(',', Spacing::Alone));
                desire.to_tokens(&mut repetition_args);

                tokens.append(Group::new(Delimiter::Parenthesis, repetition_args));
            }
        }
    }
}

impl TryFrom<Expr> for ScentBuilder {
    type Error = Error;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Path(path) => path.try_into(),
            Expr::Lit(literal) => literal.lit.try_into(),
            Expr::Binary(binary) => binary.try_into(),
            Expr::Paren(paren) => (*paren.expr).try_into(),
            Expr::Repeat(repeat) => repeat.try_into(),
            Expr::Try(try_expr) => try_expr.try_into(),
            Expr::Unary(unary) => unary.try_into(),
            Expr::Range(..)
            | Expr::Box(..)
            | Expr::Await(..)
            | Expr::Array(..)
            | Expr::Call(..)
            | Expr::MethodCall(..)
            | Expr::Tuple(..)
            | Expr::Cast(..)
            | Expr::Let(..)
            | Expr::If(..)
            | Expr::While(..)
            | Expr::ForLoop(..)
            | Expr::Loop(..)
            | Expr::Match(..)
            | Expr::Closure(..)
            | Expr::Unsafe(..)
            | Expr::Block(..)
            | Expr::Assign(..)
            | Expr::AssignOp(..)
            | Expr::Field(..)
            | Expr::Index(..)
            | Expr::Reference(..)
            | Expr::Break(..)
            | Expr::Continue(..)
            | Expr::Return(..)
            | Expr::Macro(..)
            | Expr::Struct(..)
            | Expr::Group(..)
            | Expr::Async(..)
            | Expr::TryBlock(..)
            | Expr::Type(..)
            | Expr::Yield(..)
            | Expr::Verbatim(..)
            | Expr::__Nonexhaustive => Err(Error::new_spanned(value, "Invalid scent expression")),
        }
    }
}

impl TryFrom<ExprBinary> for ScentBuilder {
    type Error = Error;

    fn try_from(value: ExprBinary) -> Result<Self, Self::Error> {
        let lhs: Self = (*value.left).try_into()?;
        let rhs: Self = (*value.right).try_into()?;

        match value.op {
            BinOp::BitOr(..) => Ok(lhs.branch(rhs)),
            BinOp::Add(..) => {
                if let ScentBuilder::Sequence(mut elements) = lhs {
                    elements.push(rhs);
                    Ok(ScentBuilder::Sequence(elements))
                } else {
                    Ok(ScentBuilder::Sequence(vec![lhs, rhs]))
                }
            }
            BinOp::BitAnd(..)
            | BinOp::Sub(..)
            | BinOp::Mul(..)
            | BinOp::Div(..)
            | BinOp::Rem(..)
            | BinOp::And(..)
            | BinOp::Or(..)
            | BinOp::BitXor(..)
            | BinOp::Shl(..)
            | BinOp::Shr(..)
            | BinOp::Eq(..)
            | BinOp::Lt(..)
            | BinOp::Le(..)
            | BinOp::Ne(..)
            | BinOp::Ge(..)
            | BinOp::Gt(..)
            | BinOp::AddEq(..)
            | BinOp::SubEq(..)
            | BinOp::MulEq(..)
            | BinOp::DivEq(..)
            | BinOp::RemEq(..)
            | BinOp::BitXorEq(..)
            | BinOp::BitAndEq(..)
            | BinOp::BitOrEq(..)
            | BinOp::ShlEq(..)
            | BinOp::ShrEq(..) => Err(Error::new_spanned(value.op, "Invalid binary operation")),
        }
    }
}

impl TryFrom<ExprPath> for ScentBuilder {
    type Error = Error;

    fn try_from(_value: ExprPath) -> Result<Self, Self::Error> {
        Ok(ScentBuilder::Clear)
    }
}

impl TryFrom<ExprRepeat> for ScentBuilder {
    type Error = Error;

    fn try_from(value: ExprRepeat) -> Result<Self, Self::Error> {
        let repeater = ScentRepeater::try_from(*value.len)?;
        (*value.expr)
            .try_into()
            .map(|scent: Self| scent.repeat(&repeater))
    }
}

impl TryFrom<ExprTry> for ScentBuilder {
    type Error = Error;

    fn try_from(value: ExprTry) -> Result<Self, Self::Error> {
        Self::try_from(value.expr.deref().clone()).map(|scent| scent.branch(Self::Clear))
    }
}

impl TryFrom<ExprUnary> for ScentBuilder {
    type Error = Error;

    fn try_from(value: ExprUnary) -> Result<Self, Self::Error> {
        match *value.expr {
            Expr::Try(..) | Expr::Repeat(..) => {
                Self::try_from(*value.expr).map(Self::minimize_cast)
            }
            Expr::Path(..)
            | Expr::Lit(..)
            | Expr::Binary(..)
            | Expr::Paren(..)
            | Expr::Unary(..)
            | Expr::Range(..)
            | Expr::Box(..)
            | Expr::Await(..)
            | Expr::Array(..)
            | Expr::Call(..)
            | Expr::MethodCall(..)
            | Expr::Tuple(..)
            | Expr::Cast(..)
            | Expr::Let(..)
            | Expr::If(..)
            | Expr::While(..)
            | Expr::ForLoop(..)
            | Expr::Loop(..)
            | Expr::Match(..)
            | Expr::Closure(..)
            | Expr::Unsafe(..)
            | Expr::Block(..)
            | Expr::Assign(..)
            | Expr::AssignOp(..)
            | Expr::Field(..)
            | Expr::Index(..)
            | Expr::Reference(..)
            | Expr::Break(..)
            | Expr::Continue(..)
            | Expr::Return(..)
            | Expr::Macro(..)
            | Expr::Struct(..)
            | Expr::Group(..)
            | Expr::Async(..)
            | Expr::TryBlock(..)
            | Expr::Type(..)
            | Expr::Yield(..)
            | Expr::Verbatim(..)
            | Expr::__Nonexhaustive => Err(Error::new_spanned(
                value.expr,
                "Expected try or repeat expression",
            )),
        }
    }
}

impl TryFrom<Lit> for ScentBuilder {
    type Error = Error;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        match value {
            Lit::Char(c) => Ok(ScentBuilder::Atom(c.value())),
            Lit::Str(s) => Ok(ScentBuilder::Sequence(
                s.value().chars().map(ScentBuilder::Atom).collect(),
            )),
            Lit::ByteStr(..)
            | Lit::Byte(..)
            | Lit::Int(..)
            | Lit::Float(..)
            | Lit::Bool(..)
            | Lit::Verbatim(..) => Err(Error::new_spanned(
                value,
                "Expected character or string literal.",
            )),
        }
    }
}

/// Information regarding the number of times a [`ScentBuilder`] can be repeated.
#[derive(Debug)]
struct ScentRepeater {
    /// The smallest number of repeats.
    minimum: usize,
    /// The largest number of repeats.
    ///
    /// A number <= `minimum` indicates the [`ScentBuilder`] must be repeated exactly
    /// `minimum` times.
    maximum: usize,
}

impl ScentRepeater {
    /// Converts `lit` to a [`usize`].
    ///
    /// [`Err`] indicates `lit` is unable to be converted.
    fn usize_try_from_lit(lit: &Lit) -> ParseResult<usize> {
        if let Lit::Int(int) = lit {
            int.base10_parse::<usize>()
        } else {
            Err(Error::new_spanned(lit, "Expected usize literal"))
        }
    }

    /// Converts `expr` to a [`usize`].
    ///
    /// [`Err`] indicates `expr` is unable to be converted.
    fn usize_try_from_expr(expr: Expr) -> ParseResult<usize> {
        if let Expr::Lit(literal) = expr {
            Self::usize_try_from_lit(&literal.lit)
        } else {
            Err(Error::new_spanned(expr, "Expected literal"))
        }
    }
}

impl TryFrom<Expr> for ScentRepeater {
    type Error = Error;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Lit(literal) => literal.lit.try_into(),
            Expr::Range(range) => range.try_into(),
            Expr::Try(..)
            | Expr::Repeat(..)
            | Expr::Path(..)
            | Expr::Binary(..)
            | Expr::Paren(..)
            | Expr::Unary(..)
            | Expr::Box(..)
            | Expr::Await(..)
            | Expr::Array(..)
            | Expr::Call(..)
            | Expr::MethodCall(..)
            | Expr::Tuple(..)
            | Expr::Cast(..)
            | Expr::Let(..)
            | Expr::If(..)
            | Expr::While(..)
            | Expr::ForLoop(..)
            | Expr::Loop(..)
            | Expr::Match(..)
            | Expr::Closure(..)
            | Expr::Unsafe(..)
            | Expr::Block(..)
            | Expr::Assign(..)
            | Expr::AssignOp(..)
            | Expr::Field(..)
            | Expr::Index(..)
            | Expr::Reference(..)
            | Expr::Break(..)
            | Expr::Continue(..)
            | Expr::Return(..)
            | Expr::Macro(..)
            | Expr::Struct(..)
            | Expr::Group(..)
            | Expr::Async(..)
            | Expr::TryBlock(..)
            | Expr::Type(..)
            | Expr::Yield(..)
            | Expr::Verbatim(..)
            | Expr::__Nonexhaustive => Err(Error::new_spanned(value, "Expected literal or range")),
        }
    }
}

impl TryFrom<ExprRange> for ScentRepeater {
    type Error = Error;

    fn try_from(value: ExprRange) -> Result<Self, Self::Error> {
        Ok(Self {
            minimum: value
                .clone()
                .from
                .map_or(Ok(0), |from| Self::usize_try_from_expr(*from))?,
            maximum: value.clone().to.map_or(Ok(usize::max_value()), |to| {
                Self::usize_try_from_expr(*to).map(|max| {
                    max.saturating_sub(if let RangeLimits::HalfOpen(..) = value.limits {
                        1
                    } else {
                        0
                    })
                })
            })?,
        })
    }
}

impl TryFrom<Lit> for ScentRepeater {
    type Error = Error;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        Self::usize_try_from_lit(&value).map(|minimum| Self {
            minimum,
            maximum: 0,
        })
    }
}

/// Maps to [`cur::Cast`].
///
/// Using `CastBuilder` instead of [`Cast`] removes need for adding [`cur`] as a dependency,
/// which would be a circular dependency.
#[derive(Clone, Debug)]
enum CastBuilder {
    /// Maps to [`Cast::Minimum`].
    Minimum,
    /// Maps to [`Cast::Maximum`].
    Maximum,
}

impl ToTokens for CastBuilder {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append(Ident::new("Cast", Span::call_site()));
        tokens.append(Punct::new(':', Spacing::Joint));
        tokens.append(Punct::new(':', Spacing::Alone));

        match self {
            Self::Minimum => tokens.append(Ident::new("Minimum", Span::call_site())),
            Self::Maximum => tokens.append(Ident::new("Maximum", Span::call_site())),
        }
    }
}
