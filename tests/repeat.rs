use cur::{Cast, Scent};
use cur_macro::scent;

/// Try.
#[test]
fn try_expr() {
    #[scent]
    const TRY: Scent = 'a'?;

    assert_eq!(TRY, Scent::Union(&[Scent::Atom('a'), Scent::Clear]));
}

/// Negation of Try.
#[test]
fn min_try_expr() {
    #[scent]
    const MIN_TRY: Scent = -'a'?;

    assert_eq!(MIN_TRY, Scent::Union(&[Scent::Clear, Scent::Atom('a')]));
}

/// Index with usize.
#[test]
fn exact() {
    #[scent]
    const THREE_REPEATS: Scent = 'a'[3];

    assert_eq!(THREE_REPEATS, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('a'), Scent::Atom('a')]));
}

/// Negation of Index with usize.
#[test]
fn min_exact() {
    #[scent]
    const MIN_THREE_REPEATS: Scent = -'a'[3];

    assert_eq!(MIN_THREE_REPEATS, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('a'), Scent::Atom('a')]));
}

/// Index with RangeFull.
#[test]
fn zero_or_more() {
    #[scent]
    const ZERO_OR_MORE: Scent = 'a'[..];

    assert_eq!(ZERO_OR_MORE, Scent::Repetition(&Scent::Atom('a'), Cast::Maximum));
}

/// Negation of Index.
#[test]
fn min_zero_or_more() {
    #[scent]
    const MIN_ZERO_OR_MORE: Scent = -'a'[..];

    assert_eq!(MIN_ZERO_OR_MORE, Scent::Repetition(&Scent::Atom('a'), Cast::Minimum));
}

/// Index with RangeFrom.
#[test]
fn start_or_more() {
    #[scent]
    const THREE_OR_MORE: Scent = 'a'[3..];

    assert_eq!(THREE_OR_MORE, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('a'), Scent::Atom('a'), Scent::Repetition(&Scent::Atom('a'), Cast::Maximum)]));
}

/// Negation of Index with RangeFrom.
#[test]
fn min_start_or_more() {
    #[scent]
    const MIN_THREE_OR_MORE: Scent = -'a'[3..];

    assert_eq!(MIN_THREE_OR_MORE, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('a'), Scent::Atom('a'), Scent::Repetition(&Scent::Atom('a'), Cast::Minimum)]));
}

/// Index with RangeTo.
#[test]
fn less_than_end() {
    #[scent]
    const LESS_THAN_FOUR: Scent = 'a'[..4];

    assert_eq!(LESS_THAN_FOUR, Scent::Sequence(&[Scent::Union(&[Scent::Atom('a'), Scent::Clear]), Scent::Union(&[Scent::Atom('a'), Scent::Clear]), Scent::Union(&[Scent::Atom('a'), Scent::Clear])]));
}

/// Negation of Index with RangeTo.
#[test]
fn min_less_than_end() {
    #[scent]
    const MIN_LESS_THAN_FOUR: Scent = -'a'[..4];

    assert_eq!(MIN_LESS_THAN_FOUR, Scent::Sequence(&[Scent::Union(&[Scent::Clear, Scent::Atom('a')]), Scent::Union(&[Scent::Clear, Scent::Atom('a')]), Scent::Union(&[Scent::Clear, Scent::Atom('a')])]));
}

/// Index with RangeToInclusive.
#[test]
fn end_or_less() {
    #[scent]
    const THREE_OR_LESS: Scent = 'a'[..=3];

    assert_eq!(THREE_OR_LESS, Scent::Sequence(&[Scent::Union(&[Scent::Atom('a'), Scent::Clear]), Scent::Union(&[Scent::Atom('a'), Scent::Clear]), Scent::Union(&[Scent::Atom('a'), Scent::Clear])]));
}

/// Negation of Index with RangeToInclusive.
#[test]
fn min_end_or_less() {
    #[scent]
    const MIN_THREE_OR_LESS: Scent = -'a'[..=3];

    assert_eq!(MIN_THREE_OR_LESS, Scent::Sequence(&[Scent::Union(&[Scent::Clear, Scent::Atom('a')]), Scent::Union(&[Scent::Clear, Scent::Atom('a')]), Scent::Union(&[Scent::Clear, Scent::Atom('a')])]));
}

/// Index with Range.
#[test]
fn start_to_end() {
    #[scent]
    const TWO_TO_FIVE: Scent = 'a'[2..5];

    assert_eq!(TWO_TO_FIVE, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('a'), Scent::Union(&[Scent::Atom('a'), Scent::Clear]), Scent::Union(&[Scent::Atom('a'), Scent::Clear])]));
}

/// Negation of Index with Range.
#[test]
fn min_start_to_end() {
    #[scent]
    const MIN_TWO_TO_FIVE: Scent = -'a'[2..5];

    assert_eq!(MIN_TWO_TO_FIVE, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('a'), Scent::Union(&[Scent::Clear, Scent::Atom('a')]), Scent::Union(&[Scent::Clear, Scent::Atom('a')])]));
}

/// Index with RangeToInclusive.
#[test]
fn start_to_end_inclusive() {
    #[scent]
    const TWO_TO_FOUR_INCLUSIVE: Scent = 'a'[2..=4];

    assert_eq!(TWO_TO_FOUR_INCLUSIVE, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('a'), Scent::Union(&[Scent::Atom('a'), Scent::Clear]), Scent::Union(&[Scent::Atom('a'), Scent::Clear])]));
}

/// Negation of Index with RangeToInclusive.
#[test]
fn min_start_to_end_inclusive() {
    #[scent]
    const MIN_TWO_TO_FOUR_INCLUSIVE: Scent = -'a'[2..=4];

    assert_eq!(MIN_TWO_TO_FOUR_INCLUSIVE, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('a'), Scent::Union(&[Scent::Clear, Scent::Atom('a')]), Scent::Union(&[Scent::Clear, Scent::Atom('a')])]));
}

/// BitOr with Try.
#[test]
fn try_union() {
    #[scent]
    const TRY_UNION: Scent = ('a' | 'b')?;

    assert_eq!(TRY_UNION, Scent::Union(&[Scent::Atom('a'), Scent::Atom('b'), Scent::Clear]));
}

/// Negation of BitOr with Try.
#[test]
fn min_try_union() {
    #[scent]
    const MIN_TRY_UNION: Scent = -('a' | 'b')?;

    assert_eq!(MIN_TRY_UNION, Scent::Union(&[Scent::Clear, Scent::Atom('a'), Scent::Atom('b')]));
}

/// BitOr with Index.
#[test]
fn repeat_union() {
    #[scent]
    const REPEAT_UNION: Scent = ('a' | 'b')[1..3];

    assert_eq!(REPEAT_UNION, Scent::Sequence(&[Scent::Union(&[Scent::Atom('a'), Scent::Atom('b')]), Scent::Union(&[Scent::Atom('a'), Scent::Atom('b'), Scent::Clear])]));
}

/// Negation of BitOr with Index.
#[test]
fn min_repeat_union() {
    #[scent]
    const MIN_REPEAT_UNION: Scent = -('a' | 'b')[1..3];

    assert_eq!(MIN_REPEAT_UNION, Scent::Sequence(&[Scent::Union(&[Scent::Atom('a'), Scent::Atom('b')]), Scent::Union(&[Scent::Clear, Scent::Atom('a'), Scent::Atom('b')])]));
}

/// Add with Try.
#[test]
fn try_sequence() {
    #[scent]
    const TRY_SEQUENCE: Scent = ('a' + 'b')?;

    assert_eq!(TRY_SEQUENCE, Scent::Union(&[Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b')]), Scent::Clear]));
}

/// Negation of Add with Try.
#[test]
fn min_try_sequence() {
    #[scent]
    const MIN_TRY_SEQUENCE: Scent = -('a' + 'b')?;

    assert_eq!(MIN_TRY_SEQUENCE, Scent::Union(&[Scent::Clear, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b')])]));
}

/// Add with Index.
#[test]
fn repeat_sequence() {
    #[scent]
    const REPEAT_SEQUENCE: Scent = ('a' + 'b')[1..];

    assert_eq!(REPEAT_SEQUENCE, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b'), Scent::Repetition(&Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b')]), Cast::Maximum)]));
}

/// Negation of Add with Index.
#[test]
fn min_repeat_sequence() {
    #[scent]
    const MIN_REPEAT_SEQUENCE: Scent = -('a' + 'b')[1..];

    assert_eq!(MIN_REPEAT_SEQUENCE, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b'), Scent::Repetition(&Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b')]), Cast::Minimum)]));
}
