use cur::{Cast, Scent};
use cur_macro::scent;

#[scent]
const DIGIT: Scent = '0'..'9';

mod scents {
    use super::*;

    #[scent]
    pub const LOWERCASE: Scent = 'a'..'z';
}

/// A [`Scent`].
#[test]
fn copy() {
    #[scent]
    const COPY: Scent = DIGIT;

    assert_eq!(COPY, Scent::Range('0', '9'));
}

/// A Union.
#[test]
fn union() {
    #[scent]
    const DIGIT_OR_A: Scent = 'a' | DIGIT;

    assert_eq!(DIGIT_OR_A, Scent::Union(&[Scent::Atom('a'), Scent::Range('0', '9')]));
}

/// A sequence.
#[test]
fn sequence() {
    #[scent]
    const DIGIT_AND_A: Scent = DIGIT + 'a';

    assert_eq!(DIGIT_AND_A, Scent::Sequence(&[Scent::Range('0', '9'), Scent::Atom('a')]));
}

/// A option.
#[test]
fn option() {
    #[scent]
    const OPTIONAL_DIGIT: Scent = DIGIT?;

    assert_eq!(OPTIONAL_DIGIT, Scent::Union(&[Scent::Range('0', '9'), Scent::Clear]));
}

/// A minimal option.
#[test]
fn min_option() {
    #[scent]
    const MIN_OPT_DIGIT: Scent = -DIGIT?;

    assert_eq!(MIN_OPT_DIGIT, Scent::Union(&[Scent::Clear, Scent::Range('0', '9')]));
}

/// A repetition.
#[test]
fn repetition() {
    #[scent]
    const ZERO_OR_MORE_DIGITS: Scent = DIGIT[..];

    assert_eq!(ZERO_OR_MORE_DIGITS, Scent::Repetition(&Scent::Range('0', '9'), Cast::Maximum));
}

/// A minimum repetition.
#[test]
fn min_repetition() {
    #[scent]
    const MIN_DIGITS: Scent = -DIGIT[..];

    assert_eq!(MIN_DIGITS, Scent::Repetition(&Scent::Range('0', '9'), Cast::Minimum));
}

/// A path.
#[test]
fn path() {
    #[scent]
    const ALPHANUM: Scent = DIGIT | scents::LOWERCASE;

    assert_eq!(ALPHANUM, Scent::Union(&[Scent::Range('0', '9'), Scent::Range('a', 'z')]));
}
