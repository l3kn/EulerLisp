#[macro_export]
macro_rules! check_arity {
    ($args: ident, $number: expr) => {
        if $args.len() != $number {
            return Err(InvalidNumberOfArguments);
        }
    }
}
