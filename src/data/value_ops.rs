use crate::data::objects::Value;

#[derive(Copy, Clone, Debug)]
pub enum NumberCastResult {
    Int(i64),
    Float(f64),
}

impl NumberCastResult {
    pub fn downgrade(&self) -> f64 {
        match *self {
            Self::Float(f) => f,
            Self::Int(n) => n as f64,
        }
    }
}

pub fn numeric_cast(value: &Value) -> Option<NumberCastResult> {
    value
        .unwrap_int()
        .map(NumberCastResult::Int)
        .or_else(|| value.unwrap_float().map(NumberCastResult::Float))
}

pub fn equality_operator(left: &Value, right: &Value) -> bool {
    left.eq(right) || {
        let left = numeric_cast(left);
        let right = numeric_cast(right);
        left.and(right)
            .map(|_| left.unwrap().downgrade() == right.unwrap().downgrade())
            .unwrap_or(false)
    }
}

macro_rules! cast_binary {
    ($left:expr, $op:tt, $right:expr ) => {
        match ($left, $right) {
            (&Value::Int(left), &Value::Int(right)) => Some((left $op right).into()),
            (left, right) => {
                use crate::data::value_ops::numeric_cast;
                fn f(left: &Value, right: &Value) -> Option<Value> {
                    let left = numeric_cast(left)?;
                    let right = numeric_cast(right)?;
                    Some((left.downgrade() $op right.downgrade()).into())
                }
                f(left, right)
            }
        }
    };
}

macro_rules! comparison_operator {
    ($left:expr, $right:expr, $pat:pat) => {
        match $left.partial_cmp($right).or_else(|| {
            use crate::data::value_ops::numeric_cast;
            let left = numeric_cast($left)?;
            let right = numeric_cast($right)?;
            left.downgrade().partial_cmp(&right.downgrade())
        }) {
            Some($pat) => Some(true),
            Some(_) => Some(false),

            None => None,
        }
    };
}

pub(crate) use cast_binary;
pub(crate) use comparison_operator;

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;

    use crate::data::gc::GC;
    use rstest::*;

    use super::*;

    #[fixture]
    pub fn gc() -> GC {
        unsafe { GC::default_gc() }
    }

    #[rstest]
    fn cmp_should_work_for_strings(mut gc: GC) {
        let s123 = gc.store("123".to_string());
        let s123_2 = gc.store("123".to_string());
        let s124 = gc.store("124".to_string());

        assert_eq!(
            comparison_operator!(&s123, &s123_2, Ordering::Equal),
            Some(true)
        );
        assert_eq!(
            comparison_operator!(&s123, &s123_2, Ordering::Equal | Ordering::Greater),
            Some(true)
        );

        assert_eq!(
            comparison_operator!(&s123, &s124, Ordering::Equal),
            Some(false)
        );

        assert_eq!(
            comparison_operator!(&s123, &s124, Ordering::Less),
            Some(true)
        );
        drop(s123);
        drop(s123_2);
        drop(s124);
    }

    #[rstest]
    fn eq_should_work_for_strings(mut gc: GC) {
        let s123 = gc.store("123".to_string());
        let s123_2 = gc.store("123".to_string());
        let s124 = gc.store("124".to_string());

        assert!(equality_operator(&s123, &s123_2));
        assert!(!equality_operator(&s123, &s124));

        drop(s123);
        drop(s123_2);
        drop(s124);
    }

    #[test]
    fn cmp_should_work_for_ints() {
        assert_eq!(
            comparison_operator!(&Value::Int(0), &Value::Int(0), Ordering::Equal),
            Some(true)
        );
        assert_eq!(
            comparison_operator!(
                &Value::Int(0),
                &Value::Int(0),
                Ordering::Greater | Ordering::Equal
            ),
            Some(true)
        );

        assert_eq!(
            comparison_operator!(&Value::Int(0), &Value::Int(1), Ordering::Equal),
            Some(false)
        );

        assert_eq!(
            comparison_operator!(&Value::Int(0), &Value::Int(1), Ordering::Less),
            Some(true)
        );
    }

    #[test]
    fn eq_should_work_for_ints() {
        assert!(equality_operator(&12i64.into(), &12i64.into()));
        assert!(!equality_operator(&12i64.into(), &13i64.into()));
    }

    #[rstest]
    fn eq_should_work_for_incompatible(mut gc: GC) {
        let s = gc.store("12".to_string());

        assert!(!equality_operator(&s, &12.into()));
        drop(s)
    }

    #[test]
    fn eq_should_work_for_mixed_numbers() {
        assert!(equality_operator(&12i64.into(), &12f64.into()));
        assert!(!equality_operator(&12i64.into(), &10f64.into()));
    }

    #[test]
    fn cmp_should_work_for_mixed_numbers() {
        assert_eq!(
            comparison_operator!(&Value::from(12i64), &13f64.into(), Ordering::Less),
            Some(true),
        );
        assert_eq!(
            comparison_operator!(&Value::from(12i64), &13f64.into(), Ordering::Greater),
            Some(false)
        );
    }
}
