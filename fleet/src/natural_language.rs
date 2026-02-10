use itertools::Itertools;

pub trait JoinAnd {
    fn join_and(self) -> String;
}

impl<T: IntoIterator<Item = impl ToString>> JoinAnd for T {
    fn join_and(self) -> String {
        let strings = self.into_iter().map(|x| x.to_string()).collect_vec();
        let (last, start) = strings
            .split_last()
            .expect("Cannot join_and empty iterator");
        if start.is_empty() {
            last.clone()
        } else {
            start.join(", ") + " and " + last
        }
    }
}

pub trait JoinOr {
    fn join_or(self) -> String;
}

impl<T: IntoIterator<Item = impl ToString>> JoinOr for T {
    fn join_or(self) -> String {
        let strings = self.into_iter().map(|x| x.to_string()).collect_vec();
        let (last, start) = strings.split_last().expect("Cannot join_or empty iterator");
        if start.is_empty() {
            last.clone()
        } else {
            start.join(", ") + " or " + last
        }
    }
}

pub fn an(thing: impl AsRef<str>) -> String {
    let thing = thing.as_ref();
    if let 'a' | 'e' | 'i' | 'o' | 'u' = thing.chars().next().unwrap() {
        format!("an {thing}")
    } else {
        format!("a {thing}")
    }
}
pub fn nth(count: usize) -> String {
    match count {
        0 => panic!(
            "Trying to refer to the zeroth of something. If this is actually an index, remember to add 1."
        ),
        n if n % 100 == 11 || n % 100 == 12 || n % 100 == 13 => format!("{n}th"),
        n if n % 10 == 1 => format!("{n}st"),
        n if n % 10 == 2 => format!("{n}nd"),
        n if n % 10 == 3 => format!("{n}rd"),
        n => format!("{n}th"),
    }
}
pub fn plural<T>(singular: T, plural: T, count: usize) -> T {
    match count {
        1 => singular,
        _ => plural,
    }
}
