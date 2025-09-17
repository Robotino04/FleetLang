use itertools::Itertools;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum QuoteType {
    Single,
    Double,
}

pub fn unescape(str: impl AsRef<str>, quote: QuoteType) -> Result<String, Vec<usize>> {
    let mut result = "".to_string();
    let chars = (str.as_ref().to_string() + "\0").chars().collect_vec();
    let mut pairs = chars.windows(2).enumerate();

    let mut unknown_escape_sequences = vec![];

    while let Some((i, [a, b])) = pairs.next() {
        result.push(match (*a, *b) {
            ('\\', 'n') => {
                pairs.next();
                '\n'
            }
            ('\\', 'r') => {
                pairs.next();
                '\n'
            }
            ('\\', 't') => {
                pairs.next();
                '\t'
            }
            ('\\', '\\') => {
                pairs.next();
                '\\'
            }
            ('\\', '"') if quote == QuoteType::Double => {
                pairs.next();
                '"'
            }
            ('\\', '\'') if quote == QuoteType::Single => {
                pairs.next();
                '\''
            }
            ('\\', a) => {
                unknown_escape_sequences.push(i);
                a
            }
            (a, _) => a,
        });
    }
    if !unknown_escape_sequences.is_empty() {
        return Err(unknown_escape_sequences);
    }

    Ok(result)
}

pub fn escape(str: impl AsRef<str>, quote: QuoteType) -> String {
    str.as_ref()
        .to_string()
        .chars()
        .map(|chr| match chr {
            '\n' => "\\n".to_string(),
            '\r' => "\\r".to_string(),
            '\t' => "\\t".to_string(),
            '\\' => "\\\\".to_string(),
            '\'' if quote == QuoteType::Single => "\\'".to_string(),
            '"' if quote == QuoteType::Double => "\\\"".to_string(),
            x => x.to_string(),
        })
        .collect()
}
