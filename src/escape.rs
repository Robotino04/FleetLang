pub fn unescape(str: impl AsRef<str>) -> String {
    let mut result = "".to_string();
    let chars = (str.as_ref().to_string() + "\0")
        .chars()
        .collect::<Vec<_>>();
    let mut pairs = chars.windows(2);

    while let Some([a, b]) = pairs.next() {
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
            ('\\', '"') => {
                pairs.next();
                '"'
            }
            ('\\', '\'') => {
                pairs.next();
                '\''
            }
            (a, _) => a,
        });
    }
    result
}
