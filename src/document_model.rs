use indent::indent_all_by;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocumentElement {
    Concatenation(Vec<DocumentElement>),
    Indentation(Box<DocumentElement>),

    Text(String),

    SpaceEater,
    ReverseSpaceEater,

    // these two also eat nearby spaces
    ForcedLineBreak,
    CollapsableLineBreak,

    ForcedSpace,
    CollapsableSpace,
}

impl DocumentElement {
    pub fn double_space_eater() -> DocumentElement {
        DocumentElement::Concatenation(vec![
            DocumentElement::ReverseSpaceEater,
            DocumentElement::SpaceEater,
        ])
    }

    pub fn spaced_concatentation(
        spacing: DocumentElement,
        elements: Vec<DocumentElement>,
    ) -> DocumentElement {
        let mut res = vec![];
        for el in elements {
            res.push(el);
            res.push(spacing.clone());
        }
        res.pop();
        DocumentElement::Concatenation(res)
    }

    pub fn empty() -> DocumentElement {
        DocumentElement::Concatenation(vec![])
    }
}

pub fn fully_flatten_document(mut element: DocumentElement) -> DocumentElement {
    let mut i = 0;
    loop {
        let (new_element, did_change) = flatten_document_elements_once(element);

        element = new_element;
        i += 1;
        if !did_change {
            eprintln!("Took {i} iterations to flatten document");
            break;
        }
    }

    element
}

fn flatten_document_elements_once(element: DocumentElement) -> (DocumentElement, bool) {
    match element {
        text @ DocumentElement::Text(_) => (text, false),
        DocumentElement::Concatenation(body) => {
            let mut res = vec![];
            let mut did_change = false;
            for el in body {
                use DocumentElement::*;
                if let Concatenation(child) = el {
                    // inline nested cats
                    for c in child {
                        res.push(flatten_document_elements_once(c).0)
                    }
                    did_change = true;
                    continue;
                } else if let ReverseSpaceEater | CollapsableLineBreak | ForcedLineBreak = el {
                    // remove all trailing spaces
                    while matches!(res.last(), Some(CollapsableSpace | ForcedSpace)) {
                        did_change = true;
                        res.pop();
                    }

                    if let CollapsableLineBreak | ForcedLineBreak = el {
                        // fallthrough to collapsing linebreaks
                    } else {
                        res.push(el);
                        continue;
                    }
                }
                if match el {
                    CollapsableLineBreak => {
                        matches!(res.last(), Some(CollapsableLineBreak | ForcedLineBreak))
                    }
                    CollapsableSpace => {
                        matches!(
                            res.last(),
                            Some(
                                CollapsableSpace
                                    | ForcedSpace
                                    | SpaceEater
                                    | CollapsableLineBreak
                                    | ForcedLineBreak
                            )
                        )
                    }
                    _ => false,
                } {
                    did_change = true;
                    continue;
                }

                let (transformed_el, child_changed) = flatten_document_elements_once(el);
                if child_changed {
                    did_change = true;
                }
                res.push(transformed_el.clone());
            }
            (DocumentElement::Concatenation(res), did_change)
        }
        lb @ DocumentElement::ForcedLineBreak => (lb, false),
        lb @ DocumentElement::CollapsableLineBreak => (lb, false),
        sp @ DocumentElement::ForcedSpace => (sp, false),
        sp @ DocumentElement::CollapsableSpace => (sp, false),
        se @ DocumentElement::SpaceEater => (se, false),
        se @ DocumentElement::ReverseSpaceEater => (se, false),
        DocumentElement::Indentation(child) => {
            if let DocumentElement::Concatenation(body) = *child {
                let is_space = |el: &DocumentElement| {
                    matches!(
                        el,
                        DocumentElement::ForcedLineBreak
                            | DocumentElement::CollapsableLineBreak
                            | DocumentElement::ForcedSpace
                            | DocumentElement::CollapsableSpace
                            | DocumentElement::SpaceEater
                            | DocumentElement::ReverseSpaceEater
                    )
                };

                if body.iter().all(is_space) {
                    // start_spaces and end_spaces would be duplicating the elements in this case
                    return (DocumentElement::Concatenation(body), true);
                }

                let start_spaces = body
                    .iter()
                    .take_while(|el| is_space(el))
                    .cloned()
                    .collect::<Vec<_>>();
                let end_spaces = body
                    .iter()
                    .rev()
                    .take_while(|el| is_space(el))
                    .cloned()
                    .collect::<Vec<_>>()
                    .iter()
                    .rev()
                    .cloned()
                    .collect::<Vec<_>>();

                if start_spaces.is_empty() && end_spaces.is_empty() {
                    // no spaces to move out of the indentation
                    let (new_el, did_change) =
                        flatten_document_elements_once(DocumentElement::Concatenation(body));
                    return (DocumentElement::Indentation(Box::new(new_el)), did_change);
                }

                let res = body
                    .iter()
                    .skip(start_spaces.len())
                    .rev()
                    .skip(end_spaces.len())
                    .rev()
                    .cloned()
                    .collect();

                return (
                    DocumentElement::Concatenation(vec![
                        DocumentElement::Concatenation(start_spaces),
                        DocumentElement::Indentation(Box::new(DocumentElement::Concatenation(res))),
                        DocumentElement::Concatenation(end_spaces),
                    ]),
                    true,
                );
            }

            let (new_child, did_change) = flatten_document_elements_once(*child);

            (
                DocumentElement::Indentation(Box::new(new_child)),
                did_change,
            )
        }
    }
}

pub fn stringify_document(element: &DocumentElement) -> String {
    match element {
        DocumentElement::Concatenation(document_elements) => {
            document_elements.iter().map(stringify_document).collect()
        }
        DocumentElement::Indentation(child) => indent_all_by(4, stringify_document(child)),
        DocumentElement::ForcedSpace => " ".to_string(),
        DocumentElement::CollapsableSpace => " ".to_string(),
        DocumentElement::ForcedLineBreak => "\n".to_string(),
        DocumentElement::CollapsableLineBreak => "\n".to_string(),
        DocumentElement::Text(text) => text.clone(),
        DocumentElement::SpaceEater => "".to_string(),
        DocumentElement::ReverseSpaceEater => "".to_string(),
    }
}

#[cfg(test)]
mod test {
    use crate::document_model::DocumentElement;

    #[test]
    fn empty_spaced_concatenation() {
        assert_eq!(
            DocumentElement::spaced_concatentation(DocumentElement::CollapsableSpace, vec![]),
            DocumentElement::Concatenation(vec![])
        );
    }
}
