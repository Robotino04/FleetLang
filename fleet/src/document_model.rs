use indent::indent_all_by;
use log::info;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocumentElement {
    Concatenation(Vec<DocumentElement>),
    Indentation(Box<DocumentElement>),

    Text(String),

    /// consumes spaces after itself
    SpaceEater,
    /// consumes spaces before itself
    ReverseSpaceEater,

    /// consumes newlines after itself. Also consumes spaces
    LineBreakEater,
    /// consumes newlines before itself. Also consumes spaces
    ReverseLineBreakEater,

    // these two also eat nearby spaces
    ForcedLineBreak,
    CollapsableLineBreak {
        /// The number of line breaks the formatter requires.
        min: usize,

        /// The maximum number of user-provided line breaks that will be preserved.
        max: usize,

        /// The number of line breaks the user has inserted (e.g. via comments or blank lines).
        /// The final number of line breaks will be clamped between [`min`] and [`max`].
        count: usize,
    },

    ForcedSpace,
    CollapsableSpace {
        min: usize,
        max: usize,
        count: usize,
    },
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

    pub fn single_collapsable_space() -> DocumentElement {
        DocumentElement::CollapsableSpace {
            min: 1,
            max: 1,
            count: 0,
        }
    }
    pub fn required_collapsable_linebreak() -> DocumentElement {
        DocumentElement::CollapsableLineBreak {
            min: 1,
            max: 1,
            count: 0,
        }
    }
}

pub fn fully_flatten_document(mut element: DocumentElement) -> DocumentElement {
    let mut i = 0;
    loop {
        let (new_element, did_change) = flatten_document_elements_once(element);

        element = new_element;
        i += 1;
        if !did_change {
            break;
        }
    }

    info!("Took {i} iterations to flatten document");

    element
}

fn consume_spaces(elements: &mut Vec<DocumentElement>) -> bool {
    let mut did_change = false;
    while let Some(DocumentElement::CollapsableSpace { .. } | DocumentElement::ForcedSpace) =
        elements.last()
    {
        elements.pop();
        did_change = true;
    }

    did_change
}

fn consume_linebreaks_and_spaces(elements: &mut Vec<DocumentElement>) -> bool {
    let mut did_change = false;
    while let Some(
        DocumentElement::CollapsableLineBreak { .. }
        | DocumentElement::ForcedLineBreak
        | DocumentElement::CollapsableSpace { .. }
        | DocumentElement::ForcedSpace,
    ) = elements.last()
    {
        elements.pop();
        did_change = true;
    }

    did_change
}

fn flatten_document_elements_once(element: DocumentElement) -> (DocumentElement, bool) {
    match element {
        text @ DocumentElement::Text(_) => (text, false),
        DocumentElement::Concatenation(mut body) => {
            if body.len() == 1 {
                return (body.pop().unwrap(), true);
            }
            let mut res = vec![];
            let mut did_change = false;
            for el in body {
                use DocumentElement::*;
                match el {
                    Concatenation(mut child) => {
                        res.append(&mut child);
                        did_change = true;
                        continue;
                    }
                    ReverseSpaceEater => {
                        did_change = consume_spaces(&mut res) || did_change;
                        res.push(el);
                        continue;
                    }
                    ForcedLineBreak => {
                        did_change = consume_spaces(&mut res) || did_change;

                        if let Some(LineBreakEater) = res.last() {
                            did_change = true;
                        } else {
                            res.push(el);
                        }

                        continue;
                    }
                    ReverseLineBreakEater => {
                        did_change = consume_linebreaks_and_spaces(&mut res) || did_change;

                        res.push(el);
                        continue;
                    }
                    CollapsableLineBreak {
                        min: min1,
                        max: max1,
                        count: count1,
                    } => {
                        did_change = consume_spaces(&mut res) || did_change;

                        if let Some(LineBreakEater) = res.last() {
                            did_change = true;
                            continue;
                        }

                        if let Some(CollapsableLineBreak {
                            min: min2,
                            max: max2,
                            count: count2,
                        }) = res.last_mut()
                        {
                            *min2 = min1.max(*min2);
                            *max2 = max1.max(*max2);
                            *count2 += count1;
                            *count2 = (*max2).min(*count2);

                            did_change = true;
                            continue;
                        } else {
                            res.push(el);
                            continue;
                        }
                    }
                    ForcedSpace => {
                        if let Some(
                            SpaceEater
                            | LineBreakEater
                            | ForcedLineBreak
                            | CollapsableLineBreak { .. },
                        ) = res.last()
                        {
                            did_change = true;
                            continue;
                        }
                    }
                    CollapsableSpace {
                        min: min1,
                        max: max1,
                        count: count1,
                    } => {
                        if let Some(
                            SpaceEater
                            | LineBreakEater
                            | ForcedLineBreak
                            | CollapsableLineBreak { .. },
                        ) = res.last()
                        {
                            did_change = true;
                            continue;
                        }

                        if let Some(CollapsableSpace {
                            min: min2,
                            max: max2,
                            count: count2,
                        }) = res.last_mut()
                        {
                            *min2 = min1.max(*min2);
                            *max2 = max1.max(*max2);
                            *count2 += count1;
                            *count2 = (*max2).min(*count2);

                            did_change = true;
                            continue;
                        } else {
                            res.push(el);
                            continue;
                        }
                    }
                    _ => (),
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
        lb @ DocumentElement::CollapsableLineBreak { .. } => (lb, false),
        sp @ DocumentElement::ForcedSpace => (sp, false),
        sp @ DocumentElement::CollapsableSpace { .. } => (sp, false),
        se @ DocumentElement::SpaceEater => (se, false),
        se @ DocumentElement::ReverseSpaceEater => (se, false),
        le @ DocumentElement::LineBreakEater => (le, false),
        le @ DocumentElement::ReverseLineBreakEater => (le, false),
        DocumentElement::Indentation(child) => {
            if let DocumentElement::Concatenation(body) = *child {
                let is_space = |el: &DocumentElement| {
                    matches!(
                        el,
                        DocumentElement::ForcedLineBreak
                            | DocumentElement::CollapsableLineBreak { .. }
                            | DocumentElement::ForcedSpace
                            | DocumentElement::CollapsableSpace { .. }
                            | DocumentElement::SpaceEater
                            | DocumentElement::ReverseSpaceEater
                            | DocumentElement::LineBreakEater
                            | DocumentElement::ReverseLineBreakEater
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

pub fn stringify_document(element: DocumentElement) -> String {
    let element = fully_flatten_document(element);

    stringify_document_impl(element)
}

pub fn stringify_document_impl(element: DocumentElement) -> String {
    match element {
        DocumentElement::Concatenation(document_elements) => document_elements
            .into_iter()
            .map(stringify_document_impl)
            .collect(),
        DocumentElement::Indentation(child) => indent_all_by(4, stringify_document_impl(*child)),
        DocumentElement::ForcedSpace => " ".to_string(),
        DocumentElement::CollapsableSpace { min, max, count } => {
            " ".repeat(count.max(min).min(max))
        }
        DocumentElement::ForcedLineBreak => "\n".to_string(),
        DocumentElement::CollapsableLineBreak { min, max, count } => {
            "\n".repeat(count.max(min).min(max))
        }
        DocumentElement::Text(text) => text.clone(),
        DocumentElement::SpaceEater => "".to_string(),
        DocumentElement::ReverseSpaceEater => "".to_string(),
        DocumentElement::LineBreakEater => "".to_string(),
        DocumentElement::ReverseLineBreakEater => "".to_string(),
    }
}

#[cfg(test)]
mod test {
    use crate::document_model::{DocumentElement, fully_flatten_document};

    #[test]
    fn empty_spaced_concatenation() {
        assert_eq!(
            DocumentElement::spaced_concatentation(
                DocumentElement::CollapsableSpace {
                    min: 0,
                    max: 5,
                    count: 2
                },
                vec![]
            ),
            DocumentElement::Concatenation(vec![])
        );
    }

    #[test]
    fn collapsable_spaces() {
        assert_eq!(
            fully_flatten_document(DocumentElement::Concatenation(vec![
                DocumentElement::CollapsableSpace {
                    min: 0,
                    max: 5,
                    count: 2
                },
                DocumentElement::CollapsableSpace {
                    min: 0,
                    max: 5,
                    count: 2
                },
            ])),
            DocumentElement::CollapsableSpace {
                min: 0,
                max: 5,
                count: 4
            }
        );
    }

    #[test]
    fn collapsable_spaces_max() {
        assert_eq!(
            fully_flatten_document(DocumentElement::Concatenation(vec![
                DocumentElement::CollapsableSpace {
                    min: 0,
                    max: 5,
                    count: 4
                },
                DocumentElement::CollapsableSpace {
                    min: 0,
                    max: 5,
                    count: 2
                },
            ])),
            DocumentElement::CollapsableSpace {
                min: 0,
                max: 5,
                count: 5
            }
        );
    }

    #[test]
    fn collapsable_linebreak() {
        assert_eq!(
            fully_flatten_document(DocumentElement::Concatenation(vec![
                DocumentElement::CollapsableLineBreak {
                    min: 0,
                    max: 5,
                    count: 2
                },
                DocumentElement::CollapsableLineBreak {
                    min: 0,
                    max: 5,
                    count: 2
                },
            ])),
            DocumentElement::CollapsableLineBreak {
                min: 0,
                max: 5,
                count: 4
            }
        );
    }

    #[test]
    fn collapsable_linebreak_max() {
        assert_eq!(
            fully_flatten_document(DocumentElement::Concatenation(vec![
                DocumentElement::CollapsableLineBreak {
                    min: 0,
                    max: 5,
                    count: 4
                },
                DocumentElement::CollapsableLineBreak {
                    min: 0,
                    max: 5,
                    count: 2
                },
            ])),
            DocumentElement::CollapsableLineBreak {
                min: 0,
                max: 5,
                count: 5
            }
        );
    }

    #[test]
    fn space_eater() {
        assert_eq!(
            fully_flatten_document(DocumentElement::Concatenation(vec![
                DocumentElement::SpaceEater,
                DocumentElement::single_collapsable_space(),
                DocumentElement::ForcedSpace,
                DocumentElement::single_collapsable_space(),
                DocumentElement::single_collapsable_space(),
                DocumentElement::ForcedSpace,
            ])),
            DocumentElement::SpaceEater
        );
    }

    #[test]
    fn reverse_space_eater() {
        assert_eq!(
            fully_flatten_document(DocumentElement::Concatenation(vec![
                DocumentElement::single_collapsable_space(),
                DocumentElement::ForcedSpace,
                DocumentElement::single_collapsable_space(),
                DocumentElement::single_collapsable_space(),
                DocumentElement::ForcedSpace,
                DocumentElement::ReverseSpaceEater,
            ])),
            DocumentElement::ReverseSpaceEater
        );
    }

    #[test]
    fn linebreak_eater() {
        assert_eq!(
            fully_flatten_document(DocumentElement::Concatenation(vec![
                DocumentElement::LineBreakEater,
                DocumentElement::required_collapsable_linebreak(),
                DocumentElement::ForcedLineBreak,
                DocumentElement::required_collapsable_linebreak(),
                DocumentElement::required_collapsable_linebreak(),
                DocumentElement::ForcedLineBreak,
            ])),
            DocumentElement::LineBreakEater,
        );
    }

    #[test]
    fn reverse_linebreak_eater() {
        assert_eq!(
            fully_flatten_document(DocumentElement::Concatenation(vec![
                DocumentElement::required_collapsable_linebreak(),
                DocumentElement::ForcedLineBreak,
                DocumentElement::required_collapsable_linebreak(),
                DocumentElement::required_collapsable_linebreak(),
                DocumentElement::ForcedLineBreak,
                DocumentElement::ReverseLineBreakEater,
            ])),
            DocumentElement::ReverseLineBreakEater,
        );
    }
}
