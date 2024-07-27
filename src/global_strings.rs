use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    hash::Hash,
};

thread_local!(static STRINGS: RefCell<Option<Vec<String>>> = RefCell::new(None));

#[derive(Clone, Copy, Eq, Ord, Default)]
#[repr(transparent)]
pub struct GlobalString(usize);

impl Hash for GlobalString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl GlobalString {
    pub fn with<R, F>(&self, func: F) -> R
    where
        F: FnOnce(&str) -> R,
    {
        STRINGS.with_borrow_mut(|strings| match strings {
            None => {
                *strings = Some(Vec::new());
                func("")
            }
            Some(strings) => {
                if let Some(string) = strings.get(self.0) {
                    func(string)
                } else {
                    func("")
                }
            }
        })
    }

    pub fn get_string(&self) -> String {
        STRINGS.with_borrow(|strings| match strings {
            None => String::new(),
            Some(strings) => {
                if let Some(string) = strings.get(self.0) {
                    string.clone()
                } else {
                    String::new()
                }
            }
        })
    }

    pub fn new(string: &String) -> Self {
        match find_str(string) {
            Some(index) => Self(index),
            _ => Self(insert_str(string.to_string())),
        }
    }

    pub fn from_static(string: &str) -> Self {
        match find_str(string) {
            Some(index) => Self(index),
            _ => Self(insert_str(string.to_string())),
        }
    }

    pub fn from_str(string: String) -> Self {
        match find_str(&string) {
            Some(index) => Self(index),
            _ => Self(insert_str(string)),
        }
    }
}

fn find_str(str: &str) -> Option<usize> {
    STRINGS.with_borrow(|strings| strings.as_ref()?.iter().position(|val| val == str))
}

fn insert_str(string: String) -> usize {
    STRINGS.with_borrow_mut(move |strings| {
        let strings = match strings {
            Some(v) => v,
            None => {
                let vec = Vec::new();
                *strings = Some(vec);
                &mut strings.as_mut().unwrap()
            }
        };
        strings.push(string);
        strings.len() - 1
    })
}

impl Debug for GlobalString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.with(|v| f.write_str(v))
    }
}

impl Display for GlobalString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.with(move |v| f.write_str(v))
    }
}

impl PartialEq for GlobalString {
    fn eq(&self, other: &Self) -> bool {
        if self.0 == other.0 {
            true
        } else {
            STRINGS.with_borrow(|strings| {
                match strings {
                    None => false,
                    Some(v) => {
                        match (v.get(self.0), v.get(other.0)) {
                            (Some(str_self), Some(str_other)) => str_self.eq(str_other),
                            _ => false
                        }
                    }
                }
            })
        }
    }
}

impl PartialOrd for GlobalString {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.0 == other.0 {
            Some(std::cmp::Ordering::Equal)
        } else {
            STRINGS.with_borrow(|strings| {
                match strings {
                    None => None,
                    Some(v) => {
                        match (v.get(self.0), v.get(other.0)) {
                            (Some(str_self), Some(str_other)) => str_self.partial_cmp(str_other),
                            _ => None
                        }
                    }
                }
            })
        }
    }
}
