use std::{str::CharIndices, fmt::{Display, self}};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct FileLocation {
    pub line: usize,
    pub offset: usize,
}

impl Display for FileLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f,"{}:{}",self.line,self.offset)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
    Keyword,
    Id,
    NumberLiteral,
    StringLiteral,
    Punctuation,
    Operator,
    Comment,
}
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Token<'a> {
    pub ttype: TokenType,
    pub text: &'a str,
    pub location: FileLocation,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OwnedToken {
    pub ttype: TokenType,
    pub text: String,
    pub location: FileLocation,
}

impl<'a> From<Token<'a>> for OwnedToken {
    fn from(token: Token<'a>) -> Self {
        OwnedToken { ttype: token.ttype, text: token.text.to_string(), location: token.location }
    }
}

impl<'a> Token<'a> {
    fn to_owned(&self) -> OwnedToken {
        OwnedToken::from(*self)
    }
}

#[derive(Debug)]
pub struct TokenIterator<'a> {
    content: &'a str,
    iter: std::iter::Peekable<CharIndices<'a>>,
    line: usize,
    line_start_index: usize,
}

impl<'a> TokenIterator<'a> {
    pub fn new(content: &'a str) -> Self {
        TokenIterator {
            content,
            iter: content.char_indices().peekable(),
            line: 1,
            line_start_index: 0,
        }
    }
}

const KEYWORDS: [&str; 17] = [ "use", "let", "var", "struct", "fn", "type", "self", "Self", "class", "trait", "true", "false", "if", "else", "while", "for", "match"];

fn token_type_from_alpha_str<'a>(token_str: &'a str) -> TokenType {
    for keyword in &KEYWORDS {
        if token_str == *keyword {
            return TokenType::Keyword
        }
    }
    if token_str.chars().next().unwrap().is_numeric() {
        return TokenType::NumberLiteral
    }
    TokenType::Id
}

//fn is_comment(character: &char, )

const PUNCTUATION: [char; 9] = [ ';', ':', '(', ')', '{', '}', '[', ']', ','];

fn is_punctuation(character: &char) -> bool {
    for sym in &PUNCTUATION {
        if *character == *sym {
            return true;
        }
    }
    false
}

const OPERATORS: [char; 10] = ['=', '+', '-', '*', '/', '&', '^', '|', '<', '>'];

fn is_operator(character: &char) -> bool {
    for sym in &OPERATORS {
        if *character == *sym {
            return true;
        }
    }
    false
}

fn legal_id_char(character: &char) -> bool {
    character.is_alphanumeric() && !is_punctuation(character)
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut alpha_start = None;
        let line = &mut self.line;
        let line_offset = &mut self.line_start_index;

        while let Some((index,character)) = self.iter.peek() {
            let index = *index;
            //print!("{}",index);
            let character = *character;
            if legal_id_char(&character) {
                if alpha_start.is_none() {
                    alpha_start = Some(index);
                }
            } else {
                if let Some(alpha) = alpha_start {
                    let token_str = &self.content[alpha..index];
                    return Some(Token {
                        ttype: token_type_from_alpha_str(token_str),
                        text: token_str,
                        location: FileLocation {
                            line: *line,
                            offset: index - *line_offset,
                        }
                    })
                }
                if is_punctuation(&character) {
                    let _ = self.iter.next();
                    return Some(Token {
                        ttype: TokenType::Punctuation,
                        text: &self.content[index..index+1],
                        location: FileLocation {
                            line: *line,
                            offset: index - *line_offset,
                        }
                    })
                } else if is_operator(&character) {
                    let (_start,op) = self.iter.next().unwrap();
                    if op == '/' {
                        if let Some((_,'/')) = self.iter.peek() {
                            // Comment
                            while let Some(_) = self.iter.next() {
                                if let Some((_,'\n')) = self.iter.peek() {
                                    break;
                                    /*return Some(Token {
                                        ttype: TokenType::Comment,
                                        text: &self.content[index+1..=current.0],
                                        location: FileLocation { line: *line, offset: (index-1) - *line_offset }
                                    })*/
                                }
                            }
                        } else {
                            return Some(Token {
                                ttype: TokenType::Operator,
                                text: &self.content[index..index+1],
                                location: FileLocation {
                                    line: *line,
                                    offset: index - *line_offset,
                                }
                            })
                        }
                    } else {
                        return Some(Token {
                            ttype: TokenType::Operator,
                            text: &self.content[index..index+1],
                            location: FileLocation {
                                line: *line,
                                offset: index - *line_offset,
                            }
                        })
                    }
                } else if character == '\n' {
                    *line_offset = index;
                    *line += 1;
                } else if character == '"' { //start of string literal
                    let lit_start = index;
                    let _ = self.iter.next();
                    while let Some((index,character)) = self.iter.next() {
                        if character == '"' {
                            return Some(Token {
                                ttype: TokenType::StringLiteral,
                                text: &self.content[lit_start..index+1],
                                location: FileLocation {
                                    line: *line,
                                    offset: lit_start - *line_offset,
                                },
                            })
                        }
                    }
                    panic!("Unterminated string literal! (Started at {}:{}",line,lit_start - *line_offset);
                }
            }
            let _ = self.iter.next();
        }
        None
    }
}