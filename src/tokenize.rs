use std::str::CharIndices;

#[derive(Debug, Copy, Clone)]
pub enum TokenType<'a> {
    Keyword(&'a str),
    Id(&'a str),
    NumberLiteral(&'a str),
    StringLiteral(&'a str),
    Punctuation(&'a str),
    Operator(&'a str),
}
#[derive(Debug, Copy, Clone)]
pub struct Token<'a> {
    pub token: TokenType<'a>,
    pub line: usize,
    pub offset: usize,
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

const KEYWORDS: [&str; 10] = [ "use", "let", "var", "struct", "fn", "type", "self", "Self", "class", "trait"];

fn token_type_from_alpha_str<'a>(token_str: &'a str) -> TokenType<'a> {
    for keyword in &KEYWORDS {
        if token_str == *keyword {
            return TokenType::Keyword(token_str)
        }
    }
    if token_str.chars().next().unwrap().is_numeric() {
        return TokenType::NumberLiteral(token_str)
    }
    TokenType::Id(token_str)
}

const PUNCTUATION: [char; 8] = [ ';', ':', '(', ')', '{', '}', '[', ']'];

fn is_punctuation(character: &char) -> bool {
    for sym in &PUNCTUATION {
        if *character == *sym {
            return true;
        }
    }
    false
}

const OPERATORS: [char; 8] = ['=', '+', '-', '*', '/', '&', '^', '|'];

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
                        token: token_type_from_alpha_str(token_str),
                        line: *line,
                        offset: index - *line_offset,
                    })
                }
                if is_punctuation(&character) {
                    let _ = self.iter.next();
                    return Some(Token {
                        token: TokenType::Punctuation(&self.content[index..index+1]),
                        line: *line,
                        offset: index - *line_offset,
                    })
                } else if is_operator(&character) {
                    let _ = self.iter.next();
                    return Some(Token {
                        token: TokenType::Operator(&self.content[index..index+1]),
                        line: *line,
                        offset: index - *line_offset,
                    })
                } else if character == '\n' {
                    *line_offset = index;
                    *line += 1;
                } else if character == '"' { //start of string literal
                    let lit_start = index;
                    let _ = self.iter.next();
                    while let Some((index,character)) = self.iter.next() {
                        if character == '"' {
                            return Some(Token {
                                token: TokenType::StringLiteral(&self.content[lit_start..index+1]),
                                line: *line,
                                offset: lit_start - *line_offset,
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