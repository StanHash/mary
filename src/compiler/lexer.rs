use crate::ir::IntValue;

use super::parser::Token;

use lexgen::lexer;

fn parse_dec(digits: &str) -> IntValue {
    IntValue::from_str_radix(digits, 10).unwrap()
}

fn parse_hex(digits: &str) -> IntValue {
    IntValue::from_str_radix(digits, 16).unwrap()
}

#[derive(Debug, Default)]
pub struct LexerState {
    /// buffer for building string literals
    string_buf: Vec<u8>,
}

lexer! {
    pub Lexer(LexerState) -> Token;

    let dec_digit = ['0'-'9'];
    let hex_digit = $dec_digit | ['a'-'f' 'A'-'F'];

    rule Init {
        /* ignore whitespace */
        $$ascii_whitespace,

        /* keywords */
        "func"    = Token::KwFunc,
        "proc"    = Token::KwProc,
        "const"   = Token::KwConst,
        "script"  = Token::KwScript,
        "var"     = Token::KwVar,
        "if"      = Token::KwIf,
        "else"    = Token::KwElse,
        "for"     = Token::KwFor,
        "do"      = Token::KwDo,
        "while"   = Token::KwWhile,
        "switch"  = Token::KwSwitch,
        "case"    = Token::KwCase,
        "default" = Token::KwDefault,
        "exit"    = Token::KwExit,
        "string"  = Token::KwString,
        "integer" = Token::KwInteger,

        /* punctuation */
        "(" = Token::LParen,
        ")" = Token::RParen,
        "{" = Token::LCurly,
        "}" = Token::RCurly,
        "," = Token::Comma,
        ":" = Token::Colon,
        ";" = Token::Semicolon,

        /* operators */
        "+"  = Token::Plus,
        "-"  = Token::Minus,
        "*"  = Token::Times,
        "/"  = Token::Divide,
        "%"  = Token::Modulus,
        "++" = Token::PlusPlus,
        "--" = Token::MinusMinus,
        "&&" = Token::LAnd,
        "||" = Token::LOr,
        "!"  = Token::Negate,
        "==" = Token::CompareEq,
        "!=" = Token::CompareNe,
        "<"  = Token::CompareLt,
        "<=" = Token::CompareLe,
        ">=" = Token::CompareGe,
        ">"  = Token::CompareGt,
        "="  = Token::Equal,
        "+=" = Token::AddEqual,
        "-=" = Token::SubEqual,
        "*=" = Token::MulEqual,
        "/=" = Token::DivEqual,
        "%=" = Token::ModEqual,

        /* names */

        let name_head = ['a'-'z' 'A'-'Z' '_'];
        let name_tail = $name_head | $dec_digit;

        $name_head $name_tail * => |lexer| lexer.return_(Token::Name(String::from(lexer.match_()))),

        /* integer literals */

        $dec_digit + => |lexer| lexer.return_(Token::Integer(parse_dec(lexer.match_()))),
        "0x" $hex_digit + => |lexer| lexer.return_(Token::Integer(parse_hex(&lexer.match_()[2..]))),

        /* string literals */

        '"' => |lexer| {
            lexer.switch(LexerRule::String)
        },

        /* comments */
        "//" => |lexer| lexer.switch(LexerRule::LineComment),
        "/*" => |lexer| lexer.switch(LexerRule::MultComment),

        /* CPP line markers (treat it as a comment for now) */
        "#" => |lexer| lexer.switch(LexerRule::LineComment),
    }

    rule LineComment {
        (_ # '\n') * ('\n' | $) => |lexer| {
            lexer.reset_match();
            lexer.switch(LexerRule::Init)
        },
    }

    rule MultComment {
        "*/" => |lexer| {
            lexer.reset_match();
            lexer.switch(LexerRule::Init)
        },

        _ => |lexer| lexer.continue_(),
    }

    rule String {
        '"' => |lexer| {
            use std::mem;
            let str_bytes = mem::take(&mut lexer.state().string_buf);
            lexer.switch_and_return(LexerRule::Init, Token::StringLit(str_bytes))
        },

        // TODO: change those a bit to make writing scripts easier

        "\\a" => |lexer| {
            lexer.state().string_buf.push(b'\x07');
            lexer.continue_()
        },

        "\\b" => |lexer| {
            lexer.state().string_buf.push(b'\x08');
            lexer.continue_()
        },

        "\\f" => |lexer| {
            lexer.state().string_buf.push(b'\x0C');
            lexer.continue_()
        },

        "\\n" => |lexer| {
            lexer.state().string_buf.push(b'\n');
            lexer.continue_()
        },

        "\\r" => |lexer| {
            lexer.state().string_buf.push(b'\r');
            lexer.continue_()
        },

        "\\t" => |lexer| {
            lexer.state().string_buf.push(b'\t');
            lexer.continue_()
        },

        "\\v" => |lexer| {
            lexer.state().string_buf.push(b'\x0B');
            lexer.continue_()
        },

        "\\\\" => |lexer| {
            lexer.state().string_buf.push(b'\\');
            lexer.continue_()
        },

        "\\\"" => |lexer| {
            lexer.state().string_buf.push(b'"');
            lexer.continue_()
        },

        "\\\n" => |lexer| {
            lexer.state().string_buf.push(b'\n');
            lexer.continue_()
        },

        "\\x" $hex_digit $hex_digit => |lexer| {
            let m = lexer.match_();
            let byte = u8::from_str_radix(&m[m.len() - 2..], 16).unwrap();
            lexer.state().string_buf.push(byte);
            lexer.continue_()
        },

        _ => |lexer| {
            // NOTE: this probably doesn't work for non ASCII characters?
            // TODO: make this work for those

            let m = lexer.match_();
            let bytes = (&m[m.len() - 1..]).as_bytes();
            lexer.state().string_buf.extend(bytes);
            lexer.continue_()
        },
    }

    // TODO: comments
    // TODO: locations
}
