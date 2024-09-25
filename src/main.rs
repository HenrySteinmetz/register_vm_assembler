use std::{
    fs::File,
    io::{Read, Write},
    path::PathBuf,
    str::FromStr,
};

use clap::Parser;

enum Error {
    RegisterIndexOutOfBounds(u8),
}

type LineNumber = u32;
type SectionNumber = u32;

#[derive(strum::EnumString, strum::EnumIter)]
#[repr(u8)]
pub enum OpCode {
    STOP = 0,
    LOAD = 1,
    ADD = 2,
    SUB = 3,
    MUL = 4,
    DIV = 5,
    PRINT = 6,
    JMP = 7,
    JMPB = 8,
    JMPF = 9,
    JMPE = 10,
    CL = 11,
    JL = 12,
    JLE = 13,
    JLNE = 14,
    INC = 15,
    DEC = 16,
    IGL = 254,
    NOP = 255,
}

#[derive(Debug)]
struct Token {
    position: (LineNumber, SectionNumber),
    value: TokenValue,
}

#[derive(Debug)]
enum TokenValue {
    Literal(Literal),
    RegisterIndex(RegisterIndex),
    RegisterValue(RegisterIndex),
}

impl PartialEq<OperandType> for TokenValue {
    fn eq(&self, other: &OperandType) -> bool {
        match (self, other) {
            (TokenValue::Literal(x), OperandType::Literal(y)) => x == y,
            (TokenValue::RegisterIndex(_), OperandType::RegisterIndex) => true,
            (TokenValue::RegisterValue(_), OperandType::RegisterValue) => true,
            (_, OperandType::Any) => true,
            _ => false,
        }
    }
}

impl PartialEq<LiteralType> for Literal {
    fn eq(&self, other: &LiteralType) -> bool {
        match (self, other) {
            (Literal::Int(_), LiteralType::Int) => true,
            (Literal::Float(_), LiteralType::Float) => true,
            (Literal::String(_), LiteralType::String) => true,
            (Literal::Bool(_), LiteralType::Bool) => true,
            (_, LiteralType::Any) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
struct RegisterIndex(u8);

impl TryFrom<u8> for RegisterIndex {
    type Error = Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0..=31 => Ok(RegisterIndex(value)),
            _ => Err(Error::RegisterIndexOutOfBounds(value)),
        }
    }
}

#[derive(Debug)]
enum Literal {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Clone, Debug)]
enum OperandType {
    RegisterIndex,
    Literal(LiteralType),
    RegisterValue,
    Any,
}

#[derive(Clone, Debug)]
enum LiteralType {
    String,
    Int,
    Any,
    Float,
    Bool,
}

impl OpCode {
    fn expected_operands(&self) -> Vec<OperandType> {
        use OpCode::*;
        use OperandType::*;
        match self {
            STOP | NOP => Vec::new(),
            LOAD => vec![RegisterIndex, Literal(LiteralType::Any)],
            ADD | SUB | MUL | DIV => vec![RegisterIndex, Any, Any],
            PRINT => vec![Any],
            JMP | JMPB | JMPF => vec![Literal(LiteralType::Int)],
            JMPE => vec![Literal(LiteralType::Int), Any, Any],
            CL | JL | IGL => vec![Literal(LiteralType::String)],
            JLE | JLNE => vec![Literal(LiteralType::String), Any, Any],
            INC | DEC => vec![RegisterIndex],
        }
    }
}

#[derive(clap::Parser, Debug)]
struct Cli {
    input_file: PathBuf,

    #[arg(short)]
    output_location: PathBuf,
}

fn main() {
    let cli = Cli::parse();

    if cli.output_location.exists() {
        panic!("Output location already exists! This operation would overwrite the existing file in that location!");
    }

    if !cli.input_file.exists() {
        panic!("Input file not found!")
    }

    if !cli.input_file.is_file() {
        panic!("Provided input location is not a file!");
    }

    let mut file_contents = String::new();

    std::fs::File::open(cli.input_file)
        .expect("Couldn't open input file!")
        .read_to_string(&mut file_contents)
        .expect("Read of input file failed!");

    let mut program_buffer = Vec::new();

    for (line_num, line) in file_contents.lines().enumerate() {
        println!("Line: {}", line);
        let tokens = line.split(' ');

        let string_starts = tokens.clone().filter(|t| t.starts_with('\"'));
        let string_ends = tokens.clone().filter(|t| t.ends_with('\"'));

        let mut parsed_tokens = vec![].into_iter();

        println!(
            "\tOpening tokens: {:?}, \n\tClosing tokens: {:?}",
            &string_starts.clone().collect::<Vec<&str>>(),
            &string_ends.clone().collect::<Vec<&str>>()
        );

        let num_string_starts = string_starts.clone().count();
        let num_string_ends = string_ends.clone().count();

        if num_string_starts < num_string_ends {
            panic!("Line: {}\t Missing opening quotation mark!", line_num);
        }

        if num_string_ends < num_string_starts {
            panic!("Line: {}\t Missing closing quotation mark!", line_num);
        }

        if num_string_starts == 0 {
            parsed_tokens = tokens
                .clone()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .into_iter();
        }

        let string_literals = string_starts.zip(string_ends);

        for (start, end) in string_literals {
            let mut temp_arr: Vec<String> = tokens.clone().map(|x| x.to_string()).collect();

            let start_idx = temp_arr
                .iter()
                .enumerate()
                .position(|t| t.1 == &start)
                .unwrap();

            let end_idx = temp_arr
                .iter()
                .enumerate()
                .position(|t| t.1 == &end)
                .unwrap();

            let mut string_literal: Vec<&str> = vec![];

            for i in start_idx..=end_idx {
                string_literal.push(&temp_arr[i]);
            }

            let str = string_literal.join("");

            temp_arr[start_idx] = str;

            for i in start_idx + 1..=end_idx {
                temp_arr.remove(i);
            }

            parsed_tokens = temp_arr.into_iter();
        }

        println!(
            "Tokens: {:?}\n",
            parsed_tokens.clone().collect::<Vec<String>>()
        );

        let opcode = OpCode::from_str(
            &parsed_tokens
                .next()
                .expect(format!("No opcode provided in line {}", line_num).as_str()),
        )
        .expect(format!("Unknown OpCode in line {}", line_num).as_str());

        let operands: Vec<Token> = parsed_tokens
            .enumerate()
            .map(|x| parse_operand(&x.1, (line_num as u32, x.0 as u32)))
            .collect();

        let expected_operands = opcode.expected_operands();

        if operands.len() != expected_operands.len() {
            panic!(
                "Line: {}\t Expected {} operands, got {}",
                line_num,
                expected_operands.len(),
                operands.len()
            );
        }

        for (ex, op) in expected_operands.iter().zip(operands.iter()) {
            if op.value != *ex {
                panic!(
                    "Line: {}\t Expected operand type {:?} , got {:?}",
                    line_num, ex, op
                );
            }
        }

        let mut program_bytes: Vec<u8> = Vec::new();

        program_bytes.push(opcode as u8);

        for op in operands {
            program_bytes.append(&mut op.value.serialize());
        }

        program_buffer.append(&mut program_bytes);
    }

    File::create(&cli.output_location)
        .expect("Failed to create output location!")
        .write(program_buffer.as_slice())
        .expect("Failed to write the programm code to the output file!");
}

impl TokenValue {
    fn serialize(&self) -> Vec<u8> {
        match self {
            TokenValue::RegisterIndex(idx) => vec![0, idx.0],
            TokenValue::RegisterValue(v) => vec![5, v.0],
            TokenValue::Literal(l) => l.serialize(),
        }
    }
}

impl Literal {
    fn serialize(&self) -> Vec<u8> {
        match self {
            Literal::Int(int) => {
                let mut ret: Vec<u8> = vec![1];
                let mut int_bytes = int.to_le_bytes().to_vec();
                ret.append(&mut int_bytes);
                ret
            }
            Literal::Float(float) => {
                let mut ret: Vec<u8> = vec![2];
                let mut float_bytes = float.to_le_bytes().to_vec();
                ret.append(&mut float_bytes);
                ret
            }
            Literal::String(str) => {
                let mut ret: Vec<u8> = vec![3];
                let mut bytes = str.bytes().collect::<Vec<u8>>();
                let mut len = bytes.len().to_le_bytes().to_vec();

                ret.append(&mut len);
                ret.append(&mut bytes);
                ret
            }
            Literal::Bool(bool) => vec![4, if *bool { 1 } else { 0 }],
        }
    }
}

fn parse_operand(token: &str, position: (LineNumber, SectionNumber)) -> Token {
    if token.starts_with('#') {
        let mut int = token.chars();
        int.next();
        let idx = RegisterIndex::try_from(
            int.as_str().parse::<u8>().expect(
                format!(
                    "Line: {}, Section: {}\tCouldn't parse {} as register value.",
                    position.0,
                    position.1,
                    int.as_str()
                )
                .as_str(),
            ),
        );
        let value = match idx {
            Ok(v) => v,
            Err(_e) => panic!("Line: {}, Section: {}\t Register index is out of bounds. There are only 32 registers!", position.0, position.1)
        };
        return Token {
            position,
            value: TokenValue::RegisterIndex(value),
        };
    }

    if token.starts_with('$') {
        let mut int = token.chars();
        int.next();
        let idx = RegisterIndex::try_from(
            int.as_str().parse::<u8>().expect(
                format!(
                    "Line: {}, Section: {}\tCouldn't parse `{}` as a register index.",
                    position.0,
                    position.1,
                    int.as_str()
                )
                .as_str(),
            ),
        );
        let value = match idx {
            Ok(v) => v,
            Err(_e) => panic!("Line: {}, Section: {}\tCouldn't parse `{}` as a register index while trying to get a register value.", position.0, position.1, int.as_str())
        };
        return Token {
            position,
            value: TokenValue::RegisterValue(value),
        };
    }

    if token.starts_with('\"') && token.ends_with('\"') {
        let mut string = token.chars();
        string.next();
        string.next_back();

        let value = TokenValue::Literal(Literal::String(string.collect::<String>()));

        return Token { position, value };
    }

    if token.starts_with('\"') {
        panic!(
            "Line: {}, Section: {}\tString literal `{}` does not have a closing quotation mark!",
            position.0, position.1, token
        );
    }

    if token.contains('.') {
        let float = token.parse::<f64>().expect(
            format!(
                "Line: {}, Section: {}\t Couldn't parse `{}` as a float!",
                position.0, position.1, token
            )
            .as_str(),
        );

        let value = TokenValue::Literal(Literal::Float(float));
        return Token { position, value };
    }

    if token == "true" {
        let value = TokenValue::Literal(Literal::Bool(true));
        return Token { position, value };
    }

    if token == "false" {
        let value = TokenValue::Literal(Literal::Bool(false));
        return Token { position, value };
    }

    let int = token.parse::<i64>().expect(
        format!(
            "Line: {}, Section: {}\t Unknown token type or illegal integer `{}`!",
            position.0, position.1, token
        )
        .as_str(),
    );

    let value = TokenValue::Literal(Literal::Int(int));
    Token { position, value }
}
