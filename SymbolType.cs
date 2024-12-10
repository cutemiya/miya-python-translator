namespace Translator;

public enum SymbolType
{
    // Терминальные символы
    Var,
    Logical,
    Begin,
    End,
    If,
    Then,
    Else,
    EndIf,
    Read,
    Write,
    Colon,
    Semicolon,
    Equal,
    LeftParen,
    RightParen,
    Comma,
    Or,
    And,
    Imp,
    Not,
    Identifier,
    Boolean,
    EOF,

    // Нетерминальные символы
    I,
    Program,
    VarDefinition,
    CalcDefinition,
    AssignmentList,
    VarList,
    Assignment,
    Operator,
    Expression,
    ImpExpression,
    OrExpression,
    AndExpression,
    AtomExpression,
    Operand,
    ReadStatement,
    WriteStatement,
    IfStatement,
    IfStatementWithElse
}