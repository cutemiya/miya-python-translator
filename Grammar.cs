namespace Translator;

public static class Grammar
{
    public static readonly Rule[] Rules =
    [
        new(
            [
                SymbolType.EOF,
                SymbolType.CalcDefinition,
                SymbolType.VarDefinition,
                SymbolType.I
            ],
            null,
            SymbolType.Program),

        new(
            [
                SymbolType.End,
                SymbolType.AssignmentList,
                SymbolType.Begin
            ],
            null,
            SymbolType.CalcDefinition),

        new(
            [
                SymbolType.Semicolon,
                SymbolType.Logical,
                SymbolType.Colon,
                SymbolType.VarList,
                SymbolType.Var
            ],
            null,
            SymbolType.VarDefinition),

        new(
            [
                SymbolType.Identifier
            ],
            [
                SymbolType.Colon
            ],
            SymbolType.VarList),

        new(
            [
                SymbolType.Identifier
            ],
            [
                SymbolType.RightParen,
                SymbolType.Semicolon
            ],
            SymbolType.VarList),

        new(
            [
                SymbolType.VarList,
                SymbolType.Comma,
                SymbolType.Identifier
            ],
            null,
            SymbolType.VarList),

        new(
            [
                SymbolType.Assignment
            ],
            [
                SymbolType.EndIf
            ],
            SymbolType.AssignmentList),
                
            new(
            [
                SymbolType.Assignment
            ],
            [
                SymbolType.Else
            ],
            SymbolType.AssignmentList),
                
        
        new(
            [
                SymbolType.Assignment
            ],
            [
                SymbolType.Else
            ],
            SymbolType.AssignmentList),

        new(
            [
                SymbolType.Assignment
            ],
            [
                SymbolType.End
            ],
            SymbolType.AssignmentList),

        new(
            [
                SymbolType.AssignmentList,
                SymbolType.Assignment
            ],
            null,
            SymbolType.AssignmentList),

        new(
            [
                SymbolType.Semicolon,
                SymbolType.Logical,
                SymbolType.Colon,
                SymbolType.VarList,
                SymbolType.Var
            ],
            null,
            SymbolType.VarDefinition),

        new(
            [
                SymbolType.Operator
            ],
            null,
            SymbolType.Assignment),

        new(
            [
                SymbolType.Semicolon,
                SymbolType.Expression,
                SymbolType.Equal,
                SymbolType.Identifier
            ],
            null,
            SymbolType.Assignment),

        new(
            [
                SymbolType.ReadStatement
            ],
            null,
            SymbolType.Operator),

        new(
            [
                SymbolType.WriteStatement
            ],
            null,
            SymbolType.Operator),

        new(
            [
                SymbolType.IfStatement
            ],
            null,
            SymbolType.Operator),
                
        new(
            [
                SymbolType.IfStatementWithElse
            ],
            null,
            SymbolType.Operator),

        new(
            [
                SymbolType.RightParen,
                SymbolType.Expression,
                SymbolType.LeftParen
            ],
            null,
            SymbolType.AtomExpression),

        new(
            [
                SymbolType.Operand
            ],
            null,
            SymbolType.AtomExpression),

        new(
            [
                SymbolType.AtomExpression,
                SymbolType.And,
                SymbolType.AndExpression
            ],
            [
                SymbolType.Or
            ],
            SymbolType.AndExpression),

        new(
            [
                SymbolType.AndExpression,
                SymbolType.Or,
                SymbolType.OrExpression
            ],
            [
                SymbolType.Imp
            ],
            SymbolType.OrExpression),

        new(
            [
                SymbolType.OrExpression,
                SymbolType.Imp,
                SymbolType.ImpExpression
            ],
            [
                SymbolType.Imp
            ],
            SymbolType.ImpExpression),

        new(
            [
                SymbolType.AtomExpression,
                SymbolType.And,
                SymbolType.AndExpression
            ],
            [
                SymbolType.Semicolon
            ],
            SymbolType.AndExpression),

        new(
            [
                SymbolType.AndExpression,
                SymbolType.Or,
                SymbolType.OrExpression
            ],
            [
                SymbolType.Semicolon
            ],
            SymbolType.OrExpression),

        new(
            [
                SymbolType.OrExpression,
                SymbolType.Imp,
                SymbolType.ImpExpression
            ],
            [
                SymbolType.Semicolon
            ],
            SymbolType.ImpExpression),

        new(
            [
                SymbolType.AtomExpression,
                SymbolType.And,
                SymbolType.AndExpression
            ],
            [
                SymbolType.RightParen
            ],
            SymbolType.AndExpression),

        new(
            [
                SymbolType.AndExpression,
                SymbolType.Or,
                SymbolType.OrExpression
            ],
            [
                SymbolType.RightParen
            ],
            SymbolType.OrExpression),

        new(
            [
                SymbolType.OrExpression,
                SymbolType.Imp,
                SymbolType.ImpExpression
            ],
            [
                SymbolType.RightParen
            ],
            SymbolType.ImpExpression),

        new(
            [
                SymbolType.AtomExpression,
                SymbolType.And,
                SymbolType.AndExpression
            ],
            [
                SymbolType.Then
            ],
            SymbolType.AndExpression),

        new(
            [
                SymbolType.AndExpression,
                SymbolType.Or,
                SymbolType.OrExpression
            ],
            [
                SymbolType.Then
            ],
            SymbolType.OrExpression),

        new(
            [
                SymbolType.OrExpression,
                SymbolType.Imp,
                SymbolType.ImpExpression
            ],
            [
                SymbolType.Then
            ],
            SymbolType.ImpExpression),

        new(
            [
                SymbolType.ImpExpression,
                SymbolType.Not
            ],
            [
                SymbolType.Semicolon
            ],
            SymbolType.AtomExpression),

        new(
            [
                SymbolType.ImpExpression,
                SymbolType.Not
            ],
            [
                SymbolType.RightParen
            ],
            SymbolType.AtomExpression),

        new(
            [
                SymbolType.ImpExpression,
                SymbolType.Not
            ],
            [
                SymbolType.Then
            ],
            SymbolType.Expression),

        new(
            [
                SymbolType.ImpExpression
            ],
            [
                SymbolType.Semicolon
            ],
            SymbolType.Expression),

        new(
            [
                SymbolType.ImpExpression
            ],
            [
                SymbolType.RightParen
            ],
            SymbolType.Expression),

        new(
            [
                SymbolType.ImpExpression
            ],
            [
                SymbolType.Then
            ],
            SymbolType.Expression),

        new(
            [
                SymbolType.Identifier
            ],
            [
                SymbolType.RightParen
            ],
            SymbolType.Operand),

        new(
            [
                SymbolType.Identifier
            ],
            [
                SymbolType.Semicolon
            ],
            SymbolType.Operand),

        new(
            [
                SymbolType.Identifier
            ],
            [
                SymbolType.Imp
            ],
            SymbolType.Operand),

        new(
            [
                SymbolType.Identifier
            ],
            [
                SymbolType.Or
            ],
            SymbolType.Operand),

        new(
            [
                SymbolType.Identifier
            ],
            [
                SymbolType.And
            ],
            SymbolType.Operand),

        new(
            [
                SymbolType.Identifier
            ],
            [
                SymbolType.Then
            ],
            SymbolType.Operand),

        new(
            [
                SymbolType.Boolean
            ],
            null,
            SymbolType.Operand),

        new(
            [
                SymbolType.Semicolon,
                SymbolType.RightParen,
                SymbolType.VarList,
                SymbolType.LeftParen,
                SymbolType.Read
            ],
            null,
            SymbolType.ReadStatement),

        new(
            [
                SymbolType.Semicolon,
                SymbolType.RightParen,
                SymbolType.VarList,
                SymbolType.LeftParen,
                SymbolType.Write
            ],
            null,
            SymbolType.WriteStatement),

        new(
            [
                SymbolType.Semicolon,
                SymbolType.EndIf,
                SymbolType.AssignmentList,
                SymbolType.Then,
                SymbolType.Expression,
                SymbolType.If
            ],
            null,
            SymbolType.WriteStatement),
                
        new(
            [
                SymbolType.Semicolon,
                SymbolType.EndIf,
                SymbolType.AssignmentList,
                SymbolType.Else,
                SymbolType.AssignmentList,
                SymbolType.Then,
                SymbolType.Expression,
                SymbolType.If
            ],
            null,
            SymbolType.WriteStatement)
    ];

    public static readonly SymbolType[][] ShiftRules =
    [
        [
            SymbolType.I,
            SymbolType.Var
        ],

        [
            SymbolType.Logical,
            SymbolType.Semicolon,
            SymbolType.Begin
        ],

        [
            SymbolType.Semicolon,
            SymbolType.Begin
        ],

        [
            SymbolType.Semicolon,
            SymbolType.Read
        ],

        [
            SymbolType.Semicolon,
            SymbolType.Write
        ],

        [
            SymbolType.Semicolon,
            SymbolType.If
        ],

        [
            SymbolType.Semicolon,
            SymbolType.End
        ],

        [
            SymbolType.Semicolon,
            SymbolType.EndIf
        ],
        [
        SymbolType.Semicolon,
        SymbolType.Else
        ],

        [
            SymbolType.Semicolon,
            SymbolType.Identifier
        ],

        [
            SymbolType.Colon,
            SymbolType.Logical
        ],

        [
            SymbolType.Comma,
            SymbolType.Identifier
        ],

        [
            SymbolType.Equal,
            SymbolType.Not
        ],

        [
            SymbolType.Equal,
            SymbolType.LeftParen
        ],

        [
            SymbolType.Equal,
            SymbolType.Identifier
        ],

        [
            SymbolType.Equal,
            SymbolType.Boolean
        ],

        [
            SymbolType.LeftParen,
            SymbolType.Not
        ],

        [
            SymbolType.LeftParen,
            SymbolType.LeftParen
        ],

        [
            SymbolType.LeftParen,
            SymbolType.Identifier
        ],

        [
            SymbolType.LeftParen,
            SymbolType.Boolean
        ],

        [
            SymbolType.RightParen,
            SymbolType.Semicolon
        ],

        [
            SymbolType.RightParen,
            SymbolType.Imp
        ],

        [
            SymbolType.RightParen,
            SymbolType.Or
        ],

        [
            SymbolType.RightParen,
            SymbolType.And
        ],

        [
            SymbolType.RightParen,
            SymbolType.Then
        ],

        [
            SymbolType.Begin,
            SymbolType.Read
        ],

        [
            SymbolType.Begin,
            SymbolType.Write
        ],

        [
            SymbolType.Begin,
            SymbolType.If
        ],

        [
            SymbolType.Begin,
            SymbolType.Identifier
        ],

        [
            SymbolType.Var,
            SymbolType.Identifier
        ],

        [
            SymbolType.End,
            SymbolType.EOF
        ],

        [
            SymbolType.Not,
            SymbolType.LeftParen
        ],

        [
            SymbolType.Not,
            SymbolType.Identifier
        ],

        [
            SymbolType.Not,
            SymbolType.Boolean
        ],

        [
            SymbolType.Imp,
            SymbolType.LeftParen
        ],

        [
            SymbolType.Imp,
            SymbolType.Identifier
        ],

        [
            SymbolType.Imp,
            SymbolType.Boolean
        ],

        [
            SymbolType.Imp,
            SymbolType.Not
        ],

        [
            SymbolType.Or,
            SymbolType.LeftParen
        ],

        [
            SymbolType.Or,
            SymbolType.Identifier
        ],

        [
            SymbolType.Or,
            SymbolType.Boolean
        ],
        
        [
            SymbolType.Or,
            SymbolType.Not
        ],

        [
            SymbolType.And,
            SymbolType.LeftParen
        ],

        [
            SymbolType.And,
            SymbolType.Identifier
        ],

        [
            SymbolType.And,
            SymbolType.Boolean
        ],

        [
            SymbolType.And,
            SymbolType.Not
        ],

        [
            SymbolType.Read,
            SymbolType.LeftParen,
            SymbolType.Identifier
        ],

        [
            SymbolType.Write,
            SymbolType.LeftParen,
            SymbolType.Identifier
        ],

        [
            SymbolType.If,
            SymbolType.Not
        ],

        [
            SymbolType.If,
            SymbolType.LeftParen
        ],

        [
            SymbolType.If,
            SymbolType.Identifier
        ],

        [
            SymbolType.If,
            SymbolType.Boolean
        ],

        [
            SymbolType.Then,
            SymbolType.If
        ],

        [
            SymbolType.Then,
            SymbolType.Not
        ],

        [
            SymbolType.Then,
            SymbolType.LeftParen
        ],

        [
            SymbolType.Then,
            SymbolType.Write
        ],

        [
            SymbolType.Then,
            SymbolType.Read
        ],

        [
            SymbolType.Then,
            SymbolType.Identifier
        ],

        [
            SymbolType.Then,
            SymbolType.Boolean
        ],

        [
            SymbolType.EndIf,
            SymbolType.Semicolon
        ],

        [
            SymbolType.Else,
            SymbolType.Read
        ],

        [
             SymbolType.Else,
             SymbolType.Expression
        ],

        [
             SymbolType.Else,
             SymbolType.Identifier
        ],

        [
            SymbolType.Identifier,
            SymbolType.Comma
        ],

        [
            SymbolType.Identifier,
            SymbolType.Equal
        ],

        [
            SymbolType.Identifier,
            SymbolType.Imp
        ],

        [
            SymbolType.Identifier,
            SymbolType.Or
        ],

        [
            SymbolType.Identifier,
            SymbolType.And
        ],

        [
            SymbolType.Identifier,
            SymbolType.Colon
        ],

        [
            SymbolType.Identifier,
            SymbolType.Semicolon
        ],

        [
            SymbolType.Identifier,
            SymbolType.RightParen
        ],

        [
            SymbolType.Identifier,
            SymbolType.Then
        ],

        [
            SymbolType.Boolean,
            SymbolType.Imp
        ],

        [
            SymbolType.Boolean,
            SymbolType.Or
        ],

        [
            SymbolType.Boolean,
            SymbolType.And
        ],

        [
            SymbolType.Boolean,
            SymbolType.Semicolon
        ],

        [
            SymbolType.Boolean,
            SymbolType.RightParen
        ],

        [
            SymbolType.EOF
        ]
    ];
}