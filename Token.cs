namespace Translator;

public class Token(SymbolType type, string lexeme, bool literal, int line, IEnumerable<Token> children = null)
{
    public readonly SymbolType Type = type;
    public readonly string Lexeme = lexeme;
    public readonly bool Literal = literal;
    public readonly int Line = line;
    public readonly List<Token> Children = children == null ? null : [..children];

    public override string ToString()
    {
        return $"{Type}: {Lexeme}";
    }
}