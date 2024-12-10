using System.Net.Mime;
using System.Runtime.InteropServices.JavaScript;
using Translator.Exceptions;

namespace Translator;

public class Scanner(string source)
{
    private readonly List<Token> _tokens = [];
    private int _start;
    private int _current;
    private int _line = 1;

    private const int MaxIdentifierLength = 8;

    private static readonly Dictionary<string, SymbolType> Keywords = new()
    {
        { "VAR", SymbolType.Var },
        { "LOGICAL", SymbolType.Logical },
        { "BEGIN", SymbolType.Begin },
        { "END", SymbolType.End },
        { "IF", SymbolType.If },
        { "THEN", SymbolType.Then },
        { "ELSE", SymbolType.Else },
        { "END_IF", SymbolType.EndIf },
        { "READ", SymbolType.Read },
        { "WRITE", SymbolType.Write }
    };

    private static readonly Dictionary<string, SymbolType> Operators = new()
    {
        { ".OR.", SymbolType.Or },
        { ".AND.", SymbolType.And },
        { ".IMP.", SymbolType.Imp },
        { ".NOT.", SymbolType.Not }
    };

    public List<Token> ScanTokens()
    {
        _tokens.Add(new Token(SymbolType.I, "", false, _line));

        while (!IsAtEnd())
        {
            _start = _current;
            ScanToken();
        }

        _tokens.Add(new Token(SymbolType.EOF, "", false, _line));
        return _tokens;
    }

    private void ScanToken()
    {
        var c = Advance();
        switch (c)
        {
            case ':':
                AddToken(SymbolType.Colon);
                break;

            case ';':
                AddToken(SymbolType.Semicolon);
                break;

            case '=':
                AddToken(SymbolType.Equal);
                break;

            case '(':
                AddToken(SymbolType.LeftParen);
                break;

            case ')':
                AddToken(SymbolType.RightParen);
                break;

            case ',':
                AddToken(SymbolType.Comma);
                break;

            case '0':
                AddToken(SymbolType.Boolean, false);
                break;

            case '1':
                AddToken(SymbolType.Boolean, true);
                break;

            case '.':
                Operator();
                break;

            case '/':
                if (Match('/'))
                {
                    while (Peek() != '\n' && !IsAtEnd())
                    {
                        Advance();
                    }
                }
                else
                {
                    Program.Error(_line, $"Недопустимый символ: {c}");
                }

                break;

            case ' ':
            case '\r':
            case '\t':
                break;

            case '\n':
                _line++;
                break;

            default:
                if (IsAlpha(c))
                {
                    var error = Identifier();
                    if (!string.IsNullOrEmpty(error))
                    {
                        Program.Error(_start, error);
                    }
                }
                else
                {
                    Program.Error(_line, $"Недопустимый символ : {c}");
                }
                
                break;
        }
    }

    private bool IsAtEnd() => _current >= source.Length;

    private char Advance() => source[_current++];

    private void AddToken(SymbolType type, bool literal = false)
    {
        var text = source.Substring(_start, _current - _start);
        _tokens.Add(new Token(type, text, literal, _line));
    }

    private bool Match(char expected)
    {
        if (IsAtEnd())
        {
            return false;
        }

        if (source[_current] != expected)
        {
            return false;
        }

        _current++;
        return true;
    }

    private char Peek()
    {
        return IsAtEnd() ? '\0' : source[_current];
    }

    private bool IsAlpha(char c) => c is >= 'a' and <= 'z' or >= 'A' and <= 'Z' or '_';

    private string Identifier()
    {
        while (IsAlpha(Peek()))
        {
            Advance();
        }
        

        var text = source.Substring(_start, _current - _start);
        var ok = Keywords.TryGetValue(text, out var type);
        if (!ok)
        {
            type = SymbolType.Identifier;

            if (text.Length > MaxIdentifierLength)
            {
                    return $"Длина идентификатора не может превышать {MaxIdentifierLength} символов";
            }
        }

        AddToken(type);
        
        return string.Empty;
    }

    private void Operator()
    {
        while (IsAlpha(Peek()))
        {
            Advance();
        }

        if (Peek() == '.')
        {
            Advance();
        }

        var text = source.Substring(_start, _current - _start);
        var ok = Operators.TryGetValue(text, out var type);
        if (!ok)
        {
            Program.Error(_line, $"Недопустимый оператор: {text}");
            return;
        }

        AddToken(type);
    }
}