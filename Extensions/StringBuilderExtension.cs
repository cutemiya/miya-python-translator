using System.Text;

namespace Translator.Extensions;

public static class StringBuilderExtension
{
    private const string VarDeclarationTitle = "# Блок с переменными";
    private const string MainDeclarationTitle = "# Основная программа\r\nprint(\"Вводите 1 или 0\")";

    public static StringBuilder AddDefault(this StringBuilder sb)
    {
        return sb;
    }

    public static StringBuilder AddVarDeclaration(this StringBuilder sb, IEnumerable<Token> tokens) =>
        tokens
            .Aggregate(sb, (acc, c) => acc.AddIdentifier(c.Lexeme));

    private static StringBuilder AddIdentifier(this StringBuilder sb, string identName) =>
        sb.AppendLine($"{identName}: bool");

    public static StringBuilder AddVarDeclarationTitle(this StringBuilder sb) => sb.AppendLine(VarDeclarationTitle);
    public static StringBuilder AddMainDeclarationTitle(this StringBuilder sb) => sb.AppendLine(MainDeclarationTitle);
}