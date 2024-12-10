using System.Text;

namespace Translator;

internal static class Program
{
    private static bool _hadError;
    private static bool _hadRuntimeException;
    private static List<string> _errors = new();

    public static void Main(string[] args)
    {
        if (args.Length > 1)
        {
            Console.WriteLine("Usage: Translator [script]");
            Environment.Exit(1);
        }

        if (args.Length == 1)
        {
            var fileInfo = IoHelper.GetFileInfo(args[0]);
            if (!IoHelper.Validate(fileInfo))
            {
                Console.WriteLine("File format is incorrect");
                Environment.Exit(-2);
            }

            RunFile(args[0], fileInfo);
        }
        else
        {
            RunPrompt();
        }
    }

    private static void RunFile(string path, IoHelper.FileInfo info)
    {
        Run(IoHelper.GetFileData(path), info.fileName);
        if (_hadError || _hadRuntimeException)
        {
            Environment.Exit(1);
        }
    }

    private static void RunPrompt()
    {
        while (true)
        {
            Console.Write("> ");

            var line = Console.ReadLine();
            if (line == null)
            {
                break;
            }

            Run(line);
            _hadError = false;
        }
    }

    private static void Run(string source, string fileName = "main.py")
    {
        if (string.IsNullOrEmpty(fileName))
            fileName = "main";
        
        var scanner = new Scanner(source);
        var tokens = scanner.ScanTokens();
        var parser = new Parser(tokens);
        var tree = parser.Parse();

        string code;
        try
        {
            code = new CodeGenerator(tree)
                .Init()
                .Generate()
                .GetCode();
        }
        catch (Exception)
        {
            _hadError = true;
            IoHelper.WriteToFile(_errors.Aggregate("", (acc, s) => $"{acc}{Environment.NewLine}{s}"), fileName.ToLogFormat());
            IoHelper.MakeFinalMessage(fileName, true, _errors);

            return;
        }

        var lexicalResult = tokens
            .Aggregate(string.Empty, (acc, t) => $"{acc}\n{t}");
        var treeMessage = GetTree(tree);
        
        IoHelper.WriteToFile(code, fileName.ToPyFormat());
        IoHelper.WriteToFile(lexicalResult, fileName.ToLexicalFormat());
        IoHelper.WriteToFile(treeMessage, fileName.ToTreeFormat());

        IoHelper.RunPythonScript(fileName.ToPyFormat());
        
        IoHelper.MakeFinalMessage(fileName, false);
        
        if (_hadError)
        {
        }
    }


    private delegate string RepDel(int i);

    private static string GetTree(Token token)
    {
        const int dept = 0;
        var result = new StringBuilder();
        Print(token, dept, Repeat, result);

        var str = string.Join("\r\n",
            result.ToString().Split(new[] { "\r\n" }, StringSplitOptions.RemoveEmptyEntries));

        return str;

        string Repeat(int i)
        {
            var res = "";
            for (var j = 0; j < i; j++)
            {
                res += " ";
            }

            return res;
        }
    }

    private static StringBuilder Print(Token token, int dept, RepDel call, StringBuilder result)
    {
        if (token == null)
        {
            return result;
        }

        if (token.Type != SymbolType.EOF)
        {
            result.AppendLine($"{call(dept)}<{token.Type}/>");
        }

        if (token.Children == null || token.Children.Count == 0) return result;
        dept += 1;

        token.Children.Reverse();
        return token.Children.Aggregate(result, (current, child) => Print(child, dept, call, current));
    }

    public static void Error(int line, string message)
    {
        Report(line, "", message);
    }

    private static void Report(int line, string where, string message)
    {
        _errors.Add($"[строка {line}] Ошибка{where}: {message}");
        _hadError = true;
    }

    public static void Error(Token token, string message)
    {
        if (token.Type == SymbolType.EOF)
        {
            Report(token.Line, " в конце", message);
        }
        else
        {
            Report(token.Line, $" в '{token.Lexeme}'", message);
        }
    }
}