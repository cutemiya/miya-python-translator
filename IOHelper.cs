using System.Diagnostics;
using System.Text;
using FileNotFoundException = System.IO.FileNotFoundException;

namespace Translator;

public static class IoHelper
{
    public record FileInfo(string fileName, string format);


    private const string DirectoryPath = "result";
    private const string FileType = ".miya";

    public static FileInfo GetFileInfo(string arg) => new FileInfo(
        arg
            .Split(Path.GetExtension(arg).ToLower())
            .FirstOrDefault()!
            .Replace(@".\", ""),
        Path
            .GetExtension(arg)
            .ToLower()
    );

    public static string GetFileData(string path)
    {
        try
        {
            return File.ReadAllText(path);
        }
        catch (FileNotFoundException)
        {
            Console.WriteLine("Такого файла нет в дирректории");
            Environment.Exit(-3);
        } catch (Exception e)
        {
            Console.WriteLine($"Непредвиденная ошибка при чтении файла: {e.Message}");
            Environment.Exit(-500);
        }

        return null;
    }

    public static void MakeFinalMessage(string fileName, bool hasError, List<string> errors = null)
    {
        Console.Write("Результат:");
        
        if (hasError)
        {
            Console.ForegroundColor = ConsoleColor.Red;
            Console.Write("провал");
            Console.WriteLine();
            
            errors?.ForEach(Console.WriteLine);
            Console.WriteLine();

            Console.ForegroundColor = ConsoleColor.DarkCyan;
            Console.WriteLine($"Ошибка сохранена в файл.");
            Console.WriteLine();
            
            Console.ForegroundColor = ConsoleColor.DarkMagenta;
            Console.WriteLine($"{DirectoryPath}/");
        
            Console.ForegroundColor = ConsoleColor.DarkYellow;
            Console.WriteLine($"{'\t'}+ /{fileName.ToLogFormat()}");
           
            Console.ResetColor();
            
            return;
        }
        
        Console.ForegroundColor = ConsoleColor.Green;
        Console.Write("успешно");
        Console.WriteLine();
        Console.WriteLine();
        
        Console.ForegroundColor = ConsoleColor.DarkCyan;
        Console.WriteLine("результат выполнения программы в папке result:");
        Console.WriteLine($"{'\t'}- код был затранслирован в python");
        Console.WriteLine($"{'\t'}- результат лексического анализатора сохранен");
        Console.WriteLine($"{'\t'}- дерево синтаксического анализатора тоже сохранено");
        Console.WriteLine();

        Console.ForegroundColor = ConsoleColor.DarkMagenta;
        Console.WriteLine($"{DirectoryPath}/");
        
        Console.ForegroundColor = ConsoleColor.DarkYellow;
        Console.WriteLine($"{'\t'}+ /{fileName.ToPyFormat()}");
        Console.WriteLine($"{'\t'}+ /{fileName.ToLexicalFormat()}");
        Console.WriteLine($"{'\t'}+ /{fileName.ToTreeFormat()}");

        Console.ResetColor();
    }
    
    public static void WriteToFile(string message, string fileName)
    {
        try
        {
            if (!Directory.Exists(DirectoryPath))
            {
                Directory.CreateDirectory(DirectoryPath);
            }
            
            using var fs = File.Create($"{DirectoryPath}/{fileName}");
            var info = new UTF8Encoding(true).GetBytes(message);
            fs.Write(info, 0, info.Length);
        }
        catch (Exception ex)
        {
            Console.Write($"не удалось записать в файл: {ex.Message}");
        }
    }
    
    public static void RunPythonScript(string fileName)
    {
        var startInfo = new ProcessStartInfo
        {
            FileName = "cmd.exe",
            Arguments = $"/K python {DirectoryPath}/{fileName}",
            UseShellExecute = true  
        };

        try
        {
            Process.Start(startInfo);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error launching CMD: {ex.Message}");
        }
    }

    public static string ToPyFormat(this string str) => $"{str}.py";
    public static string ToLexicalFormat(this string str) => $"{str}.miya-lexical";
    public static string ToTreeFormat(this string str) => $"{str}.miya-tree";
    public static string ToLogFormat(this string str) => $"{str}.miya-error-log";
    public static bool Validate(FileInfo info) => info.format == FileType;
}