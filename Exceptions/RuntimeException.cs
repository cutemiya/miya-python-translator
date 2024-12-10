namespace Translator.Exceptions;

public class RuntimeException(Token token, string message) : Exception(message)
{
    public readonly Token Token = token;
}