namespace Translator;

public class Rule(SymbolType[] stack, SymbolType[] input, SymbolType output)
{
    public readonly SymbolType[] Stack = stack;
    public readonly SymbolType[] Input = input;
    public readonly SymbolType Output = output;
}