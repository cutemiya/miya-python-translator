namespace Translator;

public class ComparePriority : EqualityComparer<SymbolType>
{
    public override bool Equals(SymbolType first, SymbolType second)
    {
        return second switch
        {
            SymbolType.ImpExpression => first is SymbolType.ImpExpression or SymbolType.OrExpression or SymbolType.AndExpression or SymbolType.AtomExpression,
            SymbolType.OrExpression => first is SymbolType.OrExpression or SymbolType.AndExpression or SymbolType.AtomExpression,
            SymbolType.AndExpression => first is SymbolType.AndExpression or SymbolType.AtomExpression,
            _ => second == first
        };
    }

    public override int GetHashCode(SymbolType obj)
    {
        throw new NotImplementedException();
    }
}