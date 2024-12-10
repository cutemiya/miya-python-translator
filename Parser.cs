using Translator.Exceptions;

namespace Translator;

public class Parser(List<Token> tokens)
{
    private readonly Stack<Token> _stack = new();
    private readonly Queue<Token> _queue = new(tokens);
    private Rule _currentRule;
    private readonly ComparePriority _comparePriority = new();

    public Token Parse()
    {
        try
        {
            while (true)
            {
                _currentRule = Grammar.Rules
                    .FirstOrDefault(rule => _stack
                                                .Take(rule.Stack.Length)
                                                .Select(token => token.Type)
                                                .SequenceEqual(rule.Stack, _comparePriority) &&
                                            (rule.Input == null || _queue
                                                .Take(rule.Input.Length)
                                                .Select(token => token.Type)
                                                .SequenceEqual(rule.Input)));

                if (_currentRule == null)
                {
                    if (_queue.Count == 0)
                    {
                        if (_stack.Count == 1)
                        {
                            return _stack.Single();
                        }
                        
                        throw Error(_stack.First(), "Неверное количество скобок в программе.");
                    }

                    Shift();
                }
                else
                {
                    Reduce();
                }
            }
        }
        catch (ParseError)
        {
            return null;
        }
    }

    private void Shift()
    {
        var shiftRule = Grammar.ShiftRules
            .FirstOrDefault(x => _queue
                .Take(x.Length)
                .Select(token => token.Type)
                .SequenceEqual(x));

        
        if (shiftRule != null)
        {
            _stack.Push(_queue.Dequeue());
        }
        else
        {
            var max = Grammar.ShiftRules.Select(ruleArr =>
            {
                IEnumerable<SymbolType> ruleEnum = ruleArr;
                IEnumerable<Token> queueEnum = _queue;

                var count = 0;

                while (ruleEnum.First() == queueEnum.First().Type)
                {
                    ++count;
                    ruleEnum = ruleEnum.Skip(1);
                    queueEnum = queueEnum.Skip(1);
                }

                return (count, ruleEnum.First());
            }).MaxBy(x => x.Item1);

            throw Error(_queue.Skip(max.Item1).First(), $"Ожидается {max.Item2}.");
        }
    }

    private void Reduce()
    {
        var token = new Token(
            _currentRule.Output,
            _stack.Take(_currentRule.Stack.Length)
                .Select(token => token.Lexeme)
                .Reverse()
                .Aggregate((accumulator, next) => accumulator + ' ' + next),
            false,
            0,
            new List<Token>(_stack.Take(_currentRule.Stack.Length)));

        for (var i = 0; i < _currentRule.Stack.Length; ++i)
        {
            _stack.Pop();
        }

        _stack.Push(token);
    }

    private static ParseError Error(Token token, string message)
    {
        Program.Error(token, message);
        return new ParseError();
    }
}