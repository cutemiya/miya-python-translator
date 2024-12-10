namespace Translator.Extensions;

using System;
using System.Collections.Generic;

static class LogicalExtension
{
    public static string TranslateLogicExpression(string expr)
    {
        var tokens = expr.Replace("(", "( ").Replace(")", " )").Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);

        var output = new Stack<string>();
        var operators = new Stack<string>();

        for (int i = 0; i < tokens.Length; i++)
        {
            string token = tokens[i];

            if (token == "(")
            {
                operators.Push(token);
            }
            else if (token == ")")
            {
                while (operators.Count > 0 && operators.Peek() != "(")
                {
                    EvaluateTopOperator(output, operators.Pop());
                }
                if (operators.Count == 0)
                {
                    throw new ArgumentException("Invalid expression: Mismatched parentheses");
                }
                operators.Pop(); // Убираем "(" из стека
            }
            else if (IsOperator(token))
            {
                while (operators.Count > 0 && Priority(operators.Peek()) >= Priority(token))
                {
                    EvaluateTopOperator(output, operators.Pop());
                }
                operators.Push(token);
            }
            else if (token == "0" || token == "1")
            {
                // Interpreting 0 as FALSE and 1 as TRUE
                output.Push(token == "0" ? "False" : "True");
            }
            else
            {
                // Interpreting other variables directly
                output.Push(token);
            }
        }

        while (operators.Count > 0)
        {
            EvaluateTopOperator(output, operators.Pop());
        }

        if (output.Count != 1)
        {
            throw new ArgumentException("Invalid expression: Failed to evaluate completely");
        }

        return output.Pop();
    }

    private static bool IsOperator(string token)
    {
        return token == ".NOT." || token == ".AND." || token == ".OR." || token == ".IMP.";
    }

    private static int Priority(string op)
    {
        if (op == ".NOT.")
            return 4;
        if (op == ".AND.")
            return 3;
        if (op == ".OR.")
            return 2;
        if (op == ".IMP.")
            return 1;
        return 0;
    }

    private static void EvaluateTopOperator(Stack<string> output, string op)
    {
        if (op == ".NOT.")
        {
            if (output.Count < 1)
                throw new ArgumentException("Invalid expression: Not enough operands for .NOT.");

            string operand = output.Pop();
            output.Push($"(not {operand})");
        }
        else
        {
            if (output.Count < 2)
                throw new ArgumentException("Invalid expression: Not enough operands");

            string right = output.Pop();
            string left = output.Pop();

            if (op == ".AND.")
            {
                output.Push($"({left} and {right})");
            }
            else if (op == ".OR.")
            {
                output.Push($"({left} or {right})");
            }
            else if (op == ".IMP.")
            {
                output.Push($"(not {left} or {right})");
            }
        }
    }
}