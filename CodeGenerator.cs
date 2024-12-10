using System.Text;
using Translator.Extensions;

namespace Translator;

public class CodeGenerator(Token treeToken)
{
    private record MainToken(SymbolType type, Token token);

    private readonly List<Token> _varDeclaration = new();
    private readonly List<MainToken> _mainDeclaration = new();
    private readonly StringBuilder _code = new();

    public CodeGenerator Init()
    {
        ReadVars(treeToken);
        ReadMain(treeToken);
        return this;

        void ReadVars(Token treeToken)
        {
            var upCh = treeToken?.Children.FirstOrDefault(t => t.Type == SymbolType.VarDefinition);

            if (upCh == null || upCh.Children.Count == 0) return;

            var upChDef = upCh.Children.FirstOrDefault(t => t.Type == SymbolType.VarList);

            if (upChDef == null || upChDef.Children.Count == 0) return;

            Add(Filter(upChDef.Children).ToList());

            return;

            void Add(List<Token> currentTokens)
            {
                if (currentTokens.Count == 0) return;


                AddCurrent(Filter(currentTokens));

                var childrenTokens = Filter(currentTokens)
                    .Where(t => t.Type == SymbolType.VarList)
                    .Aggregate(new List<Token>(), (acc, t) =>
                    {
                        acc.AddRange(t.Children);
                        return acc;
                    });

                Add(childrenTokens);
            }

            IEnumerable<Token> Filter(IEnumerable<Token> currentTokens) =>
                currentTokens.Where(t => t.Type is SymbolType.Identifier or SymbolType.VarList).ToList();

            void AddCurrent(IEnumerable<Token> currentTokens) =>
                _varDeclaration.AddRange(currentTokens.Where(t => t.Type == SymbolType.Identifier));
        }

        void ReadMain(Token treeToken)
        {
            var upperWrapper = treeToken.Children.FirstOrDefault(t => t.Type == SymbolType.CalcDefinition);
            if (upperWrapper == null || upperWrapper.Children.Count == 0) return;

            var upperAssigmentList = upperWrapper.Children.FirstOrDefault(t => t.Type == SymbolType.AssignmentList);
            if (upperAssigmentList == null || upperAssigmentList.Children.Count == 0) return;
            ReadCurrent(upperAssigmentList.Children);

            return;

            MainToken GetCurrent(List<Token> currentTokens)
            {
                var assignmentToken = currentTokens.FirstOrDefault(t => t.Type == SymbolType.Assignment);
                return assignmentToken!.Children.Any(t => t.Type == SymbolType.Operator)
                    ? new MainToken(SymbolType.Operator,
                        assignmentToken.Children.FirstOrDefault(t => t.Type == SymbolType.Operator))
                    : new MainToken(SymbolType.Assignment, assignmentToken);
            }

            Token GetAssignmentList(List<Token> currentTokens) =>
                currentTokens.FirstOrDefault(t => t.Type == SymbolType.AssignmentList);

            void ReadCurrent(List<Token> currentTokens)
            {
                _mainDeclaration.Add(GetCurrent(currentTokens));

                var currentAssignmentList = GetAssignmentList(currentTokens);

                if (currentAssignmentList == null) return;

                if (currentAssignmentList.Children != null && currentAssignmentList.Children.Count != 0)
                {
                    ReadCurrent(currentAssignmentList.Children);
                }
            }
        }
    }

    private StringBuilder GenerateMain()
    {
        var sb = new StringBuilder();
        _mainDeclaration.ForEach(t =>
        {
            if (t.type == SymbolType.Operator)
            {
                sb.AppendLine(GenerateOperator(t.token).ToString());
            }

            if (t.type == SymbolType.Assignment)
            {
                sb.AppendLine(GenerateSoloAssignment(t.token).ToString());
            }
        });
        return sb;

        StringBuilder GenerateOperator(Token currentToken)
        {
            var innerSb = new StringBuilder();
            if (currentToken.Children.Any(t => t.Type == SymbolType.ReadStatement))
            {
                return GetReadDeclaration(currentToken);
            }

            if (currentToken.Children.Any(t => t.Type == SymbolType.WriteStatement))
            {
                var writeStatementTokens =
                    currentToken.Children.First(t => t.Type == SymbolType.WriteStatement).Children;

                if (writeStatementTokens.Any(t => t.Type == SymbolType.If))
                {
                    var expression = writeStatementTokens.FirstOrDefault(t => t.Type == SymbolType.Expression);
                    var normalizedExpression = LogicalExtension.TranslateLogicExpression(expression?.Lexeme);
                    writeStatementTokens.Reverse();

                    var hasElse = writeStatementTokens.Any(t => t.Type == SymbolType.Else);
                    var searchIndex = hasElse
                        ? writeStatementTokens
                            .FindIndex(t => t.Type == SymbolType.Else)
                        : writeStatementTokens.Count - 1;

                    innerSb.AppendLine($"if {normalizedExpression}:");
                    _ = AddAssignmentList(writeStatementTokens
                            .Take(searchIndex)
                            .FirstOrDefault(t => t.Type == SymbolType.AssignmentList), new())
                        .Aggregate(innerSb, (_, c) => c
                            .Split(Environment.NewLine)
                            .Aggregate(innerSb, (innerAcc, innerC) => innerAcc.AppendLine($"\t{innerC}"))
                        );

                    if (hasElse)
                    {
                        innerSb.AppendLine("else:");
                        _ = AddAssignmentList(writeStatementTokens
                                .Skip(searchIndex)
                                .Take(writeStatementTokens.Count - 1)
                                .FirstOrDefault(t => t.Type == SymbolType.AssignmentList), new())
                            .Aggregate(innerSb, (_, c) => c
                                .Split(Environment.NewLine)
                                .Aggregate(innerSb, (innerAcc, innerC) => innerAcc.AppendLine($"\t{innerC}"))
                            );
                    }

                    return innerSb;

                    List<string> AddAssignmentList(Token token, List<string> assignmentList)
                    {
                        var current = token.Children.FirstOrDefault(t => t.Type == SymbolType.Assignment);
                        if (current != null)
                            AddAssignment(current);


                        if (token.Children != null)
                        {
                            var tokens = token.Children.FirstOrDefault(t => t.Type == SymbolType.AssignmentList);
                            if (tokens != null)
                            {
                                AddAssignmentList(tokens, assignmentList);
                            }
                        }

                        return assignmentList;

                        void AddAssignment(Token c)
                        {
                            if (c.Children.Any(t => t.Type == SymbolType.Operator))
                            {
                                assignmentList.Add(
                                    GenerateOperator(c.Children.First(t => t.Type == SymbolType.Operator)).ToString());
                            }

                            if (c.Children.Any(t => t.Type == SymbolType.Expression))
                            {
                                assignmentList.Add($"" +
                                                   $"{c.Children.First(t => t.Type == SymbolType.Identifier).Lexeme} = " +
                                                   $"{LogicalExtension.TranslateLogicExpression(c.Children.First(t => t.Type == SymbolType.Expression).Lexeme)}");
                            }
                        }
                    }
                }

                if (writeStatementTokens.Any(t => t.Type == SymbolType.Write))
                {
                    return GetWriteDeclaration(writeStatementTokens);
                }

                return innerSb;
            }

            return innerSb;

            StringBuilder GetReadDeclaration(Token curToken)
            {
                var readStatementTokensInner =
                    curToken.Children.First(t => t.Type == SymbolType.ReadStatement).Children;
                var varList = readStatementTokensInner.First(t => t.Type == SymbolType.VarList);
                var variables = GetVarList(varList.Children);

                return variables.Aggregate(innerSb, (acc, c) => acc.AppendLine($"{c} = bool(int(input(\"Введите {c}:\")))"));
            }

            StringBuilder GetWriteDeclaration(List<Token> curTokens)
            {
                var varList = curTokens.First(t => t.Type == SymbolType.VarList);
                var variables = GetVarList(varList.Children);

                return variables.Aggregate(innerSb, (acc, c) => acc.AppendLine($"print(f'{c}=" + '{' + c + "}'" + ')'));
            }

            List<string> GetVarList(List<Token> variableList)
            {
                var variables = new List<string>();
                Helper(variableList);

                return variables;

                void Helper(List<Token> vbList)
                {
                    var current = GetCurrent(vbList);
                    variables.Add(current.Lexeme);

                    if (vbList.Any(t => t.Type == SymbolType.VarList))
                    {
                        Helper(vbList.First(t => t.Type == SymbolType.VarList).Children);
                    }

                    Token GetCurrent(List<Token> lastVariableList) =>
                        lastVariableList.FirstOrDefault(t => t.Type == SymbolType.Identifier);
                }
            }
        }

        StringBuilder GenerateSoloAssignment(Token currentToken)
        {
            var innerSb = new StringBuilder();

            innerSb.AppendLine(
                $"{GetIdent(currentToken.Children)} = {LogicalExtension.TranslateLogicExpression(GetExpression(currentToken.Children))}");

            return innerSb;

            string GetIdent(List<Token> tokens) => tokens.First(t => t.Type == SymbolType.Identifier).Lexeme;
            string GetExpression(List<Token> tokens) => tokens.First(t => t.Type == SymbolType.Expression).Lexeme;
        }
    }

    public CodeGenerator Generate()
    {
        _code
            .AddDefault()
            .AddVarDeclarationTitle()
            .AddVarDeclaration(_varDeclaration)
            .AppendLine()
            .AddMainDeclarationTitle()
            .Append(GenerateMain());

        return this;
    }

    public string GetCode() => _code.ToString();
}