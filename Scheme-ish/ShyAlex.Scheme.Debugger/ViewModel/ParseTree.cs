using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Media;
using Microsoft.FSharp.Collections;

namespace ShyAlex.Scheme.Debugger.ViewModel
{
    public class ParseTree : IEnumerable<ParseTree>
    {
        public Environment Environment { get; private set; }

        public List<ParseTree> Children { get; private set; }

        public String Description { get; private set; }

        public Brush Color { get; private set; }

        public ParseTree(Environment env, List<ParseTree> children, String description, Brush color)
        {
            Environment = env;
            Children = children;
            Description = description;
            Color = color;
        }

        public static ParseTree FromSchemeTree(Environment parentEnv, Types.expression exp)
        {
            if (exp.IsLiteral)
            {
                var lit = (Types.expression.Literal)exp;
                return new ParseTree(parentEnv, null, lit.ToString(), Brushes.Cyan);
            }
            if (exp.IsKeyword)
            {
                var kw = (Types.expression.Keyword)exp;
                return new ParseTree(parentEnv, null, kw.ToString(), Brushes.DeepPink);
            }
            if (exp.IsVariable)
            {
                var v = (Types.expression.Variable)exp;
                return new ParseTree(parentEnv, null, v.ToString(), Brushes.WhiteSmoke);
            }
            if (exp.IsScope)
            {
                var s = (Types.expression.Scope)exp;
                var env = Environment.FromEnv(s.Item1);
                return new ParseTree(env, new List<ParseTree> { FromSchemeTree(env, s.Item2) }, "env", Brushes.Lime);
            }

            var e = (Types.expression.Expression)exp;
            var subExps = e.Item.Select(sube => FromSchemeTree(parentEnv, sube)).ToList();
            return new ParseTree(parentEnv, subExps, "expr", Brushes.Yellow);
        }

        public override String ToString()
        {
            return Description;
        }

        public IEnumerator<ParseTree> GetEnumerator()
        {
            return Children != null ? Children.GetEnumerator() : Enumerable.Empty<ParseTree>().GetEnumerator();
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return Children != null ? Children.GetEnumerator() : Enumerable.Empty<ParseTree>().GetEnumerator();
        }
    }
}
