using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ShyAlex.Scheme.Debugger.ViewModel
{
    public class Environment
    {
        private String e;

        public Environment(String e)
        {
            this.e = e;
        }

        public static Environment FromEnv(Env.env<Parser2.expression> env)
        {
            return new Environment(env.ToString());
        }

        public override string ToString()
        {
            return e;
        }
    }
}
