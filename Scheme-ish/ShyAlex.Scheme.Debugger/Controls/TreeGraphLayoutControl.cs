using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using QuickGraph;
using GraphSharp.Controls;
using ShyAlex.Scheme.Debugger.ViewModel;

namespace ShyAlex.Scheme.Debugger.Controls
{
    public class TreeGraphLayoutControl : GraphLayout<ParseTree, Edge<ParseTree>, BidirectionalGraph<ParseTree, Edge<ParseTree>>>
    {
    }
}
