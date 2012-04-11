using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Windows.Input;
using System.Windows.Threading;
using Microsoft.FSharp.Core;

using QuickGraph;
using GraphSharp;

namespace ShyAlex.Scheme.Debugger.ViewModel
{
    public class TreeClientApp : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged = (o, e) => { };

        public String Program { get; set; }

        public BidirectionalGraph<ParseTree, Edge<ParseTree>> Graph
        {
            get 
            {
                if (currentProgram == null)
                {
                    return null;
                }

                var graph = new BidirectionalGraph<ParseTree, Edge<ParseTree>>();
                graph.AddVertex(currentProgram.Current);
                AssembleGraph(graph, currentProgram.Current);
                return graph;
            }
        }

        private void AssembleGraph(BidirectionalGraph<ParseTree, Edge<ParseTree>> graph, ParseTree parent)
        {
            if (parent.Children == null)
            {
                return;
            }

            foreach (var child in parent.Children)
            {
                graph.AddVertex(child);
                graph.AddEdge(new Edge<ParseTree>(parent, child));
                AssembleGraph(graph, child);
            }
        }

        private IEnumerator<ParseTree> currentProgram;

        private readonly DispatcherTimer timer;

        public IList<ParseTree> CurrentTree { get { return new List<ParseTree> { currentProgram == null ? null : currentProgram.Current }; } }

        public IDictionary<String, RelayCommand> Samples { get; private set; }

        public RelayCommand ExecuteProgramCommand { get; private set; }

        public RelayCommand StepForwardCommand { get; private set; }

        public RelayCommand LoadSampleCommand { get; private set; }

        public RelayCommand PlayCommand { get; private set; }

        public TreeClientApp(IList<String> sampleFiles)
        {
            timer = new DispatcherTimer { Interval = TimeSpan.FromSeconds(0.3) };
            timer.Tick += TimerStepForward;
            Program = String.Empty;
            ExecuteProgramCommand = new RelayCommand(ExecuteProgram);
            StepForwardCommand = new RelayCommand(StepForward, p => currentProgram != null);
            LoadSampleCommand = new RelayCommand(LoadSample);
            PlayCommand = new RelayCommand(Play, p => currentProgram != null);
            Samples = sampleFiles.ToDictionary(s => s, s => LoadSampleCommand);
        }

        private void LoadSample(Object param)
        {
            var file = (String)param;
            Program = File.ReadAllText(file);
            RaisePropertyChanged("Program");
        }

        private void TimerStepForward(Object sender, EventArgs e)
        {
            if (currentProgram == null)
            {
                timer.Stop();
            }

            StepForward(null);
        }

        private void Play(Object ignored)
        {
            if (currentProgram == null)
            {
                return;
            }

            if (timer.IsEnabled)
            {
                timer.Stop();
            }
            else
            {
                timer.Start();
            }
        }

        private void StepForward(Object ignored)
        {
            if (currentProgram == null)
            {
                return;
            }

            if (!currentProgram.MoveNext())
            {
                currentProgram.Dispose();
                currentProgram = null;
            }

            StepForwardCommand.RaiseCanExecuteChanged();
            PlayCommand.RaiseCanExecuteChanged();
            RaisePropertyChanged("CurrentTree");
            RaisePropertyChanged("Graph");
        }

        private void ExecuteProgram(Object ignored)
        {
            if (currentProgram != null)
            {
                currentProgram.Dispose();
            }

            currentProgram = GetResultStream(Program).GetEnumerator();
            StepForward(null);
        }

        private void RaisePropertyChanged(String property)
        {
            PropertyChanged(this, new PropertyChangedEventArgs(property));
        }

        private static IEnumerable<ParseTree> GetResultStream(String program)
        {
            var lexedOutput = Lexer.lex(program);
            var lexResult = Lexer.validateLex(lexedOutput);

            if (lexResult.IsIncomplete || lexResult.IsOverClosed)
            {
                throw new InvalidOperationException();
            }

            var parser1Output = Parser.parse(lexedOutput).Item2;
            var env = Env.newEnv();

            return parser1Output.SelectMany(statement =>
            {
                var parser2Output = Parser2.parse(statement);
                return Interpreter.reduce(env, parser2Output)
                                    .Select(t =>
                                        {
                                            env = t.Item2;
                                            return ParseTree.FromSchemeTree(Environment.FromEnv(env), t.Item1);
                                        });
            });
        }
    }
}
