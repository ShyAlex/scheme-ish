using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Input;

namespace ShyAlex.Scheme.Debugger
{
    public class RelayCommand : ICommand
    {
        private readonly Action<Object> execute;

        private readonly Func<Object, Boolean> canExecute;

        public event EventHandler CanExecuteChanged = (o, e) => { };

        public RelayCommand(Action<Object> execute)
            :this (execute, p => true) { }

        public RelayCommand(Action<Object> execute, Func<Object, Boolean> canExecute)
        {
            this.execute = execute;
            this.canExecute = canExecute;
        }

        public Boolean CanExecute(Object parameter)
        {
            return canExecute(parameter);
        }

        public void Execute(Object parameter)
        {
            execute(parameter);
        }

        public void RaiseCanExecuteChanged()
        {
            CanExecuteChanged(this, EventArgs.Empty);
        }
    }
}
