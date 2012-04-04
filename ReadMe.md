ShyAlex.Scheme
==============

ShyAlex.Scheme is an interpreter for a Scheme-like language, which features a graphical display of the evaluation process.

Disclaimer
----------

I *really* have no idea what I'm doing. This is just me having a bash at something I found interesting.

About
-----

The project's written in a mixture of F# and C#. Opening the solution properly requires a full version of Visual Studio 2010, although opening it in the free editions (Visual C# Express and the VS 2010 shell + F#) will allow you to work with the projects which use the appropriate language.

Building simply requires the .NET 4.0 SDK - the script Build/Build.bat can be run at the command line in the Build directory to build everything. Similarly you can run the NUnit tests with the Test.bat script.

ShyAlex.Scheme.Console is a CLI for the interpreter. The CLI will print out intermediate steps in the evaluation process and the contents of the environment.

ShyAlex.Scheme.Debugger is a GUI for the interpreter, which features a graphical display of the evaluation process.

See ShyAlex.Scheme.Samples for programs which will work with the interpreter (they're used for the tests and in the GUI app).
