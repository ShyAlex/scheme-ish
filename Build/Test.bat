@echo off
set config=debug
set nunit=..\3rdParty\NUnit\nunit-console-x86.exe
%nunit% /labels ..\Scheme-ish\ShyAlex.Scheme.Tests\bin\%config%\ShyAlex.Scheme.Tests.dll
