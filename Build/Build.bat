@echo off
cd /D %~dp0
set msbuild=%windir%\Microsoft.NET\Framework\v4.0.30319\msbuild.exe
set config=Debug
set platform=x86
%msbuild% ..\Scheme-ish\Scheme-ish.sln /t:Rebuild /p:Configuration=%config%;%platform%="Mixed Platforms"
exit /b %%ERRORLEVEL%%
