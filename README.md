# <img src="vscode/icons/oly-dark.png" alt="oly-logo" width="200"/>
Oly Language Compiler and Runtime

A side project to understand programming languages and compilers in all stages.

**Note: At the moment, documentation is in progress on building the project and language specification. It will come eventually.**

## Building

Requirements:
- .NET 7 - [Download](https://dotnet.microsoft.com/en-us/download/dotnet/7.0)

This will build everything except for the VSCode extension.

In the command-line at the root of the repository, run command:

1. ```dotnet build```

You can also open `Oly.sln` in Visual Studio and build it there.

## Running Tests

**Note: Some tests are expected to fail.**

In the command-line at the root of the repository, run command:

1. ```dotnet test```

You can also run the tests in Visual Studio's Test Explorer.

## Building VSCode Extension

Requirements:
- VSCode 1.64.0 or newer
- Node.js and NPM

In the command-line at the `vscode` folder, run commands:

```
npm install
npm run compile
```

## Running VSCode Extension

After building it once:

1. Make sure `src/LanguageServer/Oly.LanguageServer/Oly.LanguageServer.fsproj` has been built.
2. Open up the `vscode` directory in VSCode.
3. Navigate to `src/extension.ts` and open it.
4. Press F5.
