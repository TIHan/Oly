# <img src="vscode/icons/oly-dark.png" alt="oly-logo" width="200"/>
Oly Language Compiler and Runtime

A side project to understand programming languages and compilers in all stages.

**Note: At the moment, documentation is in progress.**

## Building

Requirements:
- Visual Studio 2022 Community 17.13.6 or later - [Download](https://visualstudio.microsoft.com/vs/community/)
    - Must use this to build Oly with the appropriate F# version.
- .NET 8 - [Download](https://dotnet.microsoft.com/en-us/download/dotnet/8.0)

This will build everything except for the VSCode extension.

Open `Oly.sln` in Visual Studio and build it there.

## Running Tests

**Note: Some tests are expected to fail as this project is in active development.**

Use Visual Studio's Test Explorer to run the tests.

SPIR-V: Currently experimental. Requires the [Evergreen](https://github.com/tihan/evergreen) repository to be located in the same directory next to the Oly repository. You must also build `Evergreen\src\managed\Engine\gpu_test.olyx` before running the SPIR-V tests.

## Benchmarks

Requires the [Evergreen](https://github.com/tihan/evergreen) repository to be located in the same directory next to the Oly repository

Use Visual Studio to launch the `Oly.Benchmarks` project.

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
