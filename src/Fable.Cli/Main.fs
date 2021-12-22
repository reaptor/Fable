module Fable.Cli.Main

open System
open System.Collections.Concurrent
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.SourceCodeServices

open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State
open ProjectCracker

module private Util =
    let loadType (cliArgs: CliArgs) (r: PluginRef): Type =
        /// Prevent ReflectionTypeLoadException
        /// From http://stackoverflow.com/a/7889272
        let getTypes (asm: System.Reflection.Assembly) =
            let mutable types: Option<Type[]> = None
            try
                types <- Some(asm.GetTypes())
            with
            | :? System.Reflection.ReflectionTypeLoadException as e ->
                types <- Some e.Types
            match types with
            | None -> Seq.empty
            | Some types ->
                types |> Seq.filter ((<>) null)

        // The assembly may be already loaded, so use `LoadFrom` which takes
        // the copy in memory unlike `LoadFile`, see: http://stackoverflow.com/a/1477899
        System.Reflection.Assembly.LoadFrom(r.DllPath)
        |> getTypes
        // Normalize type name
        |> Seq.tryFind (fun t -> t.FullName.Replace("+", ".") = r.TypeFullName)
        |> function
            | Some t ->
                $"Loaded %s{r.TypeFullName} from %s{IO.Path.GetRelativePath(cliArgs.RootDir, r.DllPath)}"
                |> Log.always; t
            | None -> failwithf "Cannot find %s in %s" r.TypeFullName r.DllPath

    let splitVersion (version: string) =
        match Version.TryParse(version) with
        | true, v -> v.Major, v.Minor, v.Revision
        | _ -> 0, 0, 0

    // let checkFableCoreVersion (checkedProject: FSharpCheckProjectResults) =
    //     for ref in checkedProject.ProjectContext.GetReferencedAssemblies() do
    //         if ref.SimpleName = "Fable.Core" then
    //             let version = System.Text.RegularExpressions.Regex.Match(ref.QualifiedName, @"Version=(\d+\.\d+\.\d+)")
    //             let expectedMajor, expectedMinor, _ = splitVersion Literals.CORE_VERSION
    //             let actualMajor, actualMinor, _ = splitVersion version.Groups.[1].Value
    //             if not(actualMajor = expectedMajor && actualMinor = expectedMinor) then
    //                 failwithf "Fable.Core v%i.%i detected, expecting v%i.%i" actualMajor actualMinor expectedMajor expectedMinor
    //             // else printfn "Fable.Core version matches"

    let formatException file ex =
        let rec innerStack (ex: Exception) =
            if isNull ex.InnerException then ex.StackTrace else innerStack ex.InnerException
        let stack = innerStack ex
        $"[ERROR] %s{file}{Log.newLine}%s{ex.Message}{Log.newLine}%s{stack}"

    let formatLog rootDir (log: Log) =
        match log.FileName with
        | None -> log.Message
        | Some file ->
            // Add ./ to make sure VS Code terminal recognises this as a clickable path
            let file = "." + IO.Path.DirectorySeparatorChar.ToString() + IO.Path.GetRelativePath(rootDir, file)
            let severity =
                match log.Severity with
                | Severity.Warning -> "warning"
                | Severity.Error -> "error"
                | Severity.Info -> "info"
            match log.Range with
            | Some r -> $"%s{file}(%i{r.start.line},%i{r.start.column}): (%i{r.``end``.line},%i{r.``end``.column}) %s{severity} %s{log.Tag}: %s{log.Message}"
            | None -> $"%s{file}(1,1): %s{severity} %s{log.Tag}: %s{log.Message}"

    let logErrors rootDir (logs: Log seq) =
        logs
        |> Seq.filter (fun log -> log.Severity = Severity.Error)
        |> Seq.iter (fun log -> Log.error(formatLog rootDir log))

    let getFSharpDiagnostics (diagnostics: FSharpDiagnostic array) =
        diagnostics
        |> Array.map (fun er ->
            let severity =
                match er.Severity with
                | FSharpDiagnosticSeverity.Hidden
                | FSharpDiagnosticSeverity.Info -> Severity.Info
                | FSharpDiagnosticSeverity.Warning -> Severity.Warning
                | FSharpDiagnosticSeverity.Error -> Severity.Error

            let range =
                { start={ line=er.StartLine; column=er.StartColumn+1}
                  ``end``={ line=er.EndLine; column=er.EndColumn+1}
                  identifierName = None }

            let msg = $"%s{er.Message} (code %i{er.ErrorNumber})"

            Log.Make(severity, msg, fileName=er.FileName, range=range, tag="FSHARP")
        )

    let changeFsExtension isInFableHiddenDir filePath fileExt =
        let fileExt =
            // Prevent conflicts in package sources as they may include
            // JS files with same name as the F# .fs file
            if fileExt = ".js" && isInFableHiddenDir then
                CompilerOptionsHelper.DefaultExtension
            else fileExt
        Path.replaceExtension fileExt filePath

    let getOutPath (cliArgs: CliArgs) pathResolver file =
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let isInFableHiddenDir = Naming.isInFableModules file
        match cliArgs.OutDir with
        | Some outDir ->
            let projDir = IO.Path.GetDirectoryName cliArgs.ProjectFile
            let absPath = Imports.getTargetAbsolutePath pathResolver file projDir outDir
            changeFsExtension isInFableHiddenDir absPath fileExt
        | None ->
            changeFsExtension isInFableHiddenDir file fileExt

    type FileWriter(sourcePath: string, targetPath: string, cliArgs: CliArgs, pathResolver: PathResolver) =
        // In imports *.ts extensions have to be converted to *.js extensions instead
        let fileExt =
            let fileExt = cliArgs.CompilerOptions.FileExtension
            if fileExt.EndsWith(".ts") then Path.replaceExtension ".js" fileExt else fileExt
        let sourceDir = Path.GetDirectoryName(sourcePath)
        let targetDir = Path.GetDirectoryName(targetPath)
        let stream = new IO.StreamWriter(targetPath)
        let mapGenerator = lazy (SourceMapSharp.SourceMapGenerator(?sourceRoot = cliArgs.SourceMapsRoot))
        interface BabelPrinter.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask
            member _.EscapeJsStringLiteral(str) =
                Web.HttpUtility.JavaScriptStringEncode(str)
            member _.MakeImportPath(path) =
                let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
                let path =
                    match pathResolver.TryPrecompiledOutPath(sourceDir, path) with
                    | Some path -> Imports.getRelativePath sourceDir path
                    | None -> path
                let path = Imports.getImportPath pathResolver sourcePath targetPath projDir cliArgs.OutDir path
                if path.EndsWith(".fs") then
                    let isInFableHiddenDir = Path.Combine(targetDir, path) |> Naming.isInFableModules
                    changeFsExtension isInFableHiddenDir path fileExt
                else path
            member _.Dispose() = stream.Dispose()
            member _.AddSourceMapping((srcLine, srcCol, genLine, genCol, name)) =
                if cliArgs.SourceMaps then
                    let generated: SourceMapSharp.Util.MappingIndex = { line = genLine; column = genCol }
                    let original: SourceMapSharp.Util.MappingIndex = { line = srcLine; column = srcCol }
                    let targetPath = Path.normalizeFullPath targetPath
                    let sourcePath = Path.getRelativeFileOrDirPath false targetPath false sourcePath
                    mapGenerator.Force().AddMapping(generated, original, source=sourcePath, ?name=name)
        member _.SourceMap =
            mapGenerator.Force().toJSON()

    let compileFile (cliArgs: CliArgs) pathResolver (com: CompilerImpl) = async {
        try
            let babel =
                FSharp2Fable.Compiler.transformFile com
                |> FableTransforms.transformFile com
                |> Fable2Babel.Compiler.transformFile com

            let outPath = getOutPath cliArgs pathResolver com.CurrentFile

            // ensure directory exists
            let dir = IO.Path.GetDirectoryName outPath
            if not (IO.Directory.Exists dir) then IO.Directory.CreateDirectory dir |> ignore

            // write output to file
            let! sourceMap = async {
                use writer = new FileWriter(com.CurrentFile, outPath, cliArgs, pathResolver)
                do! BabelPrinter.run writer babel
                return if cliArgs.SourceMaps then Some writer.SourceMap else None
            }

            // write source map to file
            match sourceMap with
            | Some sourceMap ->
                let mapPath = outPath + ".map"
                do! IO.File.AppendAllLinesAsync(outPath, [$"//# sourceMappingURL={IO.Path.GetFileName(mapPath)}"]) |> Async.AwaitTask
                use fs = IO.File.Open(mapPath, IO.FileMode.Create)
                do! sourceMap.SerializeAsync(fs) |> Async.AwaitTask
            | None -> ()

            Log.verbose(lazy $"Compiled {IO.Path.GetRelativePath(cliArgs.RootDir, com.CurrentFile)}")

            return Ok {| File = com.CurrentFile
                         OutPath = outPath
                         Logs = com.Logs
                         WatchDependencies = com.WatchDependencies |}
        with e ->
            return Error {| File = com.CurrentFile
                            Exception = e |}
    }

module FileWatcherUtil =
    let getCommonBaseDir (files: string list) =
        let withTrailingSep d = $"%s{d}%c{IO.Path.DirectorySeparatorChar}"
        files
        |> List.map IO.Path.GetDirectoryName
        |> List.distinct
        |> List.sortBy (fun f -> f.Length)
        |> function
            | [] -> failwith "Empty list passed to watcher"
            | [dir] -> dir
            | dir::restDirs ->
                let rec getCommonDir (dir: string) =
                    // it's important to include a trailing separator when comparing, otherwise things
                    // like ["a/b"; "a/b.c"] won't get handled right
                    // https://github.com/fable-compiler/Fable/issues/2332
                    let dir' = withTrailingSep dir
                    if restDirs |> List.forall (fun d -> (withTrailingSep d).StartsWith dir') then dir
                    else
                        match IO.Path.GetDirectoryName(dir) with
                        | null -> failwith "No common base dir"
                        | dir -> getCommonDir dir
                getCommonDir dir

open Util
open FileWatcher
open FileWatcherUtil

let caseInsensitiveSet(items: string seq): ISet<string> =
    let s = HashSet(items)
    for i in items do s.Add(i) |> ignore
    s :> _

type FsWatcher(delayMs: int) =
    let globFilters = [ "*.fs"; "*.fsi"; "*.fsx"; "*.fsproj" ]
    let createWatcher () =
        let usePolling =
            // This is the same variable used by dotnet watch
            let envVar = Environment.GetEnvironmentVariable("DOTNET_USE_POLLING_FILE_WATCHER")
            not (isNull envVar) &&
                (envVar.Equals("1", StringComparison.OrdinalIgnoreCase)
                || envVar.Equals("true", StringComparison.OrdinalIgnoreCase))

        let watcher: IFileSystemWatcher =
            if usePolling then
                Log.always("Using polling watcher.")
                // Ignored for performance reasons:
                let ignoredDirectoryNameRegexes = [ "(?i)node_modules"; "(?i)bin"; "(?i)obj"; "\..+" ]
                upcast new ResetablePollingFileWatcher(globFilters, ignoredDirectoryNameRegexes)
            else
                upcast new DotnetFileWatcher(globFilters)
        watcher

    let watcher = createWatcher ()
    let observable = Observable.SingleObservable(fun () ->
        watcher.EnableRaisingEvents <- false)

    do
        watcher.OnFileChange.Add(fun path -> observable.Trigger(path))
        watcher.OnError.Add(fun ev ->
            Log.always("Watcher found an error, some events may have been lost.")
            Log.verbose(lazy ev.GetException().Message)
        )

    member _.Observe(filesToWatch: string list) =
        let commonBaseDir = getCommonBaseDir filesToWatch

        // It may happen we get the same path with different case in case-insensitive file systems
        // https://github.com/fable-compiler/Fable/issues/2277#issuecomment-737748220
        let filePaths = caseInsensitiveSet filesToWatch
        watcher.BasePath <- commonBaseDir
        watcher.EnableRaisingEvents <- true

        observable
        |> Observable.choose (fun fullPath ->
            let fullPath = Path.normalizePath fullPath
            if filePaths.Contains(fullPath)
            then Some fullPath
            else None)
        |> Observable.throttle delayMs
        |> Observable.map caseInsensitiveSet

// TODO: Check the path is actually normalized?
type File(normalizedFullPath: string) =
    let mutable sourceHash = None
    member _.NormalizedFullPath = normalizedFullPath
    member _.ReadSource() =
        match sourceHash with
        | Some h -> h, lazy File.readAllTextNonBlocking normalizedFullPath
        | _ ->
            let source = File.readAllTextNonBlocking normalizedFullPath
            let h = hash source
            sourceHash <- Some h
            h, lazy source

type ProjectCracked(cliArgs: CliArgs, crackerResponse: CrackerResponse, sourceFiles: File array) =

    member _.CliArgs = cliArgs
    member _.ProjectFile = cliArgs.ProjectFile
    member _.FableOptions = cliArgs.CompilerOptions
    member _.ProjectOptions = crackerResponse.ProjectOptions
    member _.References = crackerResponse.References
    member _.PrecompiledInfo = crackerResponse.PrecompiledInfo
    member _.CacheInvalidated = crackerResponse.CacheInvalidated
    member _.SourceFiles = sourceFiles
    member _.SourceFilePaths = sourceFiles |> Array.map (fun f -> f.NormalizedFullPath)
    member _.FableLibDir = crackerResponse.FableLibDir

    member _.MakeCompiler(currentFile, project, ?triggeredByDependency) =
        let opts =
            match triggeredByDependency with
            | Some t -> { cliArgs.CompilerOptions with TriggeredByDependency = t }
            | None -> cliArgs.CompilerOptions
        let fableLibDir = Path.getRelativePath currentFile crackerResponse.FableLibDir
        CompilerImpl(currentFile, project, opts, fableLibDir, ?outDir=cliArgs.OutDir)

    member _.MapSourceFiles(f) =
        ProjectCracked(cliArgs, crackerResponse, Array.map f sourceFiles)

    static member Init(cliArgs: CliArgs) =
        Log.always $"Parsing {cliArgs.ProjectFileAsRelativePath}..."
        let result, ms = Performance.measure <| fun () ->
            CrackerOptions(fableOpts = cliArgs.CompilerOptions,
                           fableLib = cliArgs.FableLibraryPath,
                           outDir = cliArgs.OutDir,
                           configuration = cliArgs.Configuration,
                           exclude = cliArgs.Exclude,
                           replace = cliArgs.Replace,
                           precompiledLib = cliArgs.PrecompiledLib,
                           noCache = cliArgs.NoCache,
                           noRestore = cliArgs.NoRestore,
                           projFile = cliArgs.ProjectFile)
            |> getFullProjectOpts

        // We display "parsed" because "cracked" may not be understood by users
        Log.always $"Project and references ({result.ProjectOptions.SourceFiles.Length} source files) parsed in %i{ms}ms{Log.newLine}"
        Log.verbose(lazy $"""F# PROJECT: %s{cliArgs.ProjectFileAsRelativePath}
    %s{result.ProjectOptions.OtherOptions |> String.concat $"{Log.newLine}    "}
    %s{result.ProjectOptions.SourceFiles |> String.concat $"{Log.newLine}    "}{Log.newLine}""")

        let sourceFiles = result.ProjectOptions.SourceFiles |> Array.map File
        ProjectCracked(cliArgs, result, sourceFiles)

type ProjectChecked(checker: InteractiveChecker, diagnostics: FSharpDiagnostic array, project: Project) =

    static member Check(checker: InteractiveChecker, projectFile, files: File[], ?lastFile, ?outFile, ?optimize) = async {
        Log.always "Started F# compilation..."

        let! checkResults, ms = Performance.measureAsync <| fun () ->
            let fileDic =
                files
                |> Seq.map (fun f -> f.NormalizedFullPath, f) |> dict
            let sourceReader f = fileDic.[f].ReadSource()
            let filePaths = files |> Array.map (fun file -> file.NormalizedFullPath)
            checker.ParseAndCheckProject(projectFile, filePaths, sourceReader, ?lastFile=lastFile)

        Log.always $"F# compilation finished in %i{ms}ms{Log.newLine}"

        let implFiles =
            match optimize with
            | Some true -> checkResults.GetOptimizedAssemblyContents().ImplementationFiles
            | _ -> checkResults.AssemblyContents.ImplementationFiles

        return implFiles, checkResults.Diagnostics, lazy checkResults.ProjectContext.GetReferencedAssemblies()
    }

    member _.Project = project
    member _.Checker = checker
    member _.Diagnostics = diagnostics

    static member CompileToFile(config: ProjectCracked, outPath: string) = async {
        let argv = [|
            "fsc.exe"
            yield! config.ProjectOptions.OtherOptions
            "--nowin32manifest" // See https://github.com/fsharp/fsharp-compiler-docs/issues/755
            "-o"
            outPath
            yield! config.SourceFilePaths
        |]

        Log.always("Generating assembly for precompilation...")
        let! result, ms = Performance.measureAsync <| fun () ->
            let checker = FSharpChecker.Create(
                keepAllBackgroundResolutions=false,
                keepAllBackgroundSymbolUses=false
            )
            checker.Compile(argv)
        Log.always($"Assembly generated in {ms}ms")

        return result
    }

    static member Init(projCracked: ProjectCracked) = async {
        let checker = InteractiveChecker.Create(projCracked.ProjectOptions)

        let! implFiles, errors, assemblies =
            ProjectChecked.Check(checker, projCracked.ProjectFile, projCracked.SourceFiles,
                                 optimize=projCracked.CliArgs.CompilerOptions.OptimizeFSharpAst)

        let project, ms = Performance.measure <| fun () ->
            Project.From(
                projCracked.ProjectFile,
                implFiles,
                assemblies.Value,
                ?precompiledInfo = (projCracked.PrecompiledInfo |> Option.map (fun i -> i :> _)),
                getPlugin = loadType projCracked.CliArgs
            )
        Log.always($"Fable project created in {ms}ms")

        return ProjectChecked(checker, errors, project)
    }

    member this.Update(projCracked: ProjectCracked, filesToCompile) = async {
        let! implFiles, errors, _ =
            ProjectChecked.Check(this.Checker, projCracked.ProjectFile, projCracked.SourceFiles,
                optimize = projCracked.CliArgs.CompilerOptions.OptimizeFSharpAst,
                lastFile = Array.last filesToCompile)

        let filesToCompile = set filesToCompile
        let implFiles = implFiles |> List.filter (fun f -> filesToCompile.Contains(f.FileName))
        let project = this.Project.Update(implFiles)
        return ProjectChecked(checker, errors, project)
    }

type Watcher =
    { Watcher: FsWatcher
      Subscription: IDisposable
      StartedAt: DateTime
      OnChange: ISet<string> -> unit }

    static member Create(watchDelay) =
        { Watcher = FsWatcher(watchDelay)
          Subscription = { new IDisposable with member _.Dispose() = () }
          StartedAt = DateTime.MinValue
          OnChange = ignore }

    member this.Watch(projCracked: ProjectCracked) =
        if this.StartedAt > projCracked.ProjectOptions.LoadTime then this
        else
            Log.verbose(lazy "Watcher started!")
            this.Subscription.Dispose()
            let subs =
                // TODO: watch also project.assets.json?
                this.Watcher.Observe [
                    projCracked.ProjectFile
                    yield! projCracked.References
                    yield! projCracked.SourceFiles |> Array.choose (fun f ->
                        let path = f.NormalizedFullPath
                        if Naming.isInFableModules(path) then None
                        else Some path)
                ]
                |> Observable.subscribe this.OnChange
            { this with Subscription = subs; StartedAt = DateTime.UtcNow }

type State =
    { CliArgs: CliArgs
      ProjectCrackedAndChecked: (ProjectCracked * ProjectChecked) option
      WatchDependencies: Map<string, string[]>
      PendingFiles: string[]
      DeduplicateDic: ConcurrentDictionary<string, string>
      Watcher: Watcher option
      HasCompiledOnce: bool }

    member this.RunProcessEnv =
        let nodeEnv =
            match this.CliArgs.Configuration with
            | "Release" -> "production"
            // | "Debug"
            | _ -> "development"
        [ "NODE_ENV", nodeEnv ]

    member this.TriggeredByDependency(path: string, changes: ISet<string>) =
        match Map.tryFind path this.WatchDependencies with
        | None -> false
        | Some watchDependencies -> watchDependencies |> Array.exists changes.Contains

    member this.GetPathResolver(?precompiledInfo: PrecompiledInfoImpl) =
        { new PathResolver with
            member _.TryPrecompiledOutPath(sourceDir, relativePath) =
                match precompiledInfo with
                | None -> None
                | Some precompiledInfo ->
                    let fullPath = IO.Path.Combine(sourceDir, relativePath) |> Path.normalizeFullPath
                    precompiledInfo.TryPrecompiledOutPath(fullPath)

            member _.GetOrAddDeduplicateTargetDir(importDir: string, addTargetDir) =
                // importDir must be trimmed and normalized by now, but lower it just in case
                // as some OS use case insensitive paths
                let importDir = importDir.ToLower()
                this.DeduplicateDic.GetOrAdd(importDir, fun _ ->
                    set this.DeduplicateDic.Values
                    |> addTargetDir)
        }

    static member Create(cliArgs, ?watchDelay) =
        { CliArgs = cliArgs
          ProjectCrackedAndChecked = None
          WatchDependencies = Map.empty
          Watcher = watchDelay |> Option.map Watcher.Create
          DeduplicateDic = ConcurrentDictionary()
          PendingFiles = [||]
          HasCompiledOnce = false }

let private getFilesToCompile (state: State) (changes: ISet<string>) (oldFiles: IDictionary<string, File> option) (projCracked: ProjectCracked) =
    let pendingFiles = set state.PendingFiles

    // Clear the hash of files that have changed
    let projCracked = projCracked.MapSourceFiles(fun file ->
        if changes.Contains(file.NormalizedFullPath) then
            File(file.NormalizedFullPath)
        else file)

    let filesToCompile =
        projCracked.SourceFilePaths |> Array.filter (fun path ->
            changes.Contains path
            || pendingFiles.Contains path
            || state.TriggeredByDependency(path, changes)
            // TODO: If files have been deleted, we should likely recompile after first deletion
            || (match oldFiles with Some oldFiles -> not(oldFiles.ContainsKey(path)) | None -> false)
        )
    Log.verbose(lazy $"""Files to compile:{Log.newLine}    {filesToCompile |> String.concat $"{Log.newLine}    "}""")
    projCracked, filesToCompile

let private areCompiledFilesUpToDate (state: State) (filesToCompile: string[]) =
    let pathResolver = state.GetPathResolver()
    filesToCompile
    |> Array.filter (fun file -> file.EndsWith(".fs") || file.EndsWith(".fsx"))
    |> Array.forall (fun source ->
        getOutPath state.CliArgs pathResolver source
        |> File.existsAndIsNewerThanSource source)

let private compilationCycle (state: State) (changes: ISet<string>) = async {
    let cliArgs = state.CliArgs

    let projCracked, projChecked, filesToCompile =
        match state.ProjectCrackedAndChecked with
        | None ->
            let projCracked = ProjectCracked.Init(cliArgs)
            projCracked, None, projCracked.SourceFilePaths

        | Some(projCracked, projChecked) ->
            // For performance reasons, don't crack .fsx scripts for every change
            let fsprojChanged = changes |> Seq.exists (fun c -> c.EndsWith(".fsproj"))

            if fsprojChanged then
                let oldProjCracked = projCracked
                let newProjCracked = ProjectCracked.Init(cliArgs)

                // If only source files have changed, keep the project checker to speed up recompilation
                let projChecked =
                    if oldProjCracked.ProjectOptions.OtherOptions = newProjCracked.ProjectOptions.OtherOptions
                    then Some projChecked
                    else None

                let oldFiles = oldProjCracked.SourceFiles |> Array.map (fun f -> f.NormalizedFullPath, f) |> dict
                let newProjCracked = newProjCracked.MapSourceFiles(fun f ->
                    match oldFiles.TryGetValue(f.NormalizedFullPath) with
                    | true, f -> f
                    | false, _ -> f)
                let newProjCracked, filesToCompile = getFilesToCompile state changes (Some oldFiles) newProjCracked
                newProjCracked, projChecked, filesToCompile
            else
                let projCracked, filesToCompile = getFilesToCompile state changes None projCracked
                projCracked, Some projChecked, filesToCompile

    // Update the watcher (it will restart if the fsproj has changed)
    // so changes while compiling get enqueued
    let state = { state with Watcher = state.Watcher |> Option.map (fun w -> w.Watch(projCracked)) }

    // If not in watch mode and not projCracked.CacheInvalidated, skip compilation if compiled files are up-to-date
    // NOTE: Don't skip Fable compilation in watch mode because we need to calculate watch dependencies
    if Option.isNone state.Watcher
        && not projCracked.CacheInvalidated
        && areCompiledFilesUpToDate state filesToCompile then
            Log.always "Skipped compilation because all generated files are up-to-date!"
            return state, 0
    else
        let dllPath =
            match cliArgs.Precompile, cliArgs.OutDir with
            | true, Some outDir -> PrecompiledInfoImpl.GetDllPath(outDir) |> Some
            | _ -> None

        // Start .dll assembly generation in parallel
        let! precompileLogs =
            match dllPath with
            | Some dllPath ->
                Async.StartChild(async {
                    let! diagnostics, _exitCode = ProjectChecked.CompileToFile(projCracked, dllPath)
                    return getFSharpDiagnostics diagnostics
                })
            | None -> [||] |> async.Return |> async.Return

        let! projChecked =
            match projChecked with
            | None -> ProjectChecked.Init(projCracked)
            | Some projChecked when Array.isEmpty filesToCompile -> async.Return projChecked
            | Some projChecked -> projChecked.Update(projCracked, filesToCompile)

        let logs = getFSharpDiagnostics projChecked.Diagnostics
        let hasFSharpError = logs |> Array.exists (fun l -> l.Severity = Severity.Error)

        let! logs, outPaths, state = async {
            // Skip Fable recompilation if there are F# errors, this prevents bundlers, dev servers, tests... from being triggered
            if hasFSharpError && state.HasCompiledOnce then
                return logs, Map.empty, { state with PendingFiles = filesToCompile }
            else
                Log.always "Started Fable compilation..."

                let pathResolver = state.GetPathResolver(?precompiledInfo = projCracked.PrecompiledInfo)
                let! results, ms = Performance.measureAsync <| fun () ->
                    filesToCompile
                    |> Array.filter (fun file -> file.EndsWith(".fs") || file.EndsWith(".fsx"))
                    |> Array.map (fun file ->
                        projCracked.MakeCompiler(file, projChecked.Project, state.TriggeredByDependency(file, changes))
                        |> compileFile cliArgs pathResolver)
                    |> Async.Parallel

                Log.always $"Fable compilation finished in %i{ms}ms{Log.newLine}"

                let logs, outPaths, watchDependencies =
                    ((logs, Map.empty, state.WatchDependencies), results)
                    ||> Array.fold (fun (logs, outPaths, deps) -> function
                        | Ok res ->
                            let logs = Array.append logs res.Logs
                            let outPaths = Map.add res.File res.OutPath outPaths
                            let deps = Map.add res.File res.WatchDependencies deps
                            logs, outPaths, deps
                        | Error e ->
                            let log = Log.MakeError(e.Exception.Message, fileName=e.File, tag="EXCEPTION")
                            Log.verbose(lazy e.Exception.StackTrace)
                            Array.append logs [|log|], outPaths, deps)

                let state = { state with HasCompiledOnce = true
                                         PendingFiles = [||]
                                         WatchDependencies = watchDependencies }
                return logs, outPaths, state
        }

        // Sometimes errors are duplicated
        let logs = Array.distinct logs
        do
            let filesToCompile = set filesToCompile
            for log in logs do
                match log.Severity with
                | Severity.Error -> () // We deal with errors below
                | Severity.Info | Severity.Warning ->
                    // Ignore warnings from packages in `fable_modules` folder
                    match log.FileName with
                    | Some filename when Naming.isInFableModules(filename) || not(filesToCompile.Contains(filename)) -> ()
                    | _ ->
                        let formatted = formatLog cliArgs.RootDir log
                        if log.Severity = Severity.Warning then Log.warning formatted
                        else Log.always formatted

        let errorLogs = logs |> Array.filter (fun log -> log.Severity = Severity.Error)
        errorLogs |> Array.iter (formatLog cliArgs.RootDir >> Log.error)
        let hasError = Array.isEmpty errorLogs |> not

        match hasError, dllPath with
        | false, Some dllPath ->
            Log.always($"Saving precompiled info...")
            let _, ms = Performance.measure <| fun _ ->
                let inlineExprs =
                    let file = Array.last filesToCompile
                    let com = projCracked.MakeCompiler(file, projChecked.Project)
                    let exprs = projChecked.Project.GetAllInlineExprs(com)
                    com.Logs |> logErrors cliArgs.RootDir
                    exprs

                let files =
                    projChecked.Project.ImplementationFiles |> Map.map (fun k v ->
                        match Map.tryFind k outPaths with
                        | Some outPath -> { RootModule = v.RootModule; OutPath = outPath }
                        | None -> FableError($"Cannot find out path for precompiled file {k}") |> raise)

                PrecompiledInfoImpl.Save(
                    dllPath = dllPath,
                    files = files,
                    inlineExprs = inlineExprs,
                    compilerOptions = cliArgs.CompilerOptions,
                    fableLibDir = projCracked.FableLibDir)

            Log.always($"Precompiled info saved in {ms}ms")
        | _ -> ()

        // Wait for the precompiled logs after saving the precompiled info so it can also be done in parallel
        let! precompileLogs = precompileLogs
        precompileLogs |> logErrors cliArgs.RootDir

        let exitCode, state =
            match cliArgs.RunProcess with
            // Only run process if there are no errors
            | _ when hasError -> 1, state
            | None -> 0, state
            | Some runProc ->
                let workingDir = cliArgs.RootDir

                let exeFile, args =
                    match runProc.ExeFile with
                    | Naming.placeholder ->
                        let pathResolver = state.GetPathResolver()
                        let lastFile = Array.last projCracked.SourceFiles
                        let lastFilePath = getOutPath cliArgs pathResolver lastFile.NormalizedFullPath
                        // Fable's getRelativePath version ensures there's always a period in front of the path: ./
                        let lastFilePath = Path.getRelativeFileOrDirPath true workingDir false lastFilePath
                        "node", lastFilePath::runProc.Args
                    | exeFile ->
                        File.tryNodeModulesBin workingDir exeFile
                        |> Option.defaultValue exeFile, runProc.Args

                if Option.isSome state.Watcher then
                    Process.startWithEnv state.RunProcessEnv workingDir exeFile args
                    let runProc = if runProc.IsWatch then Some runProc else None
                    0, { state with CliArgs = { cliArgs with RunProcess = runProc } }
                else
                    // TODO: When not in watch mode, run process out of this scope to free memory used by Fable/F# compiler
                    let exitCode = Process.runSyncWithEnv state.RunProcessEnv workingDir exeFile args
                    exitCode, state

        let state =
            { state with ProjectCrackedAndChecked = Some(projCracked, projChecked)
                         PendingFiles =
                            if state.PendingFiles.Length = 0 then
                                errorLogs |> Array.choose (fun l -> l.FileName) |> Array.distinct
                            else state.PendingFiles }

        return state, exitCode
}

type Msg =
    | Changes of timeStamp: DateTime * changes: ISet<string>

let startCompilation state = async {
  try
    let state =
        match state.CliArgs.RunProcess with
        | Some runProc when runProc.IsFast ->
            let workingDir = state.CliArgs.RootDir
            let exeFile =
                File.tryNodeModulesBin workingDir runProc.ExeFile
                |> Option.defaultValue runProc.ExeFile
            Process.startWithEnv state.RunProcessEnv workingDir exeFile runProc.Args
            { state with CliArgs = { state.CliArgs with RunProcess = None } }
        | _ -> state

    // Initialize changes with an empty set
    let changes = HashSet() :> ISet<_>
    let! _state, exitCode =
        match state.Watcher with
        | None -> compilationCycle state changes
        | Some watcher ->
            let agent =
                MailboxProcessor<Msg>.Start(fun agent ->
                    let rec loop state = async {
                        match! agent.Receive() with
                        | Changes(timestamp, changes) ->
                            match state.Watcher with
                            // Discard changes that may have happened before we restarted the watcher
                            | Some w when w.StartedAt < timestamp ->
                                // TODO: Get all messages until QueueLength is 0 before starting the compilation cycle?
                                Log.verbose(lazy $"""Changes:{Log.newLine}    {changes |> String.concat $"{Log.newLine}    "}""")
                                let! state, _exitCode = compilationCycle state changes
                                Log.always "Watching..."
                                return! loop state
                            | _ -> return! loop state
                    }

                    let onChange changes =
                        Changes(DateTime.UtcNow, changes) |> agent.Post

                    loop { state with Watcher = Some { watcher with OnChange = onChange } })

            // The watcher will remain active so we don't really need the reply channel
            agent.PostAndAsyncReply(fun _ -> Changes(DateTime.UtcNow, changes))

    match exitCode with
    | 0 -> return Ok()
    | _ -> return Error "Compilation failed"

  with
    | FableError e -> return Error e
    | exn -> return raise exn
}