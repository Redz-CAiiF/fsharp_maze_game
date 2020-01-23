module FMaze.Program
open FMaze.Core
open globals

//to do
//A* to resolve the maze
//printing the maze with the highlited solution path

[<EntryPoint>]
let main argv =
    //old printing functions are used for printing the maze. TODO printing functions with the given GUI Engine library.    
    let map = Maze.create ROWS COLUMNS
    let expanded_map = Maze.Expand.convert_maze_to_expandedmaze map

    printing.print_map expanded_map
    
    let resolution = maze_resolutor.Resolutor.resolve expanded_map

    printfn "%A" resolution

    System.Console.ReadKey() |> ignore
    0 // return exit code




    //sta roba mi da errore
    (*
    
<!-- ************************************************************************* -->
<!-- *************************** GenerateMetadata Phase ********************** -->
<!-- ************************************************************************* -->
<Target Name="DocGenerateMetadata" Condition="'$(BuildDocFx)' == 'true'">
  <PropertyGroup>
    <DocGenerateCommand>&quot;$(BuildDocToolPath)&quot; &quot;$(DocfxConfigFile)&quot; -o &quot;$(MetadataOutputFolder)&quot; -l &quot;$(LogFile)&quot; --logLevel &quot;$(LogLevel)&quot;</DocGenerateCommand>
    <DocGenerateCommand Condition="$(RebuildDoc)">$(DocGenerateCommand) -f</DocGenerateCommand>
    <DocGenerateCommand Condition="'$(DocTemplate)' != ''">$(DocGenerateCommand) --template &quot;$(DocTemplate)&quot; </DocGenerateCommand>
    <DocGenerateCommand Condition="'$(DocParameters)' != ''">$(DocGenerateCommand) $(DocParameters)</DocGenerateCommand>
  </PropertyGroup>
  <Message Condition="!Exists($(DocfxConfigFile))" Text="Init docfx config files" />
  <Exec Condition="!Exists($(DocfxConfigFile))" Command="&quot;$(BuildDocToolPath)&quot; init -o &quot;$(MSBuildProjectDirectory)&quot; -q --apiGlobPattern **.csproj --apiSourceFolder &quot;$(MSBuildProjectDirectory)&quot;" />
  <Message Text="Executing $(DocGenerateCommand)" />
  <Exec Command="$(DocGenerateCommand)"></Exec>
</Target>
</Project>
    
    *)