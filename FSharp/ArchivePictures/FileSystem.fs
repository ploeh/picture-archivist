module FileSystem

open System.IO

type IFileSystemOperations = 
    abstract member copy: string -> string -> bool -> unit
    abstract member delete: string -> unit
    abstract member readAllBytes: string -> byte[]
    abstract member createDirectory: string -> DirectoryInfo
    abstract member directoryExists: string -> bool

type FileSystemOperations () =
    interface IFileSystemOperations with
        member this.copy source destination overwrite =
            File.Copy(source, destination, overwrite)

        member this.delete path =
            File.Delete path

        member this.readAllBytes path =
            File.ReadAllBytes path

        member this.createDirectory path =
            let fileInfo = FileInfo path
            Directory.CreateDirectory fileInfo.DirectoryName

        member this.directoryExists path =
            let fileInfo = FileInfo path
            fileInfo.Directory.Exists