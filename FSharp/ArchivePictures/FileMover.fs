module FileMover

open FileSystem
open Domain
open Moves

let private ensureDirectoryExists (fileSystemsOperations:IFileSystemOperations) destination =
    let directoryExists = fileSystemsOperations.directoryExists destination
    if not (directoryExists) then
        fileSystemsOperations.createDirectory destination |> ignore

let private compareFiles (fileSystemsOperations:IFileSystemOperations) moveRequest =
    let sourceStream = fileSystemsOperations.readAllBytes moveRequest.Source
    let destinationStream = fileSystemsOperations.readAllBytes moveRequest.Destination
    sourceStream = destinationStream

let private copyToDestination (fileSystemsOperations:IFileSystemOperations) (moveRequest:MoveRequest) =
    ensureDirectoryExists fileSystemsOperations moveRequest.Destination
    fileSystemsOperations.copy moveRequest.Source moveRequest.Destination true

let private deleteSource (fileSystemsOperations:IFileSystemOperations) (move:Move) =
    fileSystemsOperations.delete move.Request.Source

let private moveFile (fileSystemsOperations:IFileSystemOperations) moveRequest =
    copyToDestination fileSystemsOperations moveRequest
    let filesMatch = compareFiles fileSystemsOperations moveRequest
    let move = match filesMatch with
                | true -> {Request = moveRequest; Result = Success}
                | false -> {Request = moveRequest; Result = Failure BytesDidNotMatch}

    deleteSource fileSystemsOperations move

let move (fileSystemsOperations:IFileSystemOperations) targetPath pictures =
    let moveFile = moveFile fileSystemsOperations

    pictures
    |> getMoveRequests targetPath
    |> Seq.map moveFile