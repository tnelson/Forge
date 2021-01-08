#lang forge

//Taken from https://github.com/haslab/Electrum2/wiki/Trash
//Goal is to model a trash bin such that files in the trash
//can be recovered, but only until the trash is emptied

var sig File {}
--var sig Trash in File {} //This feature isn't implemented yet
one sig TrashInFile {
    var Trash: set File //should work instead of var sig Trash in File {}
}

pred delete[f : File] {
    f not in TrashInFile.Trash
    (TrashInFile.Trash)' = TrashInFile.Trash + f
    File' = File
}

pred restore[f : File] {
    f in TrashInFile.Trash
    (TrashInFile.Trash)' = TrashInFile.Trash - f
    File' = File
}

pred emptyTrash {
    no ((TrashInFile.Trash)')
    File' = File - (TrashInFile.Trash)
}

pred do_nothing {
    (TrashInFile.Trash)' = TrashInFile.Trash
    File' = File
}

--Was a fact in Electrum
pred behavior {
    no TrashInFile.Trash
    always (
    --will fix indenter for this
    (some f: File | delete[f] or restore[f]) or emptyTrash or do_nothing
    )
}

--Past LTL not available yet
--General test for once
--pred restoreAfterDelete {
--    always (all f : File | restore[f] implies once delete[f])
--}

//general test for after
pred deleteAll {
    //If all files are in Trash,
    //then all files are deleted when trash is emptied
    always ((File in TrashInFile.Trash and emptyTrash) implies after always no File)
}

--check {behavior implies deleteAll}

pred restoreEnabled[f : File] {
    //files in the trash can be restored
    f in TrashInFile.Trash
}

//general test for releases
--pred restoreIsPossibleBeforeEmpty {
--    always (all f : File | delete[f] implies (emptyTrash releases restoreEnabled[f]))
--}

--check {behavior implies restoreIsPossibleBeforeEmpty}
