#lang forge

//Taken from https://github.com/haslab/Electrum2/wiki/Trash
//Goal is to model a trash bin such that files in the trash can be restored
//but only until the trash is emptied

var sig File {}
sig Trash in File {}