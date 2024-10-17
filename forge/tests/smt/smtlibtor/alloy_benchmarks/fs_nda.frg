#lang forge

option backend smtlibtor
option verbose 0

/*
 * Model of a generic file system.
 */

abstract sig Object {}

sig Name {}

sig File extends Object {} 
pred file_facts {
    all f: File | {
        some d : Dir | f in d.entries.contents
    }
}

sig Dir extends Object {
  entries: set DirEntry,
  parent: lone Dir
} 
// {
//   parent = this.~@contents.~@entries
//   all e1, e2 : entries | e1.name = e2.name => e1 = e2
//   this !in this.^@parent
//   this != Root => Root in this.^@parent
// }

pred dir_facts {
    all d : Dir | {
        d.parent = d.(~contents).(~entries)
        all e1, e2 : d.entries | e1.name = e2.name => e1 = e2
        d !in d.^parent
        d != Root => Root in d.^parent
    }
}

one sig Root extends Dir {}
pred root_facts {
    all r : Root | {
        no r.parent
    }
}

lone sig Cur extends Dir {}

sig DirEntry {
  name: one Name,
  contents: one Object
}
pred dir_entry_facts {
    all d : DirEntry | {
        one d.~entries
    }
}


/**
 * all directories besides root have one parent
 */
pred OneParent_buggyVersion {
    all d: Dir - Root | one d.parent
}

/**
 * all directories besides root have one parent
 */
pred OneParent_correctVersion {
    all d: Dir - Root | (one d.parent && one contents.d)
}

/**
 * Only files may be linked (that is, have more than one entry)
 * That is, all directories are the contents of at most one directory entry
 */
pred NoDirAliases {
    all o: Dir | lone o.~contents
}

pred prop{
	OneParent_correctVersion => NoDirAliases
}

pred model_facts {
    file_facts and dir_facts and root_facts and dir_entry_facts
}

//check { OneParent_buggyVersion => NoDirAliases } for 5 expect 1

test expect {
    fs_sd: {model_facts => prop} is theorem
}
