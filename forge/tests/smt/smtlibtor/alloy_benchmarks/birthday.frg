#lang forge

option backend smtlibtor
option verbose 0

/*
 * Birthday Book
 *
 * A classic Z example to explain the basic form of an Alloy model. For the original,
 * see J.M. Spivey, The Z Notation, Second Edition, Prentice Hall, 1992.
 *
 * A birthday book has two fields: known, a set of names (of persons whose birthdays are known),
 * and date, a function from known names to dates. The operation AddBirthday adds an association
 * between a name and a date; it uses the relational override operator (++), so any existing
 * mapping from the name to a date is replaced. DelBirthday removes the entry for a given name.
 * FindBirthday obtains the date d for a name n. The argument d is declared to be optional (that is,
 * a singleton or empty set), so if there is no entry for n, d will be empty. Remind gives the set
 * of names whose birthdays fall on a particular day.
 *
 * The assertion AddWorks says that if you add an entry, then look it up, you get back what you
 * just entered. DelIsUndo says that doing DelBirthday after AddBirthday undoes it, as if the add
 * had never happened. The first of these assertions is valid; the second isn't.
 *
 * The function BusyDay shows a case in which Remind produces more than one card.
 *
 * author: Daniel Jackson, 11/14/01
 */

sig Name {}
sig Date {}
sig BirthdayBook {known: set Name, date: pfunc Name -> Date}

pred field_facts {
    all n : Name | n in BirthdayBook.known => (n in (BirthdayBook.date).Date) and (one n.(BirthdayBook.date))
}

pred AddBirthday [bb, bb1: BirthdayBook, n: Name, d: Date] {
    bb1.date = bb.date ++ (n->d)
}

pred DelBirthday [bb, bb1: BirthdayBook, n: Name] {
    bb1.date = bb.date - (n->Date)
    }

pred FindBirthday [bb: BirthdayBook, n: Name, d: Date] {
    d = bb.date[n]
    }

pred Remind [bb: BirthdayBook, today: Date, cards: Name] {
    cards = (bb.date).today
    }

pred InitBirthdayBook [bb: BirthdayBook] {
    no bb.known
    }

pred AddWorks {
    all bb, bb1: BirthdayBook, n: Name, d: Date, d1: Date |
        AddBirthday [bb,bb1,n,d] && FindBirthday [bb1,n,d1] => d = d1
    }

test expect {
    birthday: {field_facts => AddWorks} is checked
}

