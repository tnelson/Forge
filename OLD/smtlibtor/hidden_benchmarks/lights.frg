#lang forge

option backend smtlibtor

abstract sig Color {}

one sig Red, Yellow, Green extends Color {}

fun colorSequence: Color -> Color {
	Color <: iden + Red->Green + Green->Yellow + Yellow->Red // does this work?
}

sig Light {}
sig LightState {color: pfunc Light -> Color}
sig Junction {lights: set Light}

fun redLights [s: LightState]: set Light { s.color.Red }

pred field_facts {
    all l : Light | all s : LightState | one s.color[l]
}

pred mostlyRed [s: LightState, j: Junction] {
	lone j.lights - redLights[s]
}

pred trans [s, s1: LightState, j: Junction] {
	lone x: j.lights | s.color[x] != s1.color[x]
	all x: j.lights |
		let step = s.color[x] -> s1.color[x] {
			step in colorSequence
			step in Red->(Color-Red) => j.lights in redLights[s]
		}
	}

pred Safe {
	all s, s1: LightState, j: Junction |
		mostlyRed [s, j] and trans [s, s1, j] => mostlyRed [s1, j]
	}

test expect {
    lights: {field_facts => Safe} is theorem
}

//assert ColorSequenceDeterministic {
//	all c: Color | lone c.colorSequence
//	}
//
//check ColorSequenceDeterministic
