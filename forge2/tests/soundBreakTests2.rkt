#lang forge

sig A { r: A }

fact r: func

<instance bitwidth="4" maxseq="4" command="Run run$1 for 5" filename="/Applications/Untitled 1.als">
    <sig label="seq/Int" ID="0" parentID="1" builtin="yes"></sig>
    <sig label="Int" ID="1" parentID="2" builtin="yes"></sig>
    <sig label="String" ID="3" parentID="2" builtin="yes"></sig>
    <sig label="univ" ID="2" builtin="yes"></sig>
    <sig label="A" ID="4" parentID="2">
        <atom label="A0"/>
        <atom label="A1"/>
        <atom label="A2"/>
        <atom label="A3"/>
        <atom label="A4"/>
        <atom label="A5"/>
        <atom label="A6"/>
        <atom label="A7"/>
        <atom label="A8"/>
        <atom label="A9"/>
    </sig>
    <field label="r" ID="5" parentID="4">
        <tuple><atom label="A0"/><atom label="A9"/></tuple>
        <tuple><atom label="A1"/><atom label="A9"/></tuple>
        <tuple><atom label="A2"/><atom label="A8"/></tuple>
        <tuple><atom label="A3"/><atom label="A7"/></tuple>
        <tuple><atom label="A4"/><atom label="A6"/></tuple>
        <tuple><atom label="A5"/><atom label="A5"/></tuple>
        <tuple><atom label="A6"/><atom label="A9"/></tuple>
        <tuple><atom label="A7"/><atom label="A9"/></tuple>
        <tuple><atom label="A8"/><atom label="A8"/></tuple>
        <tuple><atom label="A9"/><atom label="A7"/></tuple>
        <types><type ID="4"/><type ID="4"/></types>
    </field>
</instance>

run {} for exactly 10 A