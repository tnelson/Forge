#lang forge

option run_sterling off


option verbose 0
option problem_type temporal

test expect {
    aTest : {
        always true
    } is sat
}