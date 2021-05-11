#lang forge

option problem_type temporal

test expect {
    aTest : {
        always true
    } is sat
}