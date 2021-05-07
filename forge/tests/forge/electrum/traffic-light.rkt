#lang forge

option verbose 0
option problem_type temporal

sig TrafficLight {
    //all possible colors of the light
    color : set Color,
    //the current color (can change over time)
    var current : set Color
}

sig Color {
    next : set Color
}

pred validColorNext {
    //a color cannot be succeeded by itself
    no (iden & next)
    //Color succession is a ring
    all c : Color | (Color in c.^next) and (one c.next)
}

pred validCurrentColor {
    all tl : TrafficLight | {
        always (
        //traffic lights have one color at a time
        //the successor is always the next color
        one tl.color and ((tl.current)' = tl.current.next)
        )
    }
}

pred behavior {
    always validColorNext
    always validCurrentColor
}


test expect GeneralEventuallyTest {
    someLightReachesAllColorsEventually : {
        behavior
        all tl : TrafficLight |
            all c : Color | always eventually c in tl.current
    } is sat
    allLightsReachAllColorsEventually : {
        behavior
        not (all tl : TrafficLight |
            all c : Color | always eventually c in tl.current
            )
    } is unsat
}
