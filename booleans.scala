def loop: Boolean = loop

def and(x: Boolean, y: Boolean) = if (x) y else false
def and2(x: Boolean, y: => Boolean) = if (x) y else false

// prints some values
println(and(true, false))
println(and(true, true))
println(and2(true, false))
println(and2(true, true))

// call by name version returns false
println(and2(false, loop))

// call by value name enter in infinite loop
println(and(false, loop))

