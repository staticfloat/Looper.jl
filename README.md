# Looper

This package was born out of a desire to play around with loop tiling and unrolling in native Julia code.  Rather than define a domain-specific-language to make loop definitions easier/implicit (such as in [Halide](http://halide-lang.org/) or [`Tokamak.jl`](https://github.com/MikeInnes/Tokamak.jl)), `@staticfloat` wanted to see how much progress he could make in a day or two by just pushing native Julia code through the [`MacroTools.jl`](https://github.com/MikeInnes/MacroTools.jl) cheese grater.

It turns out, as long as you don't mind creating tools to rapidly shoot yourself in the foot, you can make decent progress.  This package has no problem altering your loops in such a way as to cause strange and undefined behavior.  It works by parsing loops into a kind of interemdiate representation (the `Looper.Loop` type) via the `@looper` macro:

```julia
using Looper
l = @looper for idx in 1:length(x)
    x[idx] = x[idx].^2
end
```

The variable `l` is now a parsed representation of the `for` loop defined above.  To actually run the loop, you `instantiate()` the `Loop` type wihin an `@eval`'ed function:

```julia
@eval function my_loop(x)
    $(instantiate(l))
end
```

Inspecting the value of `x` shows that it is indeed the correct result.

"So what?", you object.  "You've invented a complex way of doing the exact same thing that I could otherwise have done without this package by just writing a naked `for` loop."  Indeed this is true, however it's what you can do to the `Loop` object between parsing and instantiation that really causes it to shine:

```julia
@eval function my_unrolled_loop(x)
    $(instantiate(unroll(l, :x, 4)))
end
```

This causes all loops within `l` that use the index variable `x` to be unrolled by a factor of `4`.

## A more complex example

This one taken straight from [the tests](test/runtests.jl):

```julia
using Looper
l = @looper for x in 1:size(touchstone, 1)
    for y in 1:size(touchstone, 2)
        touchstone[x, y] = touch_count
        touch_count += 1
    end
end

function big_daddy(touchstone)
    touch_count = 0
    $(instantiate(unroll(tile(l, :x, :y, 4, 4), :y_inner, 4); verbose=true))
    return touchstone
end
```

This makes use of a few things; it demonstrats that you can have arbitrarily-nested `for` loops with a single `@looper` macro, it shows that you can compose loop transformations (such as a `tile()` followed by an `unroll()`, using the implicitly-created `y_inner` loop index) and it shows that there is a `verbose` keyword argument to `instantiate()` that lets you inspect the generated Julia code before it gets handed off to LLVM.  Running this instantiated code is similar to all previous examples:

```julia
using Test
@test big_daddy(zeros(Int64, 5, 5)) == [
    0   1   2   3  16;
    4   5   6   7  17;
    8   9  10  11  18;
   12  13  14  15  19;
   20  21  22  23  24;
]
```
