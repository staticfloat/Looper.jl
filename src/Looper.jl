module Looper
export @looper, Loop, instantiate, unroll, tile, transpose

import Base: replace, transpose, ==
using MacroTools
import MacroTools: postwalk, prewalk, walk

struct Loop
    idx
    range
    body

    function Loop(idx, range, body)
        # first, unwrap (to ensure we get to the meat of the body)
        while typeof(body) <: Array && length(body) == 1
            body = body[1]
        end
        # Then wrap as a [body] if we need to
        if !(typeof(body) <: Array)
            body = [body]
        end
        return new(idx, range, body)
    end
end

function ==(l1::Loop, l2::Loop)
    return (l1.idx == l2.idx) &&
           (l1.range == l2.range) &&
           (l1.body == l2.body)
end

# Let "postwalk" and friends iterate through our Loop construct
walk(l::Loop, inner, outer) = outer(Loop(inner(l.idx), inner(l.range), inner(l.body)))
walk(a::Array, inner, outer) = outer(inner.(a))

# Lower a Loop down into normal Julia code, lower arrays of loops to
# sequential loops in normal Julia code, everything else stays the same.
build_loops(l) = l
function build_loops(a::Array)
    return quote
        $((build_loops.(a))...)
    end
end
function build_loops(l::Loop)
    return quote
        for $(l.idx) in $(l.range)
            $(build_loops(l.body))
        end
    end
end

"""
    @loops

Parse out loops into our `Loop` intermediate representation.  This macro is able to
parse nested loops, e.g.:

    l = @loop for x in 1:10
        for y in 2:50
            ...
        end
    end

As well as condensed loops:

    l = @loop for x in 1:10, y in 2:50
        ...
    end

The two constructions are equivalent.  However, note that the condensed format will
parse only up to three indices per for loop at a time.  This is an arbitrary limit.

See the docstring for `instantiate()` for what to do with `l` once it is captured.
"""
macro looper(ex)
    return postwalk(
        x ->
        @capture(x, for idx_ in range_ body__ end) ?
            Loop(idx, range, body) : 
            @capture(x, for idx1_ in range1_, idx2_ in range2_ body__ end) ?
                Loop(idx1, range1, Loop(idx2, range2, body)) :
                @capture(x, for idx1_ in range1_, idx2_ in range2_, idx3_ in range3_ body__ end) ?
                    Loop(idx1, range1, Loop(idx2, range2, Loop(idx3, range3, body))) :
                    x,
        ex
    )
end

"""
    instantiate(l::Loop; verbose::Bool = false, preoptimize::Bool = true)

Helper function to lower a `Loop` down into normal Julia code.
As an example, for the `Loop` object `l = Loop(:x, 1:10, :(println("\$(x)")))`,
@instantiate(l) would result in something similar to:

    for x in 1:10
        println("\$(x)")
    end

The typical workflow for this package is to capture a loop with `@looper`,
manipulate it via `unroll()`, `transpose()`, `tile()`, etc... and then finally
call `instantiate()` within an `@eval`'ed function.  Full example:

    l = @looper for i in 1:size(x, 1),
                    j in 1:size(x, 2)
        total += x[j, i] * x[i, j]
    end

    @eval function unrolled_nonsense(x)
        total = 0
        \$(instantiate(unroll(l, :j, 4))
        return total
    end
"""
function instantiate(l; verbose::Bool = false, preoptimize::Bool = true)
    if preoptimize
        # Lower constant loop definitions as much as possible
        l = lower_constant_ranges(l)
        # Lower `first(a:b)` -> `a`, since it's so common in our code
        l = lower_constant_firsts(l)
        # Eliminate dead loops
        l = eliminate_dead_loops(l)
    end

    # Build function body from loop definitions
    func_body = build_loops(l)
    # Eliminate unnecessary blocks
    func_body = prewalk(e -> MacroTools.unblock(e), func_body)
    if verbose
        @show func_body
    end
    return func_body
end

function eliminate_dead_loops(l::Loop)
    # If any ranges statically evaluate down to an empty container,
    # we replace that loop with `nothing`.  We then go through any
    # Loop and drop elements from its body-vector that are === nothing
    postwalk(l -> begin
        if isa(l, Loop)
            try
                if isempty(eval(l.range))
                    return nothing
                end
            catch
            end
            # If it wasn't empty, then filter out all body elements that are not nothing
            filtered_body = [b for b in l.body if b !== nothing]

            # If that makes this body empty, then eliminate this loop as well
            if isempty(filtered_body)
                return nothing
            end

            # Otherwise, continue on as a loop.
            return Loop(l.idx, l.range, filtered_body)
        end
        return l
    end, l)
end
eliminate_dead_loops(ls::Vector) = eliminate_dead_loops.(ls)

function lower_constant_ranges(l::Loop)
    # We take an incredibly cavalier attitude toward this.  We just try to evaluate
    # every range, and if we run into problems we silently continue.
    postwalk(l -> begin
        if isa(l, Loop)
            try
                l = Loop(l.idx, eval(l.range), l.body)
            catch
            end
        end
        return l
    end, l)
end
lower_constant_ranges(ls::Vector) = lower_constant_ranges.(ls)

function lower_constant_firsts(l::Loop)
    # We lower first(a:b) -> a, and first(a:b:c) -> a, but only
    # if we can directly evalutate them.  Same calavier attitude
    # as above.
    postwalk(l -> begin
        if @capture(l, first(a_:b_)) || @capture(l, first(a_:b_:c_))
            try
                return eval(a)
            catch
            end
        end
        return l
    end, l)
end
lower_constant_firsts(ls::Vector) = lower_constant_firsts.(ls)


"""
    find_loop(l, idx::Symbol)

Given a `Loop` nest, search through the loops for the first loop that has an
index that matches the given symbol.
"""
function find_loop(l, idx::Symbol)
    loop = nothing
    postwalk(l -> begin
        if isa(l, Loop) && l.idx == idx
            loop = l
        end
        return l
    end, l)
    return loop
end


# Helper function to find an `index` within `body` and replace it with `rep`.
# Used for things like unrolling, where we find `l.idx` and replace it with `l.idx + offset`.
replace(body, idx, rep) = postwalk(elem -> elem == idx ? rep : elem, body)
shift_body(body, idx, offset) = replace(body, idx, :($(idx) + $(offset)))

function unroll_range(u::OrdinalRange, factor::Int)
    return 0:(step(u)*factor):(div(length(u),factor)*factor - 1)
end
function unroll_range_tail(u::OrdinalRange, factor::Int)
    return (div(length(u), factor)*factor + first(u)):last(u)
end

# Unroll the given index by a factor
"""
    unroll(l::Loop, idx::Symbol, factor::Int)

Unroll a given index variable by the given factor.  Unrolling a loop
involves constructing a loop that runs `1/factor` as many times, but
contains `factor` many iterations manually instantiated within it, each
shifted by `1:factor` amounts in the index being unrolled.
"""
function unroll(l::Loop, idx::Symbol, factor::Int; no_tails::Bool = false)
    return postwalk(l -> begin
        # If this loop index matches the one we're looking for
        if (isa(l, Loop) && l.idx == idx)
            # Construct the unrolled body
            unrolled_body = [
                replace(l.body, l.idx, :(first($(l.range)) + $(l.idx) + $(offset))) for offset in 0:(factor - 1)
            ]
            
            # We replace this current Loop with two; one for the unrolled loop and
            # one to catch any remainder iterations needed, unless `no_tails` is
            # set to `true`, in which case we assume (potentially disastrously!)
            # that the unroll factor perfectly divides the loop domain.
            unrolled_loop = Loop(l.idx, :(Looper.unroll_range($(l.range), $(factor))), unrolled_body)
            tail = Loop(l.idx, :(Looper.unroll_range_tail($(l.range), $(factor))), l.body)
            
            if no_tails
                return [unrolled_loop]
            else
                return [unrolled_loop, tail]
            end
        else
            return l
        end
    end, l)
end
unroll(ls::Vector, idx::Symbol, factor::Int) = unroll.(ls, idx, factor)

raw"""
Test case:

using Looper
l = @looper for x in 1:10, y in 1:10
    println("$(x) $(y)")
end
z = instantiate(l; verbose=true)
z()
z = instantiate(unroll(l, :y, 4); verbose=true)
z() # This should output the same thing as the first time
z = instantiate(unroll(unroll(l, :y, 4), :x, 8); verbose=true)
z() # same here
"""

"""
    transpose(l::Loop, idx1::Symbol, idx2::Symbol)

Transpose two loops, to change iteration order.
"""
function transpose(l::Loop, idx1::Symbol, idx2::Symbol)
    # Find the loops that contain these symbols
    loop1 = find_loop(l, idx1)
    loop2 = find_loop(l, idx2)

    if loop1 === nothing
        throw(ArgumentError("Unable to find index variable '$idx1'"))
    end
    if loop2 === nothing
        throw(ArgumentError("Unable to find index variable '$idx2'"))
    end

    # Now, swap inner loop then outer loop.
    l = postwalk(x -> begin
        if !isa(x, Loop)
            return x
        end
        if x.idx == loop1.idx
            return Loop(loop2.idx, loop2.range, x.body)
        elseif x.idx == loop2.idx
            return Loop(loop1.idx, loop1.range, x.body)
        else
            return x
        end
    end, l)

    return l
end

transpose(ls::Vector, idx1::Symbol, idx2::Symbol) = transpose.(ls, idx1, idx2)

raw"""
# Test case:

using Looper
l = @loops for x in 1:10, y in 1:10
    println("$(x) $(y)")
end
z = instantiate(l)
z()
zt = instantiate(transpose(l, :x, :y))
zt() # This should output transposed ordering
"""

function tile_range_outer(u::OrdinalRange, factor::Int)
    return (first(u) - 1):(step(u)*factor):(last(u) - factor)
end
function tile_range_inner(u::OrdinalRange, factor::Int)
    return 0:step(u):min(factor-1, last(u)-1)
end
function tile_range_remainder(u::OrdinalRange, factor::Int)
    return 0:(last(u) - last(tile_range_outer(u, factor)) - factor - 1)
end

# Just until MacroTools gets patched...
function inexpr(ex, x)
    result = false
    MacroTools.postwalk(ex) do y
        if y == x
            result = true
        end
        return y
    end
    return result
end

"""
    tile(l::Loop, x_var::Symbol, y_var::Symbol, x_size::Int, y_size::Int)

Tile two nested loops in tiles of size `x_size` and `y_size`.  Note that the `y`
loop _must_ be contained within the `x` loop.  (E.g. `x` must be the outer loop).
"""
function tile(l::Loop, x_var::Symbol, y_var::Symbol, x_size::Int, y_size::Int)
    # A tiled loop contains four nested loops, an inner and outer loop for the x and y variables
    x_outer = Symbol("$(x_var)_outer")
    x_inner = Symbol("$(x_var)_inner")
    y_outer = Symbol("$(y_var)_outer")
    y_inner = Symbol("$(y_var)_inner")

    # Find the loops
    x_loop = find_loop(l, x_var)
    y_loop = find_loop(l, y_var)
    
    # Enforce x as the outer loop
    if inexpr(y_loop, x_loop)
        l = transpose(l, :x, :y)
        x_loop = find_loop(l, x_var)
        y_loop = find_loop(l, y_var)
    end

    # We always make y as the inner loop, so we always use its body.
    body = y_loop.body

    # Rewrite index references in the body to use our tiled index calculation system
    body = replace(body, x_var, :($(x_outer) + $(x_inner) + first($(x_loop.range))))
    body = replace(body, y_var, :($(y_outer) + $(y_inner) + first($(y_loop.range))))

    return [
        # Loop over all columns by tile
        Loop(x_outer, :(Looper.tile_range_outer($(x_loop.range), $(x_size))),
            [
                # Loop over all tiles within this column
                Loop(y_outer, :(Looper.tile_range_outer($(y_loop.range), $(y_size))),
                    # Fill out every element within this tile
                    Loop(x_inner, :(Looper.tile_range_inner($(x_loop.range), $(x_size))),
                        Loop(y_inner, :(Looper.tile_range_inner($(y_loop.range), $(y_size))),
                            # This is, ideally, the hottest part of the code
                            body,
                        )
                    )
                ),
                # Having finished all full tiles within a "column", we must next 
                # compute the final (partial) tile within this column.  This also
                # means we must constrain the inner loop ranges to compute a partial
                # tile, rather than a full tile by using `tile_range_remainder()`
                Loop(y_outer, :(last(Looper.tile_range_outer($(y_loop.range), $(y_size))) + $(y_size)),
                    Loop(x_inner, :(Looper.tile_range_inner($(x_loop.range), $(x_size))),
                        Loop(y_inner, :(Looper.tile_range_remainder($(y_loop.range), $(y_size))),
                            body,
                        )
                    )
                )
            ]
        ),
        # Having finished all full columns, we must next compute the final (partial)
        # tiles all along the last partial column.
        Loop(x_outer, :(last(Looper.tile_range_outer($(x_loop.range), $(x_size))) + $(x_size)),
            Loop(y_outer, :(Looper.tile_range_outer($(y_loop.range), $(y_size))),
                Loop(x_inner, :(Looper.tile_range_remainder($(x_loop.range), $(x_size))),
                    Loop(y_inner, :(Looper.tile_range_inner($(y_loop.range), $(y_size))),
                        body,
                    )
                )
            )
        ),
        # And finally, the black-sheep tile which is restricted in both x and y
        Loop(x_outer, :(last(Looper.tile_range_outer($(x_loop.range), $(x_size))) + $(x_size)),
            Loop(y_outer, :(last(Looper.tile_range_outer($(y_loop.range), $(y_size))) + $(y_size)),
                Loop(x_inner, :(Looper.tile_range_remainder($(x_loop.range), $(x_size))),
                    Loop(y_inner, :(Looper.tile_range_remainder($(y_loop.range), $(y_size))),
                        body,
                    )
                )
            )
        ),
    ]
end
tile(l::Vector, x_var::Symbol, y_var::Symbol, x_size::Int, y_size::Int) =
    tile.(l, x_var, y_var, x_size, y_size)

raw"""
# Test case:

using Looper
l = @looper for x in 1:10, y in 1:10
    println("$(x) $(y)")
end
z = instantiate(l)
z()

ltiled = tile(l, :x, :y, 4, 4)
#ztiled = instantiate(ltiled; verbose=true)
#ztiled() # This should output tiled ordering
ztiled_unrolled = instantiate(unroll(ltiled, :y_inner, 4); verbose=true)
ztiled_unrolled() # Oh baby can it be?!
"""

end