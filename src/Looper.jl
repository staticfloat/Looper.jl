module Looper
export @looper, Loop, instantiate, unroll, split, tile, transpose

import Base: replace, transpose, ==, split
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

# Find all loops that match a given index
function find_loops(l, idx::Symbol)
    loops = []
    postwalk(l -> begin
        if isa(l, Loop) && l.idx == idx
            push!(loops, l)
        end
        return l
    end, l)
    return loops
end

# Remove all loops that match a given index
function kill_loops(l, idx::Symbol)
    return postwalk(l -> begin
        if isa(l, Loop) && l.idx == idx
            return nothing
        end
        return l
    end, l)
end

function find_outer_inner_idx(l, idx1::Symbol, idx2::Symbol)
    # Figure out which is outer:
    num_idx2_in_idx1 = sum(length(find_loops(sl, idx2)) for sl in find_loops(l, idx1))
    num_idx1_in_idx2 = sum(length(find_loops(sl, idx1)) for sl in find_loops(l, idx2))

    # If neither contains the other, we can't transpose
    if num_idx2_in_idx1 == 0 && num_idx1_in_idx2 == 0
        throw(ArgumentError("Cannot find any nested loops with indices $(repr(idx1)) and $(repr(idx2))"))
    end
    # If we have an (x -> y -> x) situation (why on earth would you even do that)
    # then give up now because we'll break badly.
    if num_idx2_in_idx1 != 0 && num_idx1_in_idx2 != 0
        throw(ArgumentError("Cannot separate mutually contained indices!"))
    end

    if num_idx1_in_idx2 != 0
        return idx2, idx1
    else
        return idx1, idx2
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
        display(func_body)
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

# Helper function to replace something within a block of parsed code.
# We special-case giving it a single `Loop` to return a `Vector{Loop}`
# so that we can actually replace the loop itself if we need to.
replace(body, val_old, val_new) = postwalk(x -> x == val_old ? val_new : x, body)
replace(body::Loop, val_old, val_new) = replace([body], val_old, val_new)

# shift_body is used to replace index expressions.  Very useful.
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
                shift_body(l.body, l.idx, :(first($(l.range)) + $(offset))) for offset in 0:(factor - 1)
                #replace(l.body, l.idx, :(first($(l.range)) + $(l.idx) + $(offset))) for offset in 0:(factor - 1)
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

"""
    transpose(l::Loop, idx1::Symbol, idx2::Symbol)

Transpose two loops, to change iteration order.  This method only makes
sense if one index contains the other.  Example:

    Loop(:x, ..., [
        Loop(:z, ..., [
            Loop(:y, range1, ...),
            Loop(:y, range2, ...),
        ])
    ])

And we transpose(l, :x, :y) then we should end up with:

    [
        Loop(:y, range1, [
            Loop(:z, ..., [
                Loop(:x, ...),
            ])
        ]),
        Loop(:y, range2, [
            Loop(:z, ..., [
                Loop(:x, ...),
            ])
        ]),
    ])
"""
function transpose(l::Loop, idx1::Symbol, idx2::Symbol)
    outer_idx, inner_idx = find_outer_inner_idx(l, idx1, idx2)
    
    # Now, let us find all inner loops contained within each outer loop
    outer_loops = find_loops(l, outer_idx)
    for outer_loop in outer_loops
        # Create an outer loop for each inner loop
        new_outer_loops = []
        inner_loops = find_loops(outer_loop, inner_idx)
        for inner_loop in inner_loops
            # For each inner loop, we take its body and wrap it with
            # a new loop that uses the outer loops body, but we also
            # need to eliminate any other loops within this outer_loop's
            # body that match.
            push!(new_outer_loops, Loop(
                inner_loop.idx,
                inner_loop.range,
                kill_loops(replace(outer_loop.body,
                    inner_loop,
                    Loop(
                        outer_loop.idx,
                        outer_loop.range,
                        inner_loop.body
                    )
                ), inner_idx)
            ))
        end

        # Now replace that outer loop with these new outer loops
        l = replace(l, outer_loop, new_outer_loops)
    end

    return l
end

transpose(ls::Vector, idx1::Symbol, idx2::Symbol) = transpose.(ls, idx1, idx2)


function split_range_outer(u::OrdinalRange, factor::Int)
    return (first(u) - 1):(step(u)*factor):(last(u) - factor)
end
function split_range_inner(u::OrdinalRange, factor::Int)
    return 0:step(u):min(factor-1, last(u)-1)
end
function split_range_remainder(u::OrdinalRange, factor::Int)
    return 0:(last(u) - last(split_range_outer(u, factor)) - factor - 1)
end


"""
    split(l::Loop, var::Symol, size::Int)

Slice a loop into two nested loops, the inner of which is of size `size`.
This is like a 1-dimensional tiling.
"""
function split(l::Loop, var::Symbol, size::Int)
    return postwalk(x -> begin
        if !isa(x, Loop)
            return x
        end

        # Did we find the loop we're interested in?
        if x.idx == var
            outer = Symbol("$(var)_outer")
            inner = Symbol("$(var)_inner")

            # If so, split; we generate two loops, each nested: A loop that
            # contains inner/outer pairs, then a loop at the end that
            # contains whatever extra cleanup we need to due to dividing
            # the loop by a `size` that is not a perfect multiple.
            body = replace(x.body, var, :($(outer) + $(inner) + first($(x.range))))
            return [
                Loop(outer, :(Looper.split_range_outer($(x.range), $(size))),
                    Loop(inner, :(Looper.split_range_inner($(x.range), $(size))), body)
                ),
                Loop(outer, :(last(Looper.split_range_outer($(x.range), $(size))) + $(size)),
                    Loop(inner, :(Looper.split_range_remainder($(x.range), $(size))), body)
                ),
            ]
        else
            return x
        end
    end, l)
end
split(ls::Vector, var::Symbol, size::Int) = split.(ls, var, size)


"""
    tile(l::Loop, x_var::Symbol, y_var::Symbol, x_size::Int, y_size::Int)

Tile two nested loops in tiles of size `x_size` and `y_size`.
"""
function tile(l::Loop, x_var::Symbol, y_var::Symbol, x_size::Int, y_size::Int)
    outer_idx, inner_idx = find_outer_inner_idx(l, x_var, y_var)
    outer_size, inner_size = (x_var == outer_idx) ? (x_size, y_size) : (y_size, x_size)

    l = split(split(l, outer_idx, outer_size), inner_idx, inner_size)
    return transpose(l, Symbol("$(outer_idx)_inner"), Symbol("$(inner_idx)_outer"))
end
tile(l::Vector, x_var::Symbol, y_var::Symbol, x_size::Int, y_size::Int) =
    tile.(l, x_var, y_var, x_size, y_size)

end