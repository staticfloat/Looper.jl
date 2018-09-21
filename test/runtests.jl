using Looper, Test

# Start with basic functionality
@testset "Range Utilities" begin
    # Unroll range tests
    @test isempty(collect(Looper.unroll_range(1:3, 4)))
    @test collect(Looper.unroll_range(1:4, 4)) == [0]
    @test isempty(collect(Looper.unroll_range_tail(1:4, 4)))

    @test collect(Looper.unroll_range(1:8, 4)) == [0, 4]
    @test isempty(collect(Looper.unroll_range_tail(1:8, 4)))

    @test collect(Looper.unroll_range(1:10, 4)) == [0, 4]
    @test collect(Looper.unroll_range_tail(1:10, 4)) == [9, 10]

    @test collect(Looper.unroll_range(2:10, 4)) == [0, 4]
    @test collect(Looper.unroll_range_tail(2:10, 4)) == [10]

    # Tile range tests
    @test collect(Looper.tile_range_outer(1:4, 4)) == [0]
    @test collect(Looper.tile_range_outer(1:8, 4)) == [0, 4]
    @test collect(Looper.tile_range_outer(1:10, 4)) == [0, 4]
    @test collect(Looper.tile_range_inner(1:3, 4)) == [0, 1, 2]
    @test collect(Looper.tile_range_inner(1:4, 4)) == [0, 1, 2, 3]
    @test collect(Looper.tile_range_inner(1:10, 4)) == [0, 1, 2, 3]

    @test collect(Looper.tile_range_remainder(1:4, 4)) == []
    @test collect(Looper.tile_range_remainder(1:5, 4)) == [0]
    @test collect(Looper.tile_range_remainder(1:10, 4)) == [0, 1]
end

# Test loop capture macro
@testset "Loop capture" begin
    # Test basic capture works
    l = @looper for x in 1:10
        x
    end
    @test l.idx == :x
    @test l.range == :(1:10)
    @test l.body == [:x]

    # Test manual construction and equality works
    @test l == Loop(:x, :(1:10), :x)

    # Test nested capture and condensed-form capture
    l1 = @looper for x in 5:50
        for y in -10:5:20
            for z in 1:1
                x = z * y
                println(x, y)
            end
        end
    end
    l2 = @looper for x in 5:50, y in -10:5:20, z in 1:1
        x = z * y
        println(x, y)
    end

    # Perform the same tests on both captures
    for lx in [l1, l2]
        @test lx.idx == :x
        @test lx.range == :(5:50)
        @test length(lx.body) == 1
        @test isa(lx.body[1], Loop)
        ly = lx.body[1]
        @test ly.idx == :y
        @test ly.range == :(-10:5:20)
        @test length(ly.body) == 1
        @test isa(ly.body[1], Loop)
        lz = ly.body[1]
        @test lz.idx == :z
        @test lz.range == :(1:1)
        @test length(lz.body) == 2
        @test all(isa.(lz.body, Expr))
        @test lz.body[1] == :(x = z * y)
        @test lz.body[2] == :(println(x, y))
    end
end

@testset "Argument Detection" begin
    # Test that we can figure out when we're referencing something
    # defined outside of the loop.
    x_out = [1.0,2.0,3.0,4.0]
    alpha = 1.5
    N = length(x_out)
    l = @looper for idx in 1:N
        x_out[idx] *= 2*alpha
        alpha += idx
    end

    args = Looper.unknown_args(l)
    @test length(args) == 3
    @test :x_out in args
    @test :alpha in args
    @test :N in args
end


@testset "Loop instantiation" begin
    # Test simple loop storage
    x_out = Int64[]
    l = @looper for x in 1:10
        push!(x_out, x*x)
    end
    func = instantiate(l)
    func(x_out = x_out)
    @test x_out == collect(1:10).^2

    # Test more complex double-loop storage
    x_out = zeros(Int64, 10, 10)
    l = @looper for xidx in 1:10, yidx in 1:10
        z = (xidx - yidx).^2
        x_out[xidx, yidx] = z
    end
    func = instantiate(l)
    func(x_out = x_out)
    @test x_out == ((0:9) .- collect(0:9)').^2

    # Test index manipulation
    x_in = 1:100
    x_out = zeros(Int64, 100)
    l = @looper for idx in 2:99
        x_slice = x_in[idx-1:idx+1]
        x_out[idx] = (minimum(x_slice) + maximum(x_slice))/2.0
    end
    func = instantiate(l)
    func(x_out = x_out, x_in = x_in)
    @test x_out == [0, 2:99..., 0]
end

@testset "Loop Unrolling" begin
    x_out = Ref(0)
    l = @looper for x in 1:10
        x_out[] += x
    end

    # Unroll `l` by a factor of 3 along `x`
    l_unrolled = unroll(l, :x, 3)
    func = instantiate(l_unrolled)

    # Test that this lowers to two loops, one with three statements in
    # the body, the other with one statement in the body.
    @test length(l_unrolled) == 2
    @test length(l_unrolled[1].body) == 3
    @test length(l_unrolled[2].body) == 1

    # Test that actually executing this loop calculates the same thing as
    # an unrolled variant.
    x_out = Ref(0)
    instantiate(l)(x_out = x_out)
    @test x_out[] == sum(1:10)

    x_out = Ref(0)
    instantiate(l_unrolled)(x_out = x_out)
    @test x_out[] == sum(1:10)

    # Next, unroll `l` by a factor of 5 along `x`, eliding any tail
    l_unrolled5 = unroll(l, :x, 5; no_tails=true)
    
    # Test that this lowers to one loop with five statements
    @test length(l_unrolled5) == 1
    @test length(l_unrolled5[1].body) == 5

    # Test that it works
    x_out = Ref(0)
    instantiate(l_unrolled5)(x_out = x_out)
    @test x_out[] == sum(1:10)

    # Next, unroll by a factor of 6 and test that it calculates the wrong result:
    l_unrolled6 = unroll(l, :x, 6; no_tails=true)
    x_out = Ref(0)
    instantiate(l_unrolled6)(x_out = x_out)
    @test x_out[] != sum(1:10)
    @test x_out[] == sum(1:6)
end

@testset "Loop Transposition" begin
    @testset "Simple Transposition" begin
        l = @looper for x in 1:10
            for y in 1:10
                touchstone[x, y] = touch_count
                touch_count += 1
            end
        end

        # Run it normally 
        touchstone = zeros(Int64, 10, 10)
        instantiate(l)(touchstone = touchstone, touch_count = 0)

        # Next, run it transpose
        touchstone_t = zeros(Int64, 10, 10)
        lp = transpose(l, :x, :y)
        instantiate(lp)(touchstone = touchstone_t, touch_count = 0)

        # Make sure the permutation actually happened
        @test touchstone == touchstone_t'
    end

    @testset "Innocent Bystander" begin
        # Make sure that transposing x and z within a loop nest of [z, y, x] leaves y alone
        l = @looper for x in 1:4, y in 1:4, z in 1:4
            touchstone[z, y, x] = touch_count
            touch_count += 1
        end

        # Run it permuted
        touchstone = zeros(Int64, 4, 4, 4)
        lp = transpose(l, :x, :z)
        instantiate(lp)(touchstone = touchstone, touch_count = 0)

        # Test that the permutation worked
        @test touchstone == permutedims(reshape(0:63, (4, 4, 4)), (3,2,1))
    end
end

@testset "Loop Tiling" begin
    l = @looper for x in 1:size(touchstone, 1)
        for y in 1:size(touchstone, 2)
            touchstone[x, y] = touch_count
            touch_count += 1
        end
    end

    # Tile this in x/y in blocks of 2
    func = instantiate(tile(l, :x, :y, 2, 2))
    touchstone = zeros(Int64, 5, 5)
    func(touchstone=touchstone, touch_count = 0)
    @test touchstone == [
        0   1   4   5   8;
        2   3   6   7   9;
       10  11  14  15  18;
       12  13  16  17  19;
       20  21  22  23  24;
    ]

    # Tile this in y/x in blocks of 2
    func = instantiate(tile(l, :y, :x, 2, 2))
    touchstone_t = zeros(Int64, 5, 5)
    func(touchstone=touchstone_t, touch_count = 0)
    @test touchstone == touchstone_t'


    # Tile in 2x3 blocks on a non-square matrix
    func = instantiate(tile(l, :x, :y, 2, 3))
    touchstone = zeros(Int64, 5, 7)
    func(touchstone=touchstone, touch_count = 0)
    @test touchstone == [
        0   1   2   6   7   8  12;
        3   4   5   9  10  11  13;
       14  15  16  20  21  22  26;
       17  18  19  23  24  25  27;
       28  29  30  31  32  33  34;
    ]
end

@testset "Composition" begin
    l = @looper for x in 1:size(touchstone, 1)
        for y in 1:size(touchstone, 2)
            touchstone[x, y] = touch_count
            touch_count += 1
        end
    end

    lc = unroll(tile(l, :x, :y, 4, 4), :y_inner, 4)

    # Three loops; big daddy, final edge, and final corner
    @test length(lc) == 3
    @test length(lc[1].body) == 2
    # Test we are fully unrolled
    @test length(lc[1].body[1].body[1].body[1].body) == 4

    func = instantiate(lc)
    touchstone = zeros(Int64, 5, 5)
    func(touchstone=touchstone, touch_count = 0)

    @test touchstone == [
        0   1   2   3  16;
        4   5   6   7  17;
        8   9  10  11  18;
       12  13  14  15  19;
       20  21  22  23  24;
    ]
end