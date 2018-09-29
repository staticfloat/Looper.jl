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
    @test collect(Looper.split_range_outer(1:4, 4)) == [0]
    @test collect(Looper.split_range_outer(1:8, 4)) == [0, 4]
    @test collect(Looper.split_range_outer(1:10, 4)) == [0, 4]
    @test collect(Looper.split_range_inner(1:3, 4)) == [0, 1, 2]
    @test collect(Looper.split_range_inner(1:4, 4)) == [0, 1, 2, 3]
    @test collect(Looper.split_range_inner(1:10, 4)) == [0, 1, 2, 3]

    @test collect(Looper.split_range_remainder(1:4, 4)) == []
    @test collect(Looper.split_range_remainder(1:5, 4)) == [0]
    @test collect(Looper.split_range_remainder(1:10, 4)) == [0, 1]
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

@testset "Loop instantiation" begin
    # Test simple loop storage
    l = @looper for x in 1:10
        push!(x_out, x*x)
    end
    @eval function simple()
        x_out = Int64[]
        $(instantiate(l))
        return x_out
    end
    @test simple() == collect(1:10).^2

    # Test more complex double-loop storage
    l = @looper for xidx in 1:10, yidx in 1:10
        z = (xidx - yidx).^2
        x_out[xidx, yidx] = z
    end
    @eval function double_loop(x_out)
        $(instantiate(l))
        return x_out
    end
    @test double_loop(zeros(Int64, 10, 10)) == ((0:9) .- collect(0:9)').^2

    # Test index manipulation
    l = @looper for idx in 2:99
        x_slice = x_in[idx-1:idx+1]
        x_out[idx] = (minimum(x_slice) + maximum(x_slice))/2.0
    end
    @eval function index_manipulation(x_in)
        x_out = zeros(Int64, length(x_in))
        $(instantiate(l))
        return x_out
    end
    @test index_manipulation(1:100) == [0, 2:99..., 0]
end

@testset "Loop Unrolling" begin
    l = @looper for x in 1:10
        x_out += x
    end

    @eval function simple()
        x_out = 0
        $(instantiate(l))
        return x_out
    end

    # Unroll `l` by a factor of 3 along `x`
    l_unrolled = unroll(l, :x, 3)

    # Test that this lowers to two loops, one with three statements in
    # the body, the other with one statement in the body.
    @test length(l_unrolled) == 2
    @test length(l_unrolled[1].body) == 3
    @test length(l_unrolled[2].body) == 1

    @eval function unrolled()
        x_out = 0
        $(instantiate(l_unrolled))
        return x_out
    end

    # Test that actually executing this loop calculates the same thing as
    # an unrolled variant.
    @test unrolled() == sum(1:10)

    # Next, unroll `l` by a factor of 5 along `x`, eliding any tail
    l_unrolled5 = unroll(l, :x, 5; no_tails=true)
    @eval function unrolled5()
        x_out = 0
        $(instantiate(l_unrolled5))
        return x_out
    end
    
    # Test that this lowers to one loop with five statements
    @test length(l_unrolled5) == 1
    @test length(l_unrolled5[1].body) == 5

    # Test that it works
    @test unrolled5() == sum(1:10)

    # Next, unroll by a factor of 6 and test that it calculates the wrong result:
    l_unrolled6 = unroll(l, :x, 6; no_tails=true)
    @eval function unrolled6()
        x_out = 0
        $(instantiate(l_unrolled6))
        return x_out
    end
    @test unrolled6() != sum(1:10)
    @test unrolled6() == sum(1:6)
end

@testset "Loop Splitting" begin
    l = @looper for x in 1:10
        x_out += x
    end

    # Split `l` by a factor of 4 along `x`
    l_split = split(l, :x, 4)

    # Test that this lowers to three loops; a nested pair and a cleanup
    @test length(l_split) == 2
    @test length(l_split[1].body) == 1
    @test length(l_split[1].body[1].body) == 1
    @test length(l_split[2].body) == 1
    @test length(l_split[2].body[1].body) == 1

    @eval function splitted()
        x_out = 0
        $(instantiate(l_split))
        return x_out
    end
    @test splitted() == sum(1:10)
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
        @eval function vanilla()
            touch_count = 0
            touchstone = zeros(Int64, 10, 10)
            $(instantiate(l))
            return touchstone
        end

        @eval function transposed()
            touch_count = 0
            touchstone = zeros(Int64, 10, 10)
            $(instantiate(transpose(l, :x, :y)))
            return touchstone
        end

        touchstone = vanilla()
        touchstone_t = transposed()

        # Make sure the permutation actually happened
        @test touchstone == touchstone_t'
    end

    @testset "Innocent Bystander" begin
        # Make sure that transposing x and z within a loop nest of [z, y, x] leaves y alone
        l = @looper for x in 1:size(touchstone,3),
                        y in 1:size(touchstone,2),
                        z in 1:size(touchstone,1)
            touchstone[z, y, x] = touch_count
            touch_count += 1
        end

        # Run it permuted
        @eval function innocent_bystander(touchstone)
            touch_count = 0
            $(instantiate(transpose(l, :x, :z)))
            return touchstone
        end

        # Test that the permutation worked
        @test innocent_bystander(zeros(Int64, 4, 4, 4)) == permutedims(reshape(0:63, (4, 4, 4)), (3,2,1))
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
    @eval function tile_xy22(touchstone)
        touch_count = 0
        $(instantiate(tile(l, :x, :y, 2, 2)))
        return touchstone
    end
    @test tile_xy22(zeros(Int64, 5, 5)) == [
        0   1   4   5   8;
        2   3   6   7   9;
       10  11  14  15  18;
       12  13  16  17  19;
       20  21  22  23  24;
    ]

    # Tile this in y/x in blocks of 2.  Note that it auto-
    # matically figures out the outer loop index, so we just
    # transpose it first to make the tiling go the other way.
    @eval function tile_yx22(touchstone)
        touch_count = 0
        $(instantiate(tile(transpose(l, :x, :y), :x, :y, 2, 2)))
        return touchstone
    end
    @test tile_yx22(zeros(Int64, 5, 5)) == tile_xy22(zeros(Int64, 5, 5))'


    # Tile in 2x3 blocks on a non-square matrix
    @eval function tile_xy23(touchstone)
        touch_count = 0
        $(instantiate(tile(l, :x, :y, 2, 3)))
        return touchstone
    end
    @test tile_xy23(zeros(Int64, 5, 7)) == [
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
    # Test we are fully unrolled
    @test length(lc[1][1].body[1].body[1].body[1].body) == 4

    @eval function big_daddy(touchstone)
        touch_count = 0
        $(instantiate(lc))
        return touchstone
    end
    @test big_daddy(zeros(Int64, 5, 5)) == [
        0   1   2   3  16;
        4   5   6   7  17;
        8   9  10  11  18;
       12  13  14  15  19;
       20  21  22  23  24;
    ]
end