using Looper, Test, BenchmarkTools

@testset "sum" begin
    # Vector sum
    N = 10000000
    l = @looper for idx in 1:10000000
        s[] += x[idx]
    end

    mysum = instantiate(unroll(l, :idx, 16); verbose=true)

    # Test that it works
    x = rand(Int64, N)
    my_result = Ref(0)
    mysum(x = x, s = my_result)
    @test sum(x) == my_result[]

    # Compare timing
    @btime sum($x)
    my_result = Ref(0)
    @btime $(mysum)(x = $x, s = $my_result)
end

@testset "gemm" begin
    # Matrix-Matrix multiplication C = A * B
    l = @looper for i in 1:size(C, 2)
        for j in 1:size(C,1)
            for k in 1:size(A,2)
                C[j, i] += A[j, k] * B[k, i]
            end
        end
    end

    function timing_test(func, name, Ns = [1000])
        println(name)
        for N in Ns
            println(" --> $N:")
            A = rand(1:100, N, N)
            B = rand(1:100, N, N)
            C = zeros(Int64, N, N)

            func(A=A, B=B, C=C)

            # Ensure that this computed properly
            C_gt = A * B
            @test C == C_gt

            @btime $(func)(A=$A, B=$B, C=$C)
        end
    end

    A = rand(1:100, 1000, 1000)
    B = rand(1:100, 1000, 1000)
    @btime $A * $B

    timing_test(instantiate(l), "Naive loop")
    
    lu4 = unroll(l, :k, 4)
    timing_test(instantiate(lu4), "Unrolled (4)")

    lu8 = unroll(l, :k, 8)
    timing_test(instantiate(lu8), "Unrolled (8)")

    lt4 = tile(l, :i, :j, 4, 4)
    timing_test(instantiate(lt4), "Tiled (4x4)")

    lt4u4 = unroll(tile(l, :i, :j, 4, 4), :k, 4)
    timing_test(instantiate(lt4u4), "Tiled (4x4), Unrolled (4)")
end
