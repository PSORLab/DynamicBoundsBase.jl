#!/usr/bin/env julia

using Test, DynamicBoundsBase

const DEqR = DynamicBoundsBase
const TSC = DEqR.TerminationStatusCode

@testset "Integrator Attributes Interface" begin

    struct UndefinedIntegrator <: DEqR.AbstractDERelaxIntegator end
    struct UndefinedProblem <: DEqR.AbstractDERelaxProblem end

    mutable struct TestIntegrator <: DEqR.AbstractODERelaxIntegator
        temp::Float64
    end

    DEqR.supports(::TestIntegrator, ::DEqR.IntegratorName) = true
    DEqR.supports(::TestIntegrator, ::DEqR.Gradient) = true
    DEqR.supports(::TestIntegrator, ::DEqR.Subgradient) = true
    DEqR.supports(::TestIntegrator, ::DEqR.Bound) = true
    DEqR.supports(::TestIntegrator, ::DEqR.Relaxation) = true
    DEqR.supports(::TestIntegrator, ::DEqR.IsNumeric) = true
    DEqR.supports(::TestIntegrator, ::DEqR.IsSolutionSet) = true
    DEqR.supports(::TestIntegrator, ::DEqR.TerminationStatus) = true
    DEqR.supports(::TestIntegrator, ::DEqR.Value) = true

    DEqR.get(t::TestIntegrator, a::DEqR.IntegratorName) = "TestIntegrator"
    DEqR.get(t::TestIntegrator, a::DEqR.Gradient) = t.temp
    DEqR.get(t::TestIntegrator, a::DEqR.Subgradient) = t.temp
    DEqR.get(t::TestIntegrator, a::DEqR.Bound) = t.temp
    DEqR.get(t::TestIntegrator, a::DEqR.Relaxation) = t.temp
    DEqR.get(t::TestIntegrator, a::DEqR.IsNumeric) = true
    DEqR.get(t::TestIntegrator, a::DEqR.IsSolutionSet) = true
    DEqR.get(t::TestIntegrator, a::DEqR.TerminationStatus) = t.temp
    DEqR.get(t::TestIntegrator, a::DEqR.Value) = t.temp

    DEqR.set(t::TestIntegrator, a::DEqR.Gradient, value) = (t.temp = value)
    DEqR.set(t::TestIntegrator, a::DEqR.Subgradient, value) = (t.temp = value)
    DEqR.set(t::TestIntegrator, a::DEqR.Bound, value) = (t.temp = value)
    DEqR.set(t::TestIntegrator, a::DEqR.Relaxation, value) = (t.temp = value)
    DEqR.set(t::TestIntegrator, a::DEqR.TerminationStatus, value) = (t.temp = value)
    DEqR.set(t::TestIntegrator, a::DEqR.Value, value) = (t.temp = value)

    undefined_integrator = UndefinedIntegrator()
    @test !DEqR.supports(undefined_integrator, DEqR.IntegratorName())
    @test !DEqR.supports(undefined_integrator, DEqR.Gradient{DEqR.LOWER}(1.0))
    @test !DEqR.supports(undefined_integrator, DEqR.Subgradient{DEqR.LOWER}(1.0))
    @test !DEqR.supports(undefined_integrator, DEqR.Bound{DEqR.LOWER}(1.0))
    @test !DEqR.supports(undefined_integrator, DEqR.Relaxation{DEqR.LOWER}(1.0))
    @test !DEqR.supports(undefined_integrator, DEqR.IsNumeric())
    @test !DEqR.supports(undefined_integrator, DEqR.IsSolutionSet())
    @test !DEqR.supports(undefined_integrator, DEqR.TerminationStatus())
    @test !DEqR.supports(undefined_integrator, DEqR.Value())

    test_integrator = TestIntegrator(1.0)
    @test @inferred DEqR.supports(test_integrator, DEqR.IntegratorName())
    @test @inferred DEqR.supports(test_integrator, DEqR.Gradient{DEqR.LOWER}(1.0))
    @test @inferred DEqR.supports(test_integrator, DEqR.Subgradient{DEqR.LOWER}(1.0))
    @test @inferred DEqR.supports(test_integrator, DEqR.Bound{DEqR.LOWER}(1.0))
    @test @inferred DEqR.supports(test_integrator, DEqR.Relaxation{DEqR.LOWER}(1.0))
    @test @inferred DEqR.supports(test_integrator, DEqR.IsNumeric())
    @test @inferred DEqR.supports(test_integrator, DEqR.IsSolutionSet())
    @test @inferred DEqR.supports(test_integrator, DEqR.TerminationStatus())
    @test @inferred DEqR.supports(test_integrator, DEqR.Value())

    @test_nowarn @inferred DEqR.set(test_integrator, DEqR.Gradient{DEqR.LOWER}(1.1), 1.2)
    val = @inferred DEqR.get(test_integrator, DEqR.Gradient{DEqR.LOWER}(1.1))
    @test val === 1.2
    @test_nowarn @inferred DEqR.set(test_integrator, DEqR.Subgradient{DEqR.LOWER}(1.1), 1.3)
    val = @inferred DEqR.get(test_integrator, DEqR.Subgradient{DEqR.LOWER}(1.1))
    @test val === 1.3
    @test_nowarn @inferred DEqR.set(test_integrator, DEqR.Bound{DEqR.LOWER}(1.1), 1.4)
    val = @inferred DEqR.get(test_integrator, DEqR.Bound{DEqR.LOWER}(1.1))
    @test val === 1.4
    @test_nowarn @inferred DEqR.set(test_integrator, DEqR.Relaxation{DEqR.LOWER}(1.1), 1.5)
    val = @inferred DEqR.get(test_integrator, DEqR.Relaxation{DEqR.LOWER}(1.1))
    @test val === 1.5
    @test_nowarn DEqR.set(test_integrator, DEqR.Value(), 1.53)
    val = @inferred DEqR.get(test_integrator, DEqR.Value())
    @test val === 1.53

    sval = @inferred DEqR.get(test_integrator, DEqR.IntegratorName())
    @test sval === "TestIntegrator"
    @test @inferred DEqR.get(test_integrator, DEqR.IsNumeric())
    @test @inferred DEqR.get(test_integrator, DEqR.IsSolutionSet())

    @test_nowarn @inferred DEqR.set(test_integrator, DEqR.TerminationStatus(), 1.9)
    val = @inferred DEqR.get(test_integrator, DEqR.TerminationStatus())
    @test val === 1.9

    @test_throws ArgumentError DEqR.get(UndefinedIntegrator(), DEqR.IntegratorName())
end
