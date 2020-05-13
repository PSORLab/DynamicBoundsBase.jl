#!/usr/bin/env julia
#using Revise
using Test, DynamicBoundsBase

const DEqR = DynamicBoundsBase
const TSC = DEqR.TerminationStatusCode

@testset "Integrator Attribute Constructor" begin
    t = TimeIndex(3)
    @test Gradient{Lower}() == Gradient{Lower}(-1, -Inf)
    @test Gradient{Upper}() == Gradient{Upper}(-1, -Inf)
    @test Gradient{Nominal}() == Gradient{Nominal}(-1, -Inf)
    @test Gradient() == Gradient{Undefined}(-1, -Inf)
    @test Gradient{Nominal}(t). index == 3
    @test Gradient{Nominal}(2.2).time == 2.2

    @test Subgradient{Lower}() == Subgradient{Lower}(-1, -Inf)
    @test Subgradient{Upper}() == Subgradient{Upper}(-1, -Inf)
    @test Subgradient() == Subgradient{Undefined}(-1, -Inf)
    @test Subgradient{Lower}(t). index == 3
    @test Subgradient{Lower}(2.2).time == 2.2

    @test Bound{Lower}() == Bound{Lower}(-1, -Inf)
    @test Bound{Upper}() == Bound{Upper}(-1, -Inf)
    @test Bound() == Bound{Undefined}(-1, -Inf)
    @test Bound{Lower}(t). index == 3
    @test Bound{Lower}(2.2).time == 2.2

    @test Relaxation{Lower}() == Relaxation{Lower}(-1, -Inf)
    @test Relaxation{Upper}() == Relaxation{Upper}(-1, -Inf)
    @test Relaxation() == Relaxation{Undefined}(-1, -Inf)
    @test Relaxation{Lower}(t). index == 3
    @test Relaxation{Lower}(2.2).time == 2.2

    @test ParameterValue() == ParameterValue(-1)

    @test ParameterBound{Lower}() == ParameterBound{Lower}(-1)
    @test ParameterBound{Upper}() == ParameterBound{Upper}(-1)
    @test ParameterBound() == ParameterBound{Undefined}(-1)
end

@testset "Integrator Attributes Interface" begin

    struct UndefinedIntegrator <: DEqR.AbstractDERelaxIntegator end
    struct UndefinedProblem <: DEqR.AbstractDERelaxProblem end

    undefined_problem = UndefinedProblem()
    DEqR.supports(undefined_problem, DEqR.ConstantStateBounds()) == false

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
    @test !DEqR.supports(undefined_integrator, DEqR.Gradient{DEqR.Lower}(1.0))
    @test !DEqR.supports(undefined_integrator, DEqR.Subgradient{DEqR.Lower}(1.0))
    @test !DEqR.supports(undefined_integrator, DEqR.Bound{DEqR.Lower}(1.0))
    @test !DEqR.supports(undefined_integrator, DEqR.Relaxation{DEqR.Lower}(1.0))
    @test !DEqR.supports(undefined_integrator, DEqR.IsNumeric())
    @test !DEqR.supports(undefined_integrator, DEqR.IsSolutionSet())
    @test !DEqR.supports(undefined_integrator, DEqR.TerminationStatus())
    @test !DEqR.supports(undefined_integrator, DEqR.Value())

    @test_throws ErrorException make(undefined_problem, undefined_integrator)

    test_integrator = TestIntegrator(1.0)
    @test @inferred DEqR.supports(test_integrator, DEqR.IntegratorName())
    @test @inferred DEqR.supports(test_integrator, DEqR.Gradient{DEqR.Lower}(1.0))
    @test @inferred DEqR.supports(test_integrator, DEqR.Subgradient{DEqR.Lower}(1.0))
    @test @inferred DEqR.supports(test_integrator, DEqR.Bound{DEqR.Lower}(1.0))
    @test @inferred DEqR.supports(test_integrator, DEqR.Relaxation{DEqR.Lower}(1.0))
    @test @inferred DEqR.supports(test_integrator, DEqR.IsNumeric())
    @test @inferred DEqR.supports(test_integrator, DEqR.IsSolutionSet())
    @test @inferred DEqR.supports(test_integrator, DEqR.TerminationStatus())
    @test @inferred DEqR.supports(test_integrator, DEqR.Value())

    @test_nowarn @inferred DEqR.set(test_integrator, DEqR.Gradient{DEqR.Lower}(1.1), 1.2)
    val = @inferred DEqR.get(test_integrator, DEqR.Gradient{DEqR.Lower}(1.1))
    @test val === 1.2
    @test_nowarn @inferred DEqR.set(test_integrator, DEqR.Subgradient{DEqR.Lower}(1.1), 1.3)
    val = @inferred DEqR.get(test_integrator, DEqR.Subgradient{DEqR.Lower}(1.1))
    @test val === 1.3
    @test_nowarn @inferred DEqR.set(test_integrator, DEqR.Bound{DEqR.Lower}(1.1), 1.4)
    val = @inferred DEqR.get(test_integrator, DEqR.Bound{DEqR.Lower}(1.1))
    @test val === 1.4
    @test_nowarn @inferred DEqR.set(test_integrator, DEqR.Relaxation{DEqR.Lower}(1.1), 1.5)
    val = @inferred DEqR.get(test_integrator, DEqR.Relaxation{DEqR.Lower}(1.1))
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

    vsbnds1 = DEqR.VariableStateBounds()
    @test vsbnds1.xL == Base.isempty
    @test vsbnds1.xU == Base.isempty

    vsbnds2 = DEqR.VariableStateBounds(x -> 1.0*x, x-> 2.0*x)
    @test vsbnds2.xL(1.0) == 1.0
    @test vsbnds2.xU(1.1) == 2.2

    cbnds1 = DEqR.ConstantStateBounds()
    @test isempty(cbnds1.xL)
    @test isempty(cbnds1.xU)

    cbnds2 = DEqR.ConstantStateBounds([1.0], [2.2])
    @test cbnds2.xL[1] == 1.0
    @test cbnds2.xU[1] == 2.2

    ref_attr = DEqR.VariableStateBounds()
    ref = Ref(ref_attr)
    @test Base.broadcastable(ref_attr).x == ref.x

    pconstr = DEqR.PolyhedralConstraint([1.0 1.0; 2.0 2.1], [3.2; 3.1])
    @test pconstr.A == [1.0 1.0; 2.0 2.1]
    @test pconstr.b == [3.2; 3.1]

    states = DEqR.IntegratorStates()
    @test !states.first_pnt_eval
    @test states.new_decision_box
    @test states.new_decision_pnt
    @test states.termination_status === RELAXATION_NOT_CALLED
end

@testset "ODE Relax Problem" begin

    x0(p) = [9.0]
    xL = [0.0]
    xU = [10.0]

    function f!(dx, x, p, t)
        dx[1] = p[1] - x[1]^2
        nothing
    end

    function Jx!(dx, x, p, t)
        dx[1] = -2.0*x[1]
        nothing
    end

    function Jp!(dx, x, p, t)
        dx[1] = one(p[1])
        nothing
    end

    tspan = (0.0, 0.1)
    pL = [-1.0]
    pU = [1.0]
    pval = [0.1]
    tsupports = [0.1; 0.2; 0.3]

    prob = DEqR.ODERelaxProb(f!, tspan, x0, pL, pU, xL = xL, xU = xU, Jx! = Jx!,
                             Jp! = Jp!, p = pval, tsupports = tsupports, nx = 1)

    @test DEqR.supports(prob, DEqR.HasStateBounds())
    @test DEqR.supports(prob, DEqR.HasConstantStateBounds())
    @test DEqR.supports(prob, DEqR.HasVariableStateBounds())
    @test DEqR.supports(prob, DEqR.HasUserJacobian())
    @test DEqR.supports(prob, DEqR.ConstantStateBounds())
    @test DEqR.supports(prob, DEqR.PolyhedralConstraint())

    @test DEqR.get(prob, DEqR.HasStateBounds())
    @test DEqR.get(prob, DEqR.HasConstantStateBounds())
    @test DEqR.get(prob, DEqR.HasVariableStateBounds())
    @test DEqR.get(prob, DEqR.HasUserJacobian())

    cbnds = DEqR.ConstantStateBounds([1.0], [2.2])
    pconstr = DEqR.PolyhedralConstraint([1.0 1.0; 2.0 2.1], [3.2; 3.1])

    @test_nowarn DEqR.set!(prob, cbnds)
    @test_nowarn DEqR.set!(prob, pconstr)

    csbnds = DEqR.get(prob, DEqR.ConstantStateBounds())
    pconstrs = DEqR.get(prob, DEqR.PolyhedralConstraint())

    @test csbnds.xL[1] == 1.0
    @test csbnds.xU[1] == 2.2

    @test pconstrs.A[2,2] == 2.1
    @test pconstrs.b[1] == 3.2
end
