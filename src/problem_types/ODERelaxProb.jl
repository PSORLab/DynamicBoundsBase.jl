# Copyright (c) 2020: Matthew Wilhelm & Matthew Stuber.
# This work is licensed under the Creative Commons Attribution-NonCommercial-
# ShareAlike 4.0 International License. To view a copy of this license, visit
# http://creativecommons.org/licenses/by-nc-sa/4.0/ or send a letter to Creative
# Commons, PO Box 1866, Mountain View, CA 94042, USA.
#############################################################################
# Dynamic Bounds Base
# The underlying abstraction layer for DynamicBounds.
# See https://github.com/PSORLab/DynamicBoundsBase.jl
#############################################################################
# src/problem_types/ODERelaxProb.jl
# Defines structures to hold parametric ODEs and access functions.
#############################################################################

"""
AbstractODERelaxProblem

Abstract type for problems used to construct relaxations of parametric ODEs.
"""
abstract type AbstractODERelaxProblem <: AbstractDERelaxProblem end
const AODERP = AbstractODERelaxProblem

"""
$(TYPEDEF)

A structure used to hold a parametric ODEs problem.

$(TYPEDFIELDS)
"""
mutable struct ODERelaxProb{F,JX,JP,xType,K} <: AODERP
    "Right-hand side function."
    f::F
    "Jacobian of rhs w.r.t x."
    Jx!::JX
    "Jacobian of rhs w.r.t p."
    Jp!::JP
    "Initial condition function."
    x0::xType
    "Lower constant state bound storage."
    xL::Vector{Float64}
    "Upper constant state bound storage."
    xU::Vector{Float64}
    "Time span of integration."
    tspan::Tuple{Float64, Float64}
    "Support points: Points 'x' values will be queried."
    tsupports::Vector{Float64}
    "Point to relax/bound in over the decision space."
    p::Vector{Float64}
    "Lower bounds of the decision space."
    pL::Vector{Float64}
    "Upper bounds of the decision space."
    pU::Vector{Float64}
    "Problem has a user-defined Jacobian of the rhs w.r.t x"
    user_Jx::Bool
    "Problem has a user-defined Jacobian of the rhs w.r.t p"
    user_Jp::Bool
    "Constant user state bounds have been set"
    user_state_bnd::Bool
    "Variable user state bounds have been set"
    variable_state_bnd::Bool
    "State space dimension"
    nx::Int
    "Decision space dimension"
    np::Int
    "Optional keywork argument: Indicates the rhs function and the initial
    condition function take an additional argument `param`. For example,
    it is now `f!(dx, x, p, param)`."
    params::Vector{Float64}
    "Storage for polyhedral constraint, if any"
    polyhedral_constraint::Union{PolyhedralConstraint, Nothing}
    "Storage for the constant state bounds, if any"
    constant_state_bounds::Union{ConstantStateBounds, Nothing}
    "The support set if used"
    support_set::SupportSet{Float64}
    "Additional keyword arguments"
    kwargs::K
end

"""
ODERelaxProb(f, tspan, x0, pL, pU; kwargs...)

Constructor for basic parametric ODE problem. The `f` is rhs function. The integration time span is `tspan`. The initial
condition (which may be a function of p) is `x0`. The lower decision variable bounds are given by `pL` and the upper
decision variable bounds are given by `pU`. Other inputs are set via keyword arguments.
"""
function ODERelaxProb(f::F, tspan::Tuple{Float64, Float64}, x0::xType,
                      pL::Vector{Float64}, pU::Vector{Float64}; params::Vector{Float64} = Float64[],
                      xL::Vector{Float64} = Float64[], xU::Vector{Float64} = Float64[],
                      Jx! = nothing, Jp! = nothing, kwargs...) where {F,xType,tType,pBnd}

    if haskey(kwargs,:p)
        p::Vector{Float64} = kwargs[:p]
    else
        p = 0.5*(pL + pU)
    end
    support_set = haskey(kwargs, :support_set) ? kwargs[:support_set] : SupportSet()

    np::Int = length(p)

    @assert((xL !== nothing && xU !== nothing) || (xL === nothing && xU === nothing),
            "Both state bounds must be set or neither must be set")
    @assert((pL !== nothing && pU !== nothing), "Box constraints on parameters must be defined.")
    @assert(length(pL) === np === length(pU), "The parameter vector and bounds must have the same length.")


    nx::Int64 = haskey(kwargs, :nx) ? kwargs[:nx] : length(x0(pL))
    tsupports::Vector{Float64} =  haskey(kwargs, :tsupports) ? kwargs[:tsupports] : Float64[]

    polyhedral = haskey(kwargs, :polyhedral_constraint) ?  kwargs[:polyhedral_constraint] : nothing
    constantstate = haskey(kwargs, :constant_state_bounds) ? kwargs[:constant_state_bounds] : nothing

    user_state_bnd::Bool = false
    variable_state_bnd = !isempty(xL) && !isempty(xU)
    user_state_bnd |= variable_state_bnd

    if !isempty(xL) && !isempty(xU)
        user_state_bnd |= true
        @assert length(xL) == length(xU)
    end

    # compute Jacobians
    x0s = zeros(Real, nx)
    Jx0 = zeros(Real, nx, nx)
    Jp0 = zeros(Real, nx, np)
    user_Jx!::Bool = Jx! !== nothing
    user_Jp!::Bool = Jp! !== nothing
    if user_Jx! && user_Jp!
        JxWrap = wrapfun_iip(Jx!, (Jx0, x0s, p, tspan[1],))
        JpWrap = wrapfun_iip(Jx!, (Jp0, x0s, p, tspan[1],))
        fwrap = ODEFunction(f; jac = JxWrap, paramjac = JpWrap)
    elseif user_Jx!
        JxWrap = wrapfun_iip(Jx!, (Jx0, x0s, p, tspan[1],))
        fwrap = ODEFunction(f; jac = JxWrap)
    elseif user_Jp!
        JpWrap = wrapfun_iip(Jp!, (Jp0, x0s, p, tspan[1],))
        fwrap = ODEFunction(f; paramjac = JpWrap)
    else
        fwrap = ODEFunction(f)
    end

    return ODERelaxProb(fwrap, Jx!, Jp!, x0, xL, xU,
                        tspan, tsupports, p, pL, pU, user_Jx!, user_Jp!,
                        user_state_bnd, variable_state_bnd, nx, np, params,
                        polyhedral, constantstate, support_set, kwargs)
end

supports(::ODERelaxProb, ::HasStateBounds) = true
supports(::ODERelaxProb, ::HasConstantStateBounds) = true
supports(::ODERelaxProb, ::HasVariableStateBounds) = true
supports(::ODERelaxProb, ::HasUserJacobian) = true
supports(::ODERelaxProb, ::ConstantStateBounds) = true
supports(::ODERelaxProb, ::PolyhedralConstraint) = true
supports(::ODERelaxProb, ::SupportSet) = true

get(x::ODERelaxProb, t::HasStateBounds)::Bool = x.user_state_bnd
get(x::ODERelaxProb, t::HasConstantStateBounds)::Bool = ~(isempty(x.xL) && isempty(x.xU))
function get(x::ODERelaxProb, t::HasVariableStateBounds)::Bool
    return x.variable_state_bnd
end
function get(x::ODERelaxProb, t::HasUserJacobian)::Bool
    return x.user_Jx || x.user_Jp
end

function get(x::ODERelaxProb, t::ConstantStateBounds)
    return x.constant_state_bounds
end
function set!(x::ODERelaxProb, bnds::ConstantStateBounds)
    x.constant_state_bounds = bnds
    return
end

function get(x::ODERelaxProb, invariant::PolyhedralConstraint)
    return x.polyhedral_constraint
end
function set!(x::ODERelaxProb, invariant::PolyhedralConstraint)
    x.polyhedral_constraint = invariant
    return
end

function get(x::ODERelaxProb, support_set::SupportSet{Float64})
    return x.support_set
end
function set!(x::ODERelaxProb, support_set::SupportSet{Float64})
    x.support_set = support_set
    return
end

function get(x::ODERelaxProb, t::ConstantParameterValue)
    return x.params[t.i]
end
function set!(x::ODERelaxProb, t::ConstantParameterValue, v)
    x.params[t.i] = v
    return
end
function getall(x::ODERelaxProb, t::ConstantParameterValue)
    return x.params
end
function getall!(out, x::ODERelaxProb, t::ConstantParameterValue)
    out .= x.params
    return
end
function setall!(x::ODERelaxProb, t::ConstantParameterValue, v)
    x.params .= v
    return
end

mutable struct ODELocalIntegrator{N}
    problem
    ode_problem
    sensitivity_problem
    integrator
    p::Vector{Float64}
    pduals::Vector{Dual{Nothing,Float64,N}}
    x::ElasticArray{Float64,2}
    dxdp::Vector{ElasticArray{Float64,2}}
    xduals::Vector{Dual{Nothing,Float64,N}}
    user_t::Vector{Float64}
    integrator_t::Vector{Float64}
    local_t_dict_flt::Dict{Float64,Int64}
    local_t_dict_indx::Dict{Int64,Int64}
    abs_tol::Float64
    rel_tol::Float64
    function ODELocalIntegrator{N}(prob::ODERelaxProb, integrator; kwargs...) where N
        d = new()
        d.abs_tol = 1E-9
        d.rel_tol = 1E-8
        d.problem = prob
        d.integrator = integrator
        d.ode_problem = ODEProblem(prob.f, zeros(Float64, prob.nx),
                                   prob.tspan, prob.p)
        d.sensitivity_problem = ODEForwardSensitivityProblem(prob.f,
                                                             zeros(Float64, prob.nx),
                                                             prob.tspan,
                                                             prob.p)
        d.x = zeros(Float64, prob.nx, length(prob.tsupports))
        dxdp = ElasticArray{Float64,2}[]
        for i = 1:prob.np
            push!(dxdp, ElasticArray(zeros(Float64, prob.nx, length(prob.tsupports))))
        end
        d.dxdp = dxdp
        d.p = copy(prob.p)
        d.pduals = seed_duals(prob.p)
        d.xduals = fill(Dual{Nothing}(0.0,
                                      single_seed(Partials{N, Float64}, Val(1))),
                                      (prob.nx,))
        d.user_t = prob.support_set.s
        d.integrator_t = prob.support_set.s
        d.local_t_dict_flt = Dict{Float64,Int64}()
        d.local_t_dict_indx = Dict{Int64,Int64}()
        for (i,s) in enumerate(d.user_t)
            d.local_t_dict_flt[s] = i
            d.local_t_dict_indx[i] = i
        end
        return d
    end
end
function ODELocalIntegrator(prob::ODERelaxProb, integrator)
    ODELocalIntegrator{prob.np}(prob, integrator)
end

function integrate!(d::AbstractODERelaxIntegrator, p::ODERelaxProb)

    local_ode_storage = DBB.get(d, DBB.LocalIntegrator())

    np = DBB.get(d, DBB.ParameterNumber())
    nx = DBB.get(d, DBB.StateNumber)
    DBB.getall!(local_prob_storage.p, d, DBB.ParameterValue())
    local_prob_storage.pduals .= seed_duals(local_prob_storage.p, 1:np)

    initial_condition!(local_prob_storage.x0duals, d, d.local_problem_storage.pduals)

    if !DBB.get(t.integrator, DBB.LocalSensitivityOn())
        if length(local_prob_storage.x0local) != d.nx
            resize!(local_prob_storage.x0local, d.nx)
        end
    else
        if length(local_prob_storage.x0local) != nx*(np + 1)
            resize!(local_prob_storage.x0local, nx*(np + 1))
        end
    end

    for i = 1:nx
        local_prob_storage.x0local[i] = d.local_prob_storage.x0duals[i].value
        if DBB.get(t.integrator, DBB.LocalSensitivityOn())
            for j = 1:np
                local_prob_storage.x0local[(nx + j + (i-1)*np)] = local_prob_storage.x0duals[i].partials[j]
            end
        end
    end
    local_prob_storage.problem = remake(local_problem_storage.problem,
                                        u0 = local_problem_storage.x0local,
                                        p = local_prob_storage.p)

    if ~isempty(local_prob_storage.user_t)
        solution = solve(local_prob_storage.problem,
                         local_prob_storage.integrator,
                         saveat = local_prob_storage.user_t,
                         abstol = local_prob_storage.abs_tol,
                         tstops = local_prob_storage.user_t,
                         adaptive = false,
                         reltol = local_prob_storage.rel_tol)
    else
        solution = solve(local_prob_storage.problem,
                         local_prob_storage.integrator,
                         abstol = local_prob_storage.abs_tol,
                         reltol = local_prob_storage.rel_tol)
    end

    new_length = length(solution.t)
    if DBB.get(d, DBB.LocalSensitivityOn())
        x, dxdp = extract_local_sensitivities(solution)
    else
        x = solution.u
    end

    resize!(local_prob_storage.pode_x, nx, new_length)
    resize!(local_prob_storage.integrator_t, new_length)
    prior_length = length(local_prob_storage.integrator_t)
    if new_length == prior_length
        local_prob_storage.integrator_t .= solution.t
    else
        local_prob_storage.integrator_t = solution.t
    end

    for i = 1:new_length
        local_prob_storage.pode_x[:,i] .= x[i]
    end
    if DBB.get(d, DBB.LocalSensitivityOn())
        for i = 1:np
            resize!(local_prob_storage.pode_dxdp[i], nx, new_length)
            local_prob_storage.pode_dxdp[i] .= dxdp[i]
        end
    end

    empty!(local_prob_storage.local_t_dict_flt)
    empty!(local_prob_storage.local_t_dict_indx)

    for (tindx, t) in enumerate(solution.t)
        local_prob_storage.local_t_dict_flt[t] = tindx
    end

    if !isempty(local_prob_storage.user_t)
        next_support_time = local_prob_storage.user_t[1]
        supports_left = length(local_prob_storage.user_t)
        loc_count = 1
        for (tindx, t) in enumerate(solution.t)
            if t == next_support_time
                local_prob_storage.local_t_dict_indx[loc_count] = tindx
                loc_count += 1
                supports_left -= 1
                if supports_left > 0
                    next_support_time = local_prob_storage.user_t[loc_count]
                end
            end
        end
    end

    return nothing
end
