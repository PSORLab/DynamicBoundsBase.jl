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
    "Stprage for polyhedral constraint, if any"
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
                      pL::Vector{Float64}, pU::Vector{Float64};
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
                        user_state_bnd, variable_state_bnd, nx, np,
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
