"""
$(TYPEDEF)
"""
abstract type AbstractODERelaxProblem <: AbstractDERelaxProblem end
const AODERP = AbstractODERelaxProblem

"""
$(TYPEDEF)

A structure used to hold a parametric ODEs problem.

$(TYPEDFIELDS)
"""
mutable struct ODERelaxProb{F,JX,JP,xType,K} <: AODERP
    f::F
    Jx!::JX
    Jp!::JP
    x0::xType
    xL::Vector{Float64}
    xU::Vector{Float64}
    tspan::Tuple{Float64, Float64}
    tsupports::Vector{Float64}
    p::Vector{Float64}
    pL::Vector{Float64}
    pU::Vector{Float64}
    user_Jx::Bool
    user_Jp::Bool
    user_state_bnd::Bool
    variable_state_bnd::Bool
    nx::Int
    np::Int
    polyhedral_constraint::PolyhedralConstraint
    constant_state_bounds::ConstantStateBounds
    kwargs::K
end

"""
$(TYPEDSIGNATURES)

Constructor for basic parametric ODE problem. The rhs function is
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
    np::Int = length(p)

    @assert(((xL !== nothing) & (xU !== nothing)) || ((xL == nothing) & (xU == nothing)),
            "Both state bounds must be set or neither must be set")
    @assert(((pL !== nothing) & (pU !== nothing)), "Box constraints on parameters must be defined.")
    @assert(length(pL) === np === length(pU), "The parameter vector and bounds must have the same length.")


    if haskey(kwargs,:nx)
        nx::Int = kwargs[:nx]
    else
        nx = length(x0(pL))
    end

    if haskey(kwargs,:tsupports)
        tsupports::Vector{Float64} = kwargs[:tsupports]
    else
        tsupports = Float64[]
    end

    user_state_bnd::Bool = false
    variable_state_bnd = (~isempty(xL)) & (~isempty(xU))
    user_state_bnd |= variable_state_bnd

    if (~isempty(xL)) & (~isempty(xU))
        user_state_bnd |= true
        @assert length(xL) == length(xU)
    end

    # compute Jacobians
    x0s = zeros(Real, nx)
    Jx0 = zeros(Real, nx, nx)
    Jp0 = zeros(Real, nx, np)
    user_Jx!::Bool = (Jx! !== nothing)
    user_Jp!::Bool = (Jp! !== nothing)
    if user_Jx! && user_Jp!
        JxWrap = wrapfun_iip(Jx, (Jx0, x0s, p, tspan[1],))
        JpWrap = wrapfun_iip(Jx, (Jp0, x0s, p, tspan[1],))
        fwrap = ODEFunction(f; jac = JxWrap, paramjac = JpWrap)
    elseif user_Jx!
        JxWrap = wrapfun_iip(Jx, (Jx0, x0s, p, tspan[1],))
        fwrap = ODEFunction(f; jac = JxWrap)
    elseif user_Jp!
        JpWrap = wrapfun_iip(Jp, (Jp0, x0s, p, tspan[1],))
        fwrap = ODEFunction(f; paramjac = JpWrap)
    else
        fwrap = ODEFunction(f)
    end

    return ODERelaxProb(fwrap, Jx!, Jp!, x0, xL, xU,
                        tspan, tsupports, p, pL, pU, user_Jx!, user_Jp!,
                        user_state_bnd, variable_state_bnd, nx, np,
                        PolyhedralConstraint(), ConstantStateBounds(), kwargs)
end

supports(::ODERelaxProb, ::HasStateBounds) = true
supports(::ODERelaxProb, ::HasConstantStateBounds) = true
supports(::ODERelaxProb, ::HasVariableStateBounds) = true
supports(::ODERelaxProb, ::HasUserJacobian) = true
supports(::ODERelaxProb, ::ConstantStateBounds) = true

get(x::ODERelaxProb, t::HasStateBounds)::Bool = x.user_state_bnd
get(x::ODERelaxProb, t::HasConstantStateBounds)::Bool = ~(isempty(x.xL) && isempty(x.xU))
function get(x::ODERelaxProb, t::HasVariableStateBounds)::Bool
    return x.variable_state_bnd
end
function get(x::ODERelaxProb, t::HasUserJacobian)::Bool
    return x.user_jacobian
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
