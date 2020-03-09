@enum(BoundLoc, LOWER, UPPER, NOMINAL, UNDEFINED)

"""
$(TYPEDEF)

Abstract supertype for any integrator that constructs relaxations of
a differential equation problem.
"""
abstract type AbstractDERelaxIntegator end

"""
$(TYPEDEF)

Abstract supertype for any integrator that constructs relaxations of
an ordinary differential equation problem.
"""
abstract type AbstractODERelaxIntegator <: AbstractDERelaxIntegator end

"""
$(TYPEDEF)

Abstract supertype for attributes that can be used to `set` or
`get` attributes (properties) of variables in the model.
"""
abstract type AbstractIntegatorAttribute end

#This allows to use attributes in broadcast calls without the need to
# embed it in a `Ref`
Base.broadcastable(attribute::AbstractIntegatorAttribute) = Ref(attribute)

"""
$(TYPEDEF)

An integrator attribute for the string identifying the integration scheme.
"""
struct IntegratorName <: AbstractIntegatorAttribute end

"""
$(TYPEDEF)

An integrator attribute for the Gradient.
"""
struct Gradient{T} <: AbstractIntegatorAttribute
    index::Int
    time::Float64
end
Gradient{LOWER}() = Gradient{LOWER}(-1,-Inf)
Gradient{UPPER}() = Gradient{UPPER}(-1,-Inf)
Gradient{NOMINAL}() = Gradient{NOMINAL}(-1,-Inf)
Gradient() = Gradient{UNDEFINED}(-1,-Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the gradient of
the relaxation at time index `N`.
"""
Gradient{T}(x::Val{N}) where {N,T} = Gradient{T}(N, -Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the subgradient of
the relaxation at time = `x`.
"""
Gradient{T}(x::Float64) where T = Gradient{T}(-1, x)

"""
$(TYPEDEF)

An integrator attribute for the Subgradient.
"""
struct Subgradient{T} <: AbstractIntegatorAttribute
    index::Int
    time::Float64
end
Subgradient{LOWER}() = Subgradient{LOWER}(-1,-Inf)
Subgradient{UPPER}() = Subgradient{UPPER}(-1,-Inf)
Subgradient() = Subgradient{UNDEFINED}(-1,-Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the subgradient of
the relaxation at time index `N`.
"""
Subgradient{T}(x::Val{N}) where {N,T} = Subgradient{T}(N, -Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the subgradient of
the relaxation at time = `x`.
"""
Subgradient{T}(x::Float64) where T = Subgradient{T}(-1, x)

"""
$(TYPEDEF)

An integrator attribute for value of local solution bounds.
"""
struct Value <: AbstractIntegatorAttribute
    index::Int
    time::Float64
end
Value() = Value(-1,-Inf)

"""
$(TYPEDEF)

An integrator attribute for state bounds.
"""
struct Bound{T} <: AbstractIntegatorAttribute
    index::Int
    time::Float64
end
Bound{LOWER}() = Bound{LOWER}(-1,-Inf)
Bound{UPPER}() = Bound{UPPER}(-1,-Inf)
Bound() = Bound{UNDEFINED}(-1,-Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the state
bounds at time index `N`.
"""
Bound{T}(x::Val{N}) where {N,T} = Bound{T}(N, -Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the state
bounds at time = `x`.
"""
Bound{T}(x::Float64) where T = Bound{T}(-1, x)

"""
$(TYPEDEF)

An integrator attribute for relaxations.
"""
struct Relaxation{T} <: AbstractIntegatorAttribute
    index::Int
    time::Float64
end
Relaxation{LOWER}() = Relaxation{LOWER}(-1,-Inf)
Relaxation{UPPER}() = Relaxation{UPPER}(-1,-Inf)
Relaxation() = Relaxation{UNDEFINED}(-1,-Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the relaxation
at time index `N`.
"""
Relaxation{T}(x::Val{N}) where {N,T<:BoundLoc} = Relaxation{T}(N, -Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references relaxation at time = `x`.
"""
Relaxation{T}(x::Float64) where {T} = Relaxation{T}(-1, x)

"""
$(TYPEDEF)

An integrator attribute for indicating bounds/relaxations are of numeric
solution.
"""
struct IsNumeric <: AbstractIntegatorAttribute end

"""
$(TYPEDEF)

An integrator attribute for indicating bounds & relaxations are of exact
solution.
"""
struct IsSolutionSet <: AbstractIntegatorAttribute end

@enum(TerminationStatusCode,
      COMPLETED, # GOOD RESULT
      EMPTY,
      RELAXATION_NOT_CALLED,
      NUMERICAL_ERROR,
      INVALID_OPTION,
      OTHER_ERROR
)

"""
$(TYPEDEF)

A integrator attribute used to query the `TerminationStatusCode` of the integrator
on completion.
"""
struct TerminationStatus <: AbstractIntegatorAttribute end

"""
$(TYPEDEF)

A integrator attribute used to access the current parameter value.
"""
struct ParameterValue <: AbstractIntegatorAttribute
    i::Int
end
ParameterValue() = ParameterValue(-1)

"""
$(TYPEDEF)

A integrator attribute used to access the current parameter value.
"""
struct ParameterBound{T} <: AbstractIntegatorAttribute
    i::Int
end
ParameterBound{LOWER}() = ParameterBound{LOWER}(-1)
ParameterBound{UPPER}() = ParameterBound{UPPER}(-1)
ParameterBound() = ParameterBound{UNDEFINED}(-1)

"""
$(TYPEDEF)

A integrator attribute used to access independent variable support set.
"""
struct SupportSet{T<:AbstractFloat} <: AbstractIntegatorAttribute
    s::Vector{T}
end

"""
$(TYPEDSIGNATURES)

Computes the relaxation at the current parameter value with the current
parameter and state bounds.
"""
function relax! end

"""
$(TYPEDSIGNATURES)

Provides a real-value integration at the current value set for each parameter.
"""
function integrate! end

"""
$(TYPEDSIGNATURES)

"""
function make(prob::T, integrator::AbstractDERelaxIntegator) where {T<:AbstractDERelaxProblem}
    error("make(prob<:ODERelaxProb, integrator) not defined for type of integrator")
end
