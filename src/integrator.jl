"""
$(TYPEDEF)

Abstract supertype indicating the type of value returned at a specific index.
"""
abstract type AbstractBoundLoc end
struct Lower <: AbstractBoundLoc end
struct Upper <: AbstractBoundLoc end
struct Nominal <: AbstractBoundLoc end
struct Undefined <: AbstractBoundLoc end

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

An abstract supertype for index structures needed to reference parameters or time.
"""
abstract type AbstractDynamicIndex end

"""
$(TYPEDEF)
"""
struct TimeIndex
    t::Float64
end

"""
$(TYPEDEF)

An integrator attribute for the string identifying the integration scheme.
"""
struct IntegratorName <: AbstractIntegatorAttribute end

"""
$(TYPEDEF)

An integrator attribute for the Gradient.
"""
struct Gradient{T <: AbstractBoundLoc} <: AbstractIntegatorAttribute
    index::Int
    time::Float64
end
Gradient{Lower}() = Gradient{Lower}(-1, -Inf)
Gradient{Upper}() = Gradient{Upper}(-1, -Inf)
Gradient{Nominal}() = Gradient{Nominal}(-1, -Inf)
Gradient() = Gradient{Undefined}(-1, -Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the gradient of
the relaxation at time index `i`.
"""
Gradient{T}(i::TimeIndex) where {T <: AbstractBoundLoc} = Gradient{T}(i.t, -Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the subgradient of
the relaxation at time = `x`.
"""
Gradient{T}(x::Float64) where {T <: AbstractBoundLoc} = Gradient{T}(-1, x)

"""
$(TYPEDEF)

An integrator attribute for the Subgradient.
"""
struct Subgradient{T <: AbstractBoundLoc} <: AbstractIntegatorAttribute
    index::Int
    time::Float64
end
Subgradient{Lower}() = Subgradient{Lower}(-1, -Inf)
Subgradient{Upper}() = Subgradient{Upper}(-1, -Inf)
Subgradient() = Subgradient{Undefined}(-1, -Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the subgradient of
the relaxation at time index `i`.
"""
Subgradient{T}(i::TimeIndex) where {T <: AbstractBoundLoc} = Subgradient{T}(i.t, -Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the subgradient of
the relaxation at time = `x`.
"""
Subgradient{T}(x::Float64) where {T <: AbstractBoundLoc} = Subgradient{T}(-1, x)

"""
$(TYPEDEF)

An integrator attribute for value of local solution bounds.
"""
struct Value <: AbstractIntegatorAttribute
    index::Int
    time::Float64
end
Value() = Value(-1, -Inf)

"""
$(TYPEDEF)

An integrator attribute for state bounds.
"""
struct Bound{T <: AbstractBoundLoc} <: AbstractIntegatorAttribute
    index::Int
    time::Float64
end
Bound{Lower}() = Bound{Lower}(-1, -Inf)
Bound{Upper}() = Bound{Upper}(-1, -Inf)
Bound() = Bound{Undefined}(-1, -Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the state
bounds at time index `i`.
"""
Bound{T}(i::TimeIndex) where {T <: AbstractBoundLoc} = Bound{T}(i.t, -Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the state
bounds at time = `x`.
"""
Bound{T}(x::Float64) where {T <: AbstractBoundLoc} = Bound{T}(-1, x)

"""
$(TYPEDEF)

An integrator attribute for relaxations.
"""
struct Relaxation{T <: AbstractBoundLoc} <: AbstractIntegatorAttribute
    index::Int
    time::Float64
end
Relaxation{Lower}() = Relaxation{Lower}(-1, -Inf)
Relaxation{Upper}() = Relaxation{Upper}(-1, -Inf)
Relaxation() = Relaxation{Undefined}(-1, -Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references the relaxation
at time index `i`.
"""
Relaxation{T}(i::TimeIndex) where {T <: AbstractBoundLoc} = Relaxation{T}(i.t, -Inf)

"""
$(TYPEDSIGNATURES)

This constructor builds a structure that references relaxation at time = `x`.
"""
Relaxation{T}(x::Float64) where {T <: AbstractBoundLoc} = Relaxation{T}(-1, x)

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
struct ParameterBound{T <: AbstractBoundLoc} <: AbstractIntegatorAttribute
    i::Int
end
ParameterBound{Lower}() = ParameterBound{Lower}(-1)
ParameterBound{Upper}() = ParameterBound{Upper}(-1)
ParameterBound() = ParameterBound{Undefined}(-1)

"""
$(TYPEDEF)

A integrator attribute used to access independent variable support set.
"""
struct SupportSet{T <: AbstractFloat} <: AbstractIntegatorAttribute
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
