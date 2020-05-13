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
abstract type AbstractDERelaxIntegrator end

"""
$(TYPEDEF)

Abstract supertype for any integrator that constructs relaxations of
an ordinary differential equation problem.
"""
abstract type AbstractODERelaxIntegrator <: AbstractDERelaxIntegrator end

"""
$(TYPEDEF)

Abstract supertype for attributes that can be used to `set` or
`get` attributes (properties) of variables in the model.
"""
abstract type AbstractIntegratorAttribute end

#This allows to use attributes in broadcast calls without the need to
# embed it in a `Ref`
Base.broadcastable(attribute::AbstractIntegratorAttribute) = Ref(attribute)

"""
$(TYPEDEF)

An abstract supertype for index structures needed to reference parameters or time.
"""
abstract type AbstractDynamicIndex end

"""
$(TYPEDEF)

A time index `t` used to access the value at `t.i`-th time point.
"""
struct TimeIndex
    t::Int
end

"""
$(TYPEDEF)

An integrator attribute for the string identifying the integration scheme.
"""
struct IntegratorName <: AbstractIntegratorAttribute end

"""
$(TYPEDEF)

An integrator attribute for the Gradient.
"""
struct Gradient{T <: AbstractBoundLoc} <: AbstractIntegratorAttribute
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
struct Subgradient{T <: AbstractBoundLoc} <: AbstractIntegratorAttribute
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
struct Value <: AbstractIntegratorAttribute
    index::Int
    time::Float64
end
Value() = Value(-1, -Inf)

"""
$(TYPEDEF)

An integrator attribute for state bounds.
"""
struct Bound{T <: AbstractBoundLoc} <: AbstractIntegratorAttribute
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
struct Relaxation{T <: AbstractBoundLoc} <: AbstractIntegratorAttribute
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
struct IsNumeric <: AbstractIntegratorAttribute end

"""
$(TYPEDEF)

An integrator attribute for indicating bounds & relaxations are of exact
solution.
"""
struct IsSolutionSet <: AbstractIntegratorAttribute end

@enum(TerminationStatusCode,
      COMPLETED, # GOOD RESULT
      EMPTY,
      RELAXATION_NOT_CALLED,
      NUMERICAL_ERROR,
      LIMIT_EXCEEDED,
      INVALID_OPTION,
      OTHER_ERROR
)

"""
$(TYPEDEF)

A integrator attribute used to query the `TerminationStatusCode` of the integrator
on completion.
"""
struct TerminationStatus <: AbstractIntegratorAttribute end

"""
$(TYPEDEF)

A integrator attribute used to access the current parameter value.
"""
struct ParameterValue <: AbstractIntegratorAttribute
    i::Int
end
ParameterValue() = ParameterValue(-1)

"""
$(TYPEDEF)

A integrator attribute used to access the current parameter value.
"""
struct ParameterBound{T <: AbstractBoundLoc} <: AbstractIntegratorAttribute
    i::Int
end
ParameterBound{Lower}() = ParameterBound{Lower}(-1)
ParameterBound{Upper}() = ParameterBound{Upper}(-1)
ParameterBound() = ParameterBound{Undefined}(-1)

"""
$(TYPEDEF)

A integrator attribute used to access independent variable support set.
"""
struct SupportSet{T <: AbstractFloat} <: AbstractIntegratorAttribute
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
