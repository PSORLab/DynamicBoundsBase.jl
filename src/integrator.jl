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
# src/integrator.jl
# Defines attributes and functions used by integrators.
#############################################################################

"""
$(TYPEDEF)

Abstract supertype indicating the type of value returned at a specific index.
"""
abstract type AbstractBoundLoc end

"""
$(TYPEDEF)

Indicates the lower bound, relaxation, or (sub)gradient should be returned.
"""
struct Lower <: AbstractBoundLoc end

"""
$(TYPEDEF)

Indicates the upper bound, relaxation, or (sub)gradient should be returned.
"""
struct Upper <: AbstractBoundLoc end

"""
$(TYPEDEF)

Indicates the nominal value or (sub)gradient should be returned.
"""
struct Nominal <: AbstractBoundLoc end

"""
$(TYPEDEF)

The variety of the attribute is unspecified.
"""
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

An integrator attribute retrieving the number of parameter variables (`p`) aka `np`.
"""
struct ParameterNumber end

"""
$(TYPEDEF)

An integrator attribute retrieving the number of state variables (`x`) aka `nx`.
"""
struct StateNumber end

"""
$(TYPEDEF)

An integrator attribute retrieving the number of points in the support set of (`x`) aka `nt`.
"""
struct SupportNumber end

"""
$(TYPEDEF)

An integrator attribute for the Gradient.
"""
struct Gradient{T<:AbstractBoundLoc} <: AbstractIntegratorAttribute
    index::Int
    time::Float64
end
Gradient{Lower}() = Gradient{Lower}(-1, -Inf)
Gradient{Upper}() = Gradient{Upper}(-1, -Inf)
Gradient{Nominal}() = Gradient{Nominal}(-1, -Inf)
Gradient() = Gradient{Undefined}(-1, -Inf)

"""
Gradient{T}(i::TimeIndex) where {T <: AbstractBoundLoc}

This constructor builds a structure that references the gradient of
the relaxation at time index `i`.
"""
function Gradient{T}(i::TimeIndex) where {T <: AbstractBoundLoc}
    Gradient{T}(i.t, -Inf)
end

"""
Gradient{T}(x::Float64) where {T <: AbstractBoundLoc}

This constructor builds a structure that references the subgradient of
the relaxation at time = `x`.
"""
function Gradient{T}(x::Float64) where {T <: AbstractBoundLoc}
    Gradient{T}(-1, x)
end

"""
$(TYPEDEF)

An integrator attribute for the Subgradient.
"""
struct Subgradient{T<:AbstractBoundLoc} <: AbstractIntegratorAttribute
    index::Int
    time::Float64
end
Subgradient{Lower}() = Subgradient{Lower}(-1, -Inf)
Subgradient{Upper}() = Subgradient{Upper}(-1, -Inf)
Subgradient() = Subgradient{Undefined}(-1, -Inf)

"""
Subgradient{T}(i::TimeIndex)

This constructor builds a structure that references the subgradient of
the relaxation at time index `i`.
"""
Subgradient{T}(i::TimeIndex) where {T <: AbstractBoundLoc} = Subgradient{T}(i.t, -Inf)

"""
Subgradient{T}(x::Float64)

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
struct Bound{T<:AbstractBoundLoc} <: AbstractIntegratorAttribute
    index::Int
    time::Float64
end
Bound{Lower}() = Bound{Lower}(-1, -Inf)
Bound{Upper}() = Bound{Upper}(-1, -Inf)
Bound() = Bound{Undefined}(-1, -Inf)

"""
Bound{T}(i::TimeIndex)

This constructor builds a structure that references the state
bounds at time index `i`.
"""
Bound{T}(i::TimeIndex) where {T <: AbstractBoundLoc} = Bound{T}(i.t, -Inf)

"""
Bound{T}(x::Float64)

This constructor builds a structure that references the state
bounds at time = `x`.
"""
Bound{T}(x::Float64) where {T <: AbstractBoundLoc} = Bound{T}(-1, x)

"""
$(TYPEDEF)

An integrator attribute for relaxations.
"""
struct Relaxation{T<:AbstractBoundLoc} <: AbstractIntegratorAttribute
    index::Int
    time::Float64
end
Relaxation{Lower}() = Relaxation{Lower}(-1, -Inf)
Relaxation{Upper}() = Relaxation{Upper}(-1, -Inf)
Relaxation() = Relaxation{Undefined}(-1, -Inf)

"""
Relaxation{T}(i::TimeIndex)

This constructor builds a structure that references the relaxation
at time index `i`.
"""
Relaxation{T}(i::TimeIndex) where {T <: AbstractBoundLoc} = Relaxation{T}(i.t, -Inf)

"""
Relaxation{T}(x::Float64)

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
      COMPLETED,
      EMPTY,
      NAN,
      RELAXATION_NOT_CALLED,
      NUMERICAL_ERROR,
      LIMIT_EXCEEDED,
      INVALID_OPTION,
      OTHER_ERROR
)

"""
$(TYPEDEF)

A integrator attribute used to query the `TerminationStatusCode` of the integrator
on completion. Current termination status codes are:
- `COMPLETED`: The algorithm terminate successfully with bounds and relaxations.
- `EMPTY`: The algorithm terminated successfully but the solution set was empty for
         some points in `tspan`.
- `NAN`: The algorithm terminated but some values are not a number (usually indicating
         a domain violation was encoutered when computing relaxations).
- `RELAXATION_NOT_CALLED`: The relaxation has not yet been computed.
- `NUMERICAL_ERROR`: A numerical error was encountered.
- `LIMIT_EXCEEDED`: A preset limit was exceeded (number of steps and so on).
- `INVALID_OPTION`: An invalid option was set.
- `OTHER_ERROR`: Another error was encountered.
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
SupportSet{Float64}() = SupportSet{Float64}(Float64[])
SupportSet() = SupportSet{Float64}()

"""
$(FUNCTIONNAME)

Computes the relaxation at the current parameter value with the current
parameter and state bounds.
"""
function relax! end

"""
$(FUNCTIONNAME)

Provides a real-value integration at the current value set for each parameter.
"""
function integrate! end
