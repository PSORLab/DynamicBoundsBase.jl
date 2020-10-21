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
# src/attribute_handlers.jl
# Defines API for accessing attributes of the integrators and problems.
#############################################################################

const AnyDEAttribute = Union{AbstractIntegratorAttribute,
                             AbstractRelaxProblemAttribute}

"""
$(FUNCTIONNAME)

Return a `Bool` indicating whether `::AbstractDERelaxIntegrator` supports
the `::Union{AbstractDERelaxIntegrator, AbstractDERelaxProblem}`.
"""
function supports end

supports(::AbstractDERelaxIntegrator, ::AbstractIntegratorAttribute) = false
supports(::AbstractDERelaxProblem, ::AbstractRelaxProblemAttribute) = false

"""
$(FUNCTIONNAME)

Returns the value of attribute set for the `::Union{AbstractDERelaxIntegrator, AbstractDERelaxProblem}`. May be vector
valued.
"""
function get end
function get(integrator::AbstractDERelaxIntegrator, attr::Vector{AbstractIntegratorAttribute}, idxs::Vector)
	[get(integrator, i) for i in idxs]
end
function get(problem::AbstractDERelaxProblem, attr::Vector{AbstractRelaxProblemAttribute}, idxs::Vector)
	[get(problem, i) for i in idxs]
end
function get(m::AbstractDERelaxIntegrator, attr::AbstractIntegratorAttribute)
    throw(ArgumentError("AbstractDERelaxIntegrator of type $(typeof(m)) does not support accessing the attribute $attr via get"))
end
function get(m::AbstractDERelaxProblem, attr::AbstractRelaxProblemAttribute)
    throw(ArgumentError("AbstractDERelaxProblem of type $(typeof(m)) does not support accessing the attribute $attr via get"))
end


"""
$(FUNCTIONNAME)

An version of `get` which retreives all data asssociated with a particular integrator attribute.
"""
function getall end
function getall(m::AbstractDERelaxIntegrator, attr::AbstractIntegratorAttribute, args...)
    throw(ArgumentError("AbstractDERelaxIntegrator of type $(typeof(m)) does not support accessing the attribute $attr via getall"))
end
function getall(m::AbstractDERelaxProblem, attr::AbstractRelaxProblemAttribute, args...)
    throw(ArgumentError("AbstractDERelaxProblem of type $(typeof(m)) does not support accessing the attribute $attr via getall"))
end


"""
$(FUNCTIONNAME)

An in-place version of `get!` which retreives all data asssociated with a particular integrator attribute.
"""
function getall! end
function getall!(output, m::AbstractDERelaxIntegrator, attr::AbstractIntegratorAttribute, args...)
    throw(ArgumentError("AbstractDERelaxIntegrator of type $(typeof(m)) does not support accessing the attribute $attr via getall!"))
end
function getall!(output, m::AbstractDERelaxProblem, attr::AbstractRelaxProblemAttribute, args...)
    throw(ArgumentError("AbstractDERelaxProblem of type $(typeof(m)) does not support accessing the attribute $attr via getall!"))
end


"""
$(FUNCTIONNAME)

Assigns a `value` to the `attr` attribute of the integrator, problem, or relaxation.
"""
function set! end

function set!(m::AbstractDERelaxIntegrator, attr::AbstractIntegratorAttribute, idxs::Vector, vec_of_val::Vector)
    if length(idxs) !== length(vec_of_val)
        throw(DimensionMismatch("Number of indices ($(length(idxs))) does " *
                                "not match the number of values " *
                                "($(length(vec_of_val))) set to `$attr`."))
    end
	for (i, v) in enumerate(vec_of_val)
		set!(m, attr, @inbounds idxs[i], v)
	end
end

function set!(m::AbstractDERelaxProblem, attr::AbstractRelaxProblemAttribute, idxs::Vector, vec_of_val::Vector)
    if length(idxs) !== length(vec_of_val)
        throw(DimensionMismatch("Number of indices ($(length(idxs))) does " *
                                "not match the number of values " *
                                "($(length(vec_of_val))) set to `$attr`."))
    end
	for (i, v) in enumerate(vec_of_val)
		set!(m, attr, @inbounds idxs[i], v)
	end
end

function set!(m::AbstractDERelaxIntegrator, attr::AbstractIntegratorAttribute, args...)
    throw_set_error_fallback(m, attr, args...)
end

function set!(m::AbstractDERelaxProblem, attr::AbstractRelaxProblemAttribute, args...)
    throw_set_error_fallback(m, attr, args...)
end

"""
$(FUNCTIONNAME)

An in-place version of `set!` which sets all data asssociated with a particular
integrator attribute.
"""
function setall!(m::AbstractDERelaxIntegrator, attr::AbstractIntegratorAttribute, args...)
	throw_set_error_fallback(m, attr, args...)
end

"""
UnsupportedError <: Exception

Abstract type for error thrown when an element is not supported by the integrator.
"""
abstract type UnsupportedError <: Exception end

"""
$(TYPEDEF)

An error indicating that the attribute `attr` is unsupported.
"""
struct UnsupportedRelaxAttribute{AttrType<:AnyDEAttribute} <: UnsupportedError
    attr::AttrType
    message::String
end
UnsupportedRelaxAttribute(attr::AnyDEAttribute) = UnsupportedRelaxAttribute(attr, "")
UnsupportedRelaxAttribute(err::UnsupportedRelaxAttribute) = "Attribute $(err.attr)"

"""
NotAllowedError <: Exception

Abstract type for error thrown when an operation is supported but cannot be
applied in the current state of the integrator
.
"""
abstract type NotAllowedError <: Exception end

"""
$(TYPEDEF)

An error indicating that the attribute `attr` is supported but cannot be set
for some reason given in the error string.
"""
struct SetRelaxAttributeNotAllowed{AttrType<:AnyDEAttribute} <: NotAllowedError
    attr::AttrType
	message::String
end
SetRelaxAttributeNotAllowed(attr::AnyDEAttribute) = SetRelaxAttributeNotAllowed(attr, "")
operation_name(err::SetRelaxAttributeNotAllowed) = "Setting attribute $(err.attr)"
message(err::SetRelaxAttributeNotAllowed) = err.message


for (mtype, attr_type) in ((AbstractDERelaxIntegrator, AbstractIntegratorAttribute),
	                       (AbstractDERelaxProblem, AbstractRelaxProblemAttribute))
	@eval function throw_set_error_fallback(storage::$mtype,
	                                  attr::$attr_type,
	                                  value;
	                                  error_if_supported = SetRelaxAttributeNotAllowed(attr))
	    if supports(storage, attr)
	        throw(error_if_supported)
	    else
	        throw(UnsupportedRelaxAttribute(attr))
	    end
	end
	@eval function throw_set_error_fallback(storage::$mtype,
	                                  attr::$attr_type,
	                                  index, value;
	                                  error_if_supported = SetRelaxAttributeNotAllowed(attr))
	    if supports(storage, attr, typeof(index))
	        throw(error_if_supported)
	    else
	        throw(UnsupportedRelaxAttribute(attr))
	    end
	end
end
