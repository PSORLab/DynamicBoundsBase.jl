const AnyDEAttribute =  Union{AbstractIntegatorAttribute,
                              AbstractRelaxProblemAttribute}

"""
$(TYPEDSIGNATURES)

Return a `Bool` indicating whether `::AbstractDERelaxIntegator` supports
the `::Union{AbstractDERelaxIntegator, AbstractDERelaxProblem, AbstractDERelax}`.
"""
function supports end

supports(::AbstractDERelaxIntegator, ::AbstractIntegatorAttribute) = false
supports(::AbstractDERelaxProblem, ::AbstractRelaxProblemAttribute) = false

"""
$(TYPEDSIGNATURES)
"""
function get end
get(integrator::AbstractDERelaxIntegator, attr::AbstractIntegatorAttribute, idxs::Vector) = get.(integrator, attr, idxs)
get(problem::AbstractDERelaxProblem, attr::AbstractRelaxProblemAttribute, idxs::Vector) = get.(problem, attr, idxs)
function get(m::AbstractDERelaxIntegator, attr::AbstractIntegatorAttribute, args...)
    throw(ArgumentError("AbstractDERelaxIntegator of type $(typeof(m)) does not support accessing the attribute $attr"))
end
function get(m::AbstractDERelaxProblem, attr::AbstractRelaxProblemAttribute, args...)
    throw(ArgumentError("AbstractDERelaxProblem of type $(typeof(m)) does not support accessing the attribute $attr"))
end

"""
$(FUNCTIONNAME)

An in-place version of `get`. The signature matches that of `get` except
that the the result is placed in the vector `output`.
"""
function get! end
function get!(output, m::AbstractDERelaxIntegator, attr::AbstractIntegatorAttribute, args...)
    throw(ArgumentError("ModelLike of type $(typeof(m)) does not support accessing the attribute $attr"))
end
function get!(output, m::AbstractDERelaxProblem, attr::AbstractRelaxProblemAttribute, args...)
    throw(ArgumentError("ModelLike of type $(typeof(m)) does not support accessing the attribute $attr"))
end


"""
$(FUNCTIONNAME)

An in-place version of `get!` which retreives all data asssocaited with a particular
integrator attribute.
"""
function getall!(output, m::AbstractDERelaxIntegator, attr::AbstractIntegatorAttribute, args...)
end

"""
$(FUNCTIONNAME)

Assigns a `value` to the `attr` attribute of the integrator, problem, or relaxation.
"""
function set end

function set(m::AbstractDERelaxIntegator, attr::AbstractIntegatorAttribute, idxs::Vector, vec_of_val::Vector)
    if length(idxs) != length(vec_of_val)
        throw(DimensionMismatch("Number of indices ($(length(idxs))) does " *
                                "not match the number of values " *
                                "($(length(vec_of_val))) set to `$attr`."))
    end
    return set.(m, attr, idxs, vec_of_val)
end

function set(m::AbstractDERelaxProblem, attr::AbstractRelaxProblemAttribute, idxs::Vector, vec_of_val::Vector)
    if length(idxs) != length(vec_of_val)
        throw(DimensionMismatch("Number of indices ($(length(idxs))) does " *
                                "not match the number of values " *
                                "($(length(vec_of_val))) set to `$attr`."))
    end
    return set.(m, attr, idxs, vec_of_val)
end

function set(model::AbstractDERelaxIntegator, attr::AbstractIntegatorAttribute, args...)
    throw_set_error_fallback(model, attr, args...)
end

function set(model::AbstractDERelaxProblem, attr::AbstractRelaxProblemAttribute, args...)
    throw_set_error_fallback(model, attr, args...)
end

"""
$(FUNCTIONNAME)

An in-place version of `set!` which sets all data asssocaited with a particular
integrator attribute.
"""
function setall!(m::AbstractDERelaxIntegator, attr::AbstractIntegatorAttribute, args...)
end

"""
    UnsupportedError <: Exception
Abstract type for error thrown when an element is not supported by the integrator.
"""
abstract type UnsupportedError <: Exception end

"""
$(TYPEDSIGNATURES)

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
$(TYPEDSIGNATURES)

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

function throw_set_error_fallback(model::AbstractDERelaxIntegator,
                                  attr::AbstractIntegatorAttribute,
                                  value;
                                  error_if_supported = SetRelaxAttributeNotAllowed(attr))
    if supports(model, attr)
        throw(error_if_supported)
    else
        throw(UnsupportedRelaxAttribute(attr))
    end
end
function throw_set_error_fallback(model::AbstractDERelaxProblem,
                                  attr::AbstractRelaxProblemAttribute,
                                  index, value;
                                  error_if_supported = SetRelaxAttributeNotAllowed(attr))
    if supports(model, attr, typeof(index))
        throw(error_if_supported)
    else
        throw(UnsupportedRelaxAttribute(attr))
    end
end
