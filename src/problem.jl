"""
$(TYPEDEF)

Abstract supertype for all relaxation problems.
"""
abstract type AbstractDERelaxProblem  end

"""
$(TYPEDEF)

Abstract supertype for all problem attributes.
"""
abstract type AbstractRelaxProblemAttribute end

#This allows to use attributes in broadcast calls without the need to
# embed it in a `Ref`
Base.broadcastable(attribute::AbstractRelaxProblemAttribute) = Ref(attribute)

"""
$(TYPEDEF)

A problem attribute used to check whether state bounds are defined.
"""
struct HasStateBounds <: AbstractRelaxProblemAttribute end

"""
$(TYPEDEF)

A problem attribute used to check whether state bounds are defined as constant.
"""
struct HasConstantStateBounds <: AbstractRelaxProblemAttribute end

"""
$(TYPEDEF)

A problem attribute used to store constant state bounds.

$(TYPEDFIELDS)
"""
struct ConstantStateBounds <: AbstractRelaxProblemAttribute
    "Lower state variable bounds"
    xL::Vector{Float64}
    "Upper state variable bounds"
    xU::Vector{Float64}
    function ConstantStateBounds(xL::Vector{Float64}, xU::Vector{Float64})
        @assert length(xL) == length(xU)
        new(xL, xU)
    end
end
ConstantStateBounds() = ConstantStateBounds(Float64[], Float64[])

"""
$(TYPEDEF)

A problem attribute used to check whether variable state bounds are defined.
"""
struct HasVariableStateBounds <: AbstractRelaxProblemAttribute end

"""
$(TYPEDEF)

A problem attribute used to store time-varying state bounds.

$(TYPEDFIELDS)
"""
struct VariableStateBounds{F1 <: Function, F2 <: Function} <: AbstractRelaxProblemAttribute
    "Lower state variable bounds"
    xL::F1
    "Upper state variable bounds"
    xU::F2
end
VariableStateBounds() = VariableStateBounds(Base.isempty, Base.isempty)


"""
$(TYPEDEF)

A problem attribute used to check whether a user-defined jacobian was specified.
"""
struct HasUserJacobian <: AbstractRelaxProblemAttribute end

"""
$(TYPEDEF)

An abstract supertype for all manners of path constraints.
"""
abstract type AbstractPathConstraint <: AbstractRelaxProblemAttribute end

"""
$(TYPEDEF)

An object used to specify that the invariant `Ax(t,p) <= b` is valid fo all `t`.
"""
struct PolyhedralConstraint <: AbstractPathConstraint
    A::Array{Float64,2}
    b::Vector{Float64}
    function PolyhedralConstraint(A::Array{Float64,2}, b::Vector{Float64})
        @assert size(A,1) == length(b)
        return new(A, b)
    end
end
PolyhedralConstraint() = PolyhedralConstraint(zeros(Float64,1,1), zeros(Float64,1))
