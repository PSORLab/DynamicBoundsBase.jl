"""
$(TYPEDEF)
"""
abstract type AbstractDERelaxProblem  end

"""
$(TYPEDEF)
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
"""
struct ConstantStateBounds <: AbstractRelaxProblemAttribute
    xL::Vector{Float64}
    xU::Vector{Float64}
    flag::Bool
end
ConstantStateBounds() = ConstantStateBounds(Float64[], Float64[], false)
ConstantStateBounds(xL, xU) = ConstantStateBounds(xL, xU, true)

"""
$(TYPEDEF)

A problem attribute used to check whether variable state bounds are defined.
"""
struct HasVariableStateBounds <: AbstractRelaxProblemAttribute end

"""
$(TYPEDEF)

A problem attribute used to store time-varying state bounds.
"""
struct VariableStateBounds{F1 <: Function, F2 <: Function} <: AbstractRelaxProblemAttribute
    xL::F1
    xU::F2
    flag::Bool
end
VariableStateBounds() = VariableStateBounds(Base.isempty, Base.isempty, false)
VariableStateBounds(xL, xU) = VariableStateBounds(xL, xU, true)


"""
$(TYPEDEF)

A problem attribute used to check whether a user-defined jacobian was specified.
"""
struct HasUserJacobian <: AbstractRelaxProblemAttribute end

"""
$(TYPEDEF)
"""
abstract type AbstractPathConstraint <: AbstractRelaxProblemAttribute end

"""
$(TYPEDEF)

An object used to specify that the invariant `Ax(t,p) <= b` is valid fo all `t`.
"""
struct PolyhedralConstraint <: AbstractPathConstraint
    A::Array{Float64,2}
    b::Vector{Float64}
    isset::Bool
end
function PolyhedralConstraint(A::Array{Float64,2}, b::Vector{Float64})
    return PolyhedralConstraint(A, b, true)
end
PolyhedralConstraint() = PolyhedralConstraint(zeros(Float64,1,1),zeros(Float64,1),false)
