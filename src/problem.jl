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
struct ConstantBounds <: AbstractRelaxProblemAttribute
    xL::Vector{Float64}
    xU::Vector{Float64}
    flag::Bool
end
ConstantStateBounds() = ConstantBounds(Float64[], Float64[], false)
ConstantStateBounds(xL, xU) = ConstantBounds(xL, xU, true)

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
abstract type AbstractInvariant <: AbstractRelaxProblemAttribute end

const DENSE_LINEAR_INVARIANT_A_TOL = 1e-4
const DENSE_LINEAR_INVARIANT_WIDTH_TOL = 1e-12

"""
$(TYPEDEF)

An object used to specify that the invariant `Ax(t,p) <= b` is valid fo all `t`.
"""
struct ConstantLinearInvariant <: AbstractInvariant
    A::Array{Float64,2}
    b::Vector{Float64}
    isset::Bool
end
function ConstantLinearInvariant(A::Array{Float64,2}, b::Vector{Float64})
    return LinearInvariant(A, b, true)
end
ConstantLinearInvariant() = ConstantLinearInvariant(zeros(Float64,1,1),zeros(Float64,1),false)

"""
$(TYPEDEF)

An object used to specify that the invariant `<= x(t,p) <= b` is valid fo all `t`.
"""
struct AffineLinearInvariant <: AbstractInvariant
    A::Array{Float64,2}
    b::Vector{Float64}
    isset::Bool
end
function AffineLinearInvariant(A::Array{Float64,2}, b::Vector{Float64})
    return AffineLinearInvariant(A, b, true)
end
AffineLinearInvariant() = AffineLinearInvariant(zeros(Float64,1,1),zeros(Float64,1),false)
