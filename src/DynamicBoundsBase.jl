module DynamicBoundsBase

using DocStringExtensions

export HasStateBounds, HasConstantStateBounds, HasVariableStateBounds,
       HasUserJacobian, DenseLinearInvariant, ConstantStateBounds, VariableStateBounds,
       PolyhedralConstraint, AbstractDERelaxProblem, AbstractRelaxProblemAttribute
include("problem.jl")

export NOMINAL, LOWER, UPPER, UNDEFINED, IntegratorName, Value, Gradient,
       Subgradient, Bound, Relaxation,IsNumeric, IsSolutionSet, TerminationStatus,
       ParameterValue, ParameterBound, SupportSet, TerminationStatusCode, relax!,
       integrate!, make, AbstractDERelaxIntegator, AbstractODERelaxIntegator
include("integrator.jl")

export set!, setall!, get, getall!, make
include("attribute_handlers.jl")

export IntegratorStates
include("utilities.jl")

end
