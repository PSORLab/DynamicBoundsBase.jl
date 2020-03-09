module DynamicBoundsBase

using DocStringExtensions

export HasStateBounds, HasConstantStateBounds, HasVariableStateBounds,
       HasUserJacobian, DenseLinearInvariant, ConstantBounds
include("problem.jl")

export NOMINAL, LOWER, UPPER, UNDEFINED, IntegratorName, Value, Gradient,
       Subgradient, Bound, Relaxation,IsNumeric, IsSolutionSet, TerminationStatus,
       ParameterValue, ParameterBound, SupportSet, TerminationStatusCode, relax!,
       integrate!, make
include("integrator.jl")

export set!, setall!, get, getall!, make
include("attribute_handlers.jl")

end
