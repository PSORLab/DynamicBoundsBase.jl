module DynamicBoundsBase

using DocStringExtensions

import Base.get

export HasStateBounds, HasConstantStateBounds, HasVariableStateBounds,
       HasUserJacobian, ConstantStateBounds, VariableStateBounds,
       PolyhedralConstraint, AbstractDERelaxProblem, AbstractRelaxProblemAttribute
include("problem.jl")

export Nominal, Lower, Upper, Undefined, IntegratorName, Value, Gradient,
       Subgradient, Bound, Relaxation,IsNumeric, IsSolutionSet, TerminationStatus,
       ParameterValue, ParameterBound, SupportSet, TerminationStatusCode, relax!,
       integrate!, make, AbstractDERelaxIntegator, AbstractODERelaxIntegrator,
       AbstractBoundLoc, TimeIndex, COMPLETED, EMPTY, RELAXATION_NOT_CALLED,
       NUMERICAL_ERROR, INVALID_OPTION, OTHER_ERROR, LIMIT_EXCEEDED
include("integrator.jl")

export set!, setall!, get, getall!, make, supports
include("attribute_handlers.jl")

export IntegratorStates
include("utilities.jl")

using DiffEqBase: ODEFunction, wrapfun_iip
export ODERelaxProb
include("problem_types/ODERelaxProb.jl")

end
