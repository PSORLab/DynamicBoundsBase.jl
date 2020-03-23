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
       integrate!, make, AbstractDERelaxIntegator, AbstractODERelaxIntegator,
       AbstractBoundLoc, TimeIndex
include("integrator.jl")

export set!, setall!, get, getall!, make, supports
include("attribute_handlers.jl")

export IntegratorStates
include("utilities.jl")

using DiffEqBase: ODEFunction, wrapfun_iip
export ODERelaxProb
include("problem_types/ODERelaxProb.jl")

end
