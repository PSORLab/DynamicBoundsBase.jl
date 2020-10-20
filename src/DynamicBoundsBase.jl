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
# src/DynamicBoundsBase.jl
# Main module code.
#############################################################################

module DynamicBoundsBase

using DocStringExtensions

import Base.get

export HasStateBounds, HasConstantStateBounds, HasVariableStateBounds,
       HasUserJacobian, ConstantStateBounds, VariableStateBounds,
       PolyhedralConstraint, AbstractDERelaxProblem, AbstractRelaxProblemAttribute,
       AbstractDERelaxIntegrator, AbstractIntegratorAttribute, AbstractDynamicIndex
include("problem.jl")

export Nominal, Lower, Upper, Undefined, IntegratorName, Value, Gradient,
       Subgradient, Bound, Relaxation,IsNumeric, IsSolutionSet, TerminationStatus,
       ParameterValue, ParameterBound, SupportSet, TerminationStatusCode, relax!,
       integrate!, make, AbstractDERelaxIntegator, AbstractODERelaxIntegrator,
       AbstractBoundLoc, TimeIndex, COMPLETED, EMPTY, RELAXATION_NOT_CALLED,
       NUMERICAL_ERROR, INVALID_OPTION, OTHER_ERROR, LIMIT_EXCEEDED, ParameterNumber,
       StateNumber, SupportNumber

include("integrator.jl")

export set!, setall!, get, getall!, make, supports
include("attribute_handlers.jl")

export IntegratorStates
include("utilities.jl")

using DiffEqBase: ODEFunction, wrapfun_iip
export ODERelaxProb, AbstractODERelaxProblem
include("problem_types/ODERelaxProb.jl")

end
