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

using DocStringExtensions, ElasticArrays
using ForwardDiff: Chunk, Dual, Partials, construct_seeds, single_seed
using SciMLBase: remake, AbstractODEProblem, ODEProblem, AbstractContinuousCallback, 
                 solve, ODEFunction, wrapfun_iip
using DiffEqSensitivity: extract_local_sensitivities, ODEForwardSensitivityProblem

import Base.get

export HasStateBounds, HasConstantStateBounds, HasVariableStateBounds,
       HasUserJacobian, ConstantStateBounds, VariableStateBounds,
       PolyhedralConstraint, AbstractDERelaxProblem, AbstractRelaxProblemAttribute,
       AbstractDERelaxIntegrator, AbstractIntegratorAttribute, AbstractDynamicIndex
include(joinpath(@__DIR__, "problem.jl"))

export Nominal, Lower, Upper, Undefined, IntegratorName, Value, Gradient,
       ConstantParameterValue, Subgradient, Bound, Relaxation,IsNumeric,
       IsSolutionSet, TerminationStatus, ParameterValue, ParameterBound,
       SupportSet, TerminationStatusCode, relax!,
       integrate!, make, AbstractDERelaxIntegator, AbstractODERelaxIntegrator,
       AbstractBoundLoc, TimeIndex, COMPLETED, EMPTY, RELAXATION_NOT_CALLED,
       NUMERICAL_ERROR, INVALID_OPTION, OTHER_ERROR, LIMIT_EXCEEDED,
       ParameterNumber, StateNumber, SupportNumber, LocalSensitivityOn,
       LocalIntegrator, AttachedProblem

include(joinpath(@__DIR__, "integrator.jl"))

export set!, setall!, get, getall, getall!, make, supports
include(joinpath(@__DIR__, "attribute_handlers.jl"))

export IntegratorStates
include(joinpath(@__DIR__, "utilities.jl"))

export ODERelaxProb, AbstractODERelaxProblem, ODELocalIntegrator
include(joinpath(@__DIR__, "problems", "ODERelaxProb.jl"))

end
