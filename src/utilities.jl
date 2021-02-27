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
# src/utilities.jl
# Defines the utility structure IntegratorStates which stores the current
# condition of an integrator.
#############################################################################

"""
$(TYPEDEF)

A structure that holds the state of the integrator.

$(TYPEDFIELDS)
"""
mutable struct IntegratorStates
    "Has the first point been evaluated"
    first_pnt_eval::Bool
    "Have the box-constraints changed since the last evaluation"
    new_decision_box::Bool
    "Has the decision point changed since the last evaluation"
    new_decision_pnt::Bool
    "The relaxed problem has been updated"
    set_lower_state::Bool
    "The local problem has been updated"
    set_upper_state::Bool
    "How did the integrator terminate"
    termination_status::TerminationStatusCode
end
IntegratorStates() =  IntegratorStates(false, true, true, false, false, RELAXATION_NOT_CALLED)


function seed_duals(x::AbstractArray{V}, ::Chunk{N} = Chunk(x)) where {V,N}
  seeds = construct_seeds(Partials{N,V})
  duals = [Dual{Nothing}(x[i],seeds[i]) for i in eachindex(x)]
end
