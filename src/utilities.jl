"""
$(TYPEDEF)

A structure that holds the state of the integrator

$(TYPEDFIELDS)
"""
mutable struct IntegratorStates
    "Has the first point been evaluated"
    first_pnt_eval::Bool
    "Have the box-constraints changed since the last evaluation"
    new_decision_box::Bool
    "Has the decision point changed since the last evaluation"
    new_decision_pnt::Bool
    set_lower_state::Bool
    set_upper_state::Bool
    "How did the integrator terminate"
    termination_status::TerminationStatusCode
end
IntegratorStates() =  IntegratorStates(false, true, true, false, false, RELAXATION_NOT_CALLED)
