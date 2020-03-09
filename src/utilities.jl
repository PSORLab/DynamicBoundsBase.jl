mutable struct IntegratorStates
    first_pnt_eval::Bool
    new_decision_box::Bool
    new_decision_pnt::Bool
    set_lower_state::Bool
    set_upper_state::Bool
    termination_status::TerminationStatusCode
end
IntegratorStates() =  IntegratorStates(false, true, true, false, false, RELAXATION_NOT_CALLED)
