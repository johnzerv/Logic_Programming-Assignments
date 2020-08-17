:- lib(ic).
:- lib(branch_and_bound).

jobshop_opt(Jobs, Staff, Schedule, Cost, Delta, Timeout) :-
  get_tasks(Jobs, Tasks, TotalTasks, TotalDuration),
  get_machines(Machines, TotalMachines),
  def_vars(TaskVars, Tasks, TotalTasks, TotalDuration, TotalMachines),
  state_constr(Jobs, Tasks, TaskVars, Machines, TotalDuration, Staff),
  isolate_ending_times(TaskVars, EndingTimes),
  isolate_starting_times(TaskVars, StartingTimes),
  isolate_machine_choices(TaskVars, MachineChoices),
  append(MachineChoices, StartingTimes, Vars), 
  Cost #= max(EndingTimes),
  bb_min(search(Vars, 0, first_fail, indomain, complete, []), Cost, bb_options{timeout:Timeout, delta:Delta}),
  schedule(Machines, TaskVars, Schedule).

% Get tasks to a flattened list
get_tasks([], [], _, _).  
get_tasks([JobID | RestJobs], Tasks, TotalTasks, TotalDuration) :-
  job(JobID, CurrTasks),
  findall(task(TaskID, M, D, S), (member(TaskID, CurrTasks), task(TaskID, M, D, S)), Tasks1),
  get_tasks(RestJobs, Tasks2, _, _),
  append(Tasks1, Tasks2, Tasks),
  length(Tasks, TotalTasks),
  sum_of_durations(Tasks, TotalDuration).

sum_of_durations([], 0).
sum_of_durations([task(_, _, D, _) | RestTasks], TotalDuration) :-
  sum_of_durations(RestTasks, TD1),
  TotalDuration is D + TD1.

% Get tasks to a non-flattened list
get_tasks_for_each_job([], []).
get_tasks_for_each_job([JobID | RestJobs], [Tasks | RestTasks]) :-
  job(JobID, Tasks),
  get_tasks_for_each_job(RestJobs, RestTasks).

get_machines(ExpandedMachines, TotalMachines) :-
  findall(machine(M, Q), machine(M, Q), Machines),
  expand(Machines, 1, ExpandedMachines),
  length(ExpandedMachines, TotalMachines).  

expand([], _, []).
expand([machine(M, Q) | RestMachines], Counter, ExpandedMachines) :-
  expand1(M, Q, Q, Counter, ExpandedMachine),
  Counter1 is Counter + Q,
  expand(RestMachines, Counter1, ExpandedMachine1),
  append(ExpandedMachine, ExpandedMachine1, ExpandedMachines).

expand1(_, _, TC, _, []) :- TC < 1.
expand1(M, Q, TempCounter, Counter, [(M, Counter) | Rest]) :-
  TempCounter >= 1,
  Temp is TempCounter - 1,
  Counter1 is Counter + 1,
  expand1(M, Q, Temp, Counter1, Rest).


%---Define Vars---%
def_vars(TasksVars, Tasks, TotalTasks, TotalDuration, TotalMachines):-
  length(TasksVars, TotalTasks),
  def_vars1(Tasks, TasksVars, TotalDuration, TotalMachines).

def_vars1([], _, _, _).
def_vars1([task(TaskID, _, _, _) | RestTasks], [(TaskID, S, E, M) | RestVars], TotalDuration, TotalMachines):-  
  S #:: 0..TotalDuration,
  E #:: 0..TotalDuration,
  M #:: 1..TotalMachines,
  def_vars1(RestTasks, RestVars, TotalDuration, TotalMachines).

%---Constraints---%
state_constr(Jobs, Tasks, TaskVars, Machines, TotalTimeSlots, AvailableStaff) :-
  state_duration_constr(Tasks, TaskVars),
  get_tasks_for_each_job(Jobs, TasksLists),
  state_consecutively_constr(TasksLists, TaskVars),
  state_machine_correspodence_constr(Tasks, TaskVars, Machines),
  state_overlapping_constr(TaskVars),
  state_staff_constr(Tasks, TaskVars, 0, TotalTimeSlots, AvailableStaff).

state_duration_constr([], []).
state_duration_constr([task(TaskID, _, D, _) | RestTasks], [(TaskID, S, E, _) | RestVars]) :-
  E #= S + D,
  state_duration_constr(RestTasks, RestVars).

state_consecutively_constr([], _).
state_consecutively_constr([Tasks | RestTasks], TaskVars) :-
  state_consecutively_constr1(Tasks, TaskVars, RestTaskVars),
  state_consecutively_constr(RestTasks, RestTaskVars).

state_consecutively_constr1([TaskID], [(TaskID, _, _, _) | RestVars], RestVars).
state_consecutively_constr1([TaskID1, TaskID2 | RestTasks], [(TaskID1, _, E1, _), (TaskID2, S2, E2, _) | RestVars], RestTaskVars) :-
  S2 #>= E1,
  state_consecutively_constr1([TaskID2 | RestTasks], [(TaskID2, _, E2, _) | RestVars], RestTaskVars).

state_machine_correspodence_constr([], [], _).
state_machine_correspodence_constr([task(_, Machine, _, _) | RestTasks], [(_, _, _, M) | RestVars], Machines) :-
  findall(MachineID, member((Machine, MachineID), Machines), CurrMachineIDS),
  M #:: CurrMachineIDS,
  state_machine_correspodence_constr(RestTasks, RestVars, Machines).

state_overlapping_constr([]).
state_overlapping_constr([(_, S, E, M) | RestVars]) :-
  state_overlapping_constr1(S, E, M, RestVars),
  state_overlapping_constr(RestVars).

state_overlapping_constr1(_ ,_, _, []).
state_overlapping_constr1(S1, E1, M1, [(_, S2, E2, M2) | RestVars]) :-
  (M1 #= M2) => ((E2 #=< S1) or (E1 #=< S2)),
  state_overlapping_constr1(S1, E1, M1, RestVars).

state_staff_constr(_, _, TotalTimeSlots, TotalTimeSlots, _).
state_staff_constr(Tasks, TaskVars, CurrSlot, TotalTimeSlots, AvailableStaff) :-
  CurrSlot < TotalTimeSlots,
  state_staff_constr1(Tasks, TaskVars, CurrSlot, 0, TotalStaff),
  TotalStaff #=< AvailableStaff,
  NextSlot is CurrSlot + 1,
  state_staff_constr(Tasks, TaskVars, NextSlot, TotalTimeSlots, AvailableStaff).

state_staff_constr1([], [], _, Acc, Acc).
state_staff_constr1([task(TaskID, _, _, CurrStaff) | RestTasks], [(TaskID, S, E, _) | RestVars], CurrSlot, AccStaff, TotalStaff) :-
  AccStaff1 #= (AccStaff + CurrStaff*(E #> CurrSlot and S #=< CurrSlot)),
  state_staff_constr1(RestTasks, RestVars, CurrSlot, AccStaff1, TotalStaff).

% Auxiliary predicates %
isolate_ending_times([], []).
isolate_ending_times([(_, _, E, _) | RestVars], [E | RestEndingTimes]) :-
  isolate_ending_times(RestVars, RestEndingTimes).

isolate_starting_times([], []).
isolate_starting_times([(_, S, _, _) | RestVars], [S | RestStartingTimes]) :-
  isolate_starting_times(RestVars, RestStartingTimes).

isolate_machine_choices([], []).
isolate_machine_choices([(_, _, _, M) | RestVars], [M | RestMachineChoices]) :-
  isolate_machine_choices(RestVars, RestMachineChoices).

schedule([], _, []).
schedule([(Machine, MachineID) | RestMachines], TaskVars, [execs(Machine, ConvertedTasks) | RestExecs]) :-
  findall((TaskID, S, E, MachineID), member((TaskID, S, E, MachineID), TaskVars), CurrTasks),
  sort(2, <, CurrTasks, SortedTasks), % Without sorting, CPU's time is significantly reduced
  format_convert(SortedTasks, ConvertedTasks),
  schedule(RestMachines, TaskVars, RestExecs).

format_convert([], []).
format_convert([(TaskID, S, E, _) | RestVars], [t(TaskID, S, E) | RestTasks]) :-
  format_convert(RestVars, RestTasks).

