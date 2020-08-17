% File : jobshop.pl
% This prolog program solves the jobshop problem
% Author : Yiannis Zervakis
% Date : 7/4/2020

jobshop(Schedule) :-
  deadline(Deadline),
  get_machines(Deadline, MachAvs),
  findall(TL, job(_, TL), Tasks),
  timetable(Tasks, MachAvs),
  schedule(MachAvs, Schedule).

get_machines(Deadline, MachAvs) :-
  findall(m(M, N), machine(M, N), L),
  expand(L, Deadline, [], MachAvs).

expand([], _, MachAvs, MachAvs).
expand([m(M, N) | L], Deadline, MachAvs1, MachAvs4) :-
  expand_one(M, N, Deadline, MachAvs2),
  append(MachAvs1, MachAvs2, MachAvs3),
  expand(L, Deadline, MachAvs3, MachAvs4).

expand_one(_, 0, _, []).
expand_one(M, N, Deadline, [m(M, Avs) | MachAvs]) :-
  N > 0,
  N1 is N-1,
  length(Avs, Deadline),
  expand_one(M, N1, Deadline, MachAvs).

% timetable a list of lists of tasks
timetable([], _).
timetable([Ts | TLs], MachAvs) :-
  timetable1(Ts, Ts, MachAvs, 0),
  timetable(TLs, MachAvs).

% by timetabling each list individually
timetable1([], _, _, _).
timetable1([T | Ts], Tasks, MachAvs, Shift) :-
  process_task(Tasks, T, MachAvs, Shift, NewShift),
  timetable1(Ts, Tasks, MachAvs, NewShift).

process_task(Tasks, T, [m(MachineNumber, L) | _], PrevShift, Shift) :-
  task(T, MachineNumber, D),
  extend(T, D, Extended), % real duration
  correct_duration(Extended, Tasks, Corrected, PrevShift),  % find the correct duration
  sublist(Corrected, L),
  shift_number(L, T, Shift1), % process the new shift number of Corrected's position
  Shift is Shift1 + D.

% If the task doesn't use the current machine, skip this machine
process_task(Tasks, T, [_ | RestMachines], PrevShift, Shift) :-
  process_task(Tasks, T, RestMachines, PrevShift, Shift).

extend(_, 0, []).
extend(T, D, [T | Ts]) :-
  D > 0,
  D1 is D - 1,
  extend(T, D1, Ts).

correct_duration(Task, Tasks, Corrected, PrevShift) :-
  sum_of_extra_time_slots(Tasks, Task, SumNext),
  correct_duration1(Task, PrevShift, SumNext, Corrected). 

sum_of_extra_time_slots(Tasks, Task, SumNext) :-
  next_tasks(Tasks, Task, NextTasks),  
  sum_of_next(NextTasks, Task, SumNext).

next_tasks([T | Next], [T | _], Next).
next_tasks([_ | Rest], [Task | L], Next) :-
  next_tasks(Rest, [Task | L], Next).
  
sum_of_next([], _, 0).
sum_of_next([T | Ts], [Task | TaskTail], Sum) :-
  task(T, _, D),
  sum_of_next(Ts, [Task | TaskTail], Sum1),
  Sum is D + Sum1.

correct_duration1(Task, LengthPrev, LengthNext, Corrected) :-
  create_empty_list(LengthPrev, EmptyListPrev), % Calculate the list before the <Task>
  append(EmptyListPrev, Task, Corrected1),  % Append the list before, with the <Task>
  create_empty_list(LengthNext, EmptyListNext), % Calculate the list after the <Task>
  append(Corrected1, EmptyListNext, Corrected).

create_empty_list(Length, EmptyList) :-
  length(EmptyList, Length),
  empty(EmptyList).
 
empty([]).
empty([X | L]) :-
  var(X),
  empty(L).

sublist(S, L) :-
  append(_, L2, L),
  append(S, _, L2).

% Calculates the number that a task <T> shifted
shift_number([X | _], T, 0) :- atom(X), X = T.
shift_number([T | L], Task, Shift) :-
  var(T),
  shift_number(L, Task, Shift1),
  Shift is Shift1 + 1.

shift_number([X | Rest], T, Shift) :-
  atom(X),
  not X = T, 
  shift_number(Rest, T, Shift1),
  Shift is Shift1 + 1.

% Predicate for pretty view
schedule([], []).
schedule([m(MachineNumber, L) | RestMachines], [execs(MachineNumber, Tasks) | Rest]) :-
  schedule1(L, L, Tasks),
  schedule(RestMachines, Rest).

schedule1([], _, []).
schedule1([T | L], MachineList, [t(T, Begin, End) | Rest]) :-
  shift_number(MachineList, T, Begin),
  task(T, _, D, _),
  End is Begin + D,
  skip_same_tasks(L, T, Rest1),
  schedule1(Rest1, MachineList, Rest).

schedule1([T | L], Rest) :- var(T), schedule1(L, Rest).

skip_same_tasks([], _, []).
skip_same_tasks([T | Rest], Task, [T | Rest]) :- not T = Task, !.
skip_same_tasks([_ | Tasks], Task, Rest) :- skip_same_tasks(Tasks, Task, Rest).


%---Extended Jobshop with staff for each task---%
jobshop_with_man_power(Schedule) :-
  deadline(Deadline),
  get_machines(Deadline, MachAvs),
  findall(TL, job(_, TL), Tasks),
  staff(StaffNumber),
  generate_staff(Deadline, StaffNumber, Staff),
  timetable_with_man_power(Tasks, MachAvs, Staff),
  schedule(MachAvs, Schedule).

% Generates a list with the initial number of staff extended by the deadline number
generate_staff(0, _, []).
generate_staff(Deadline, StaffNumber, [StaffNumber | RestStaff]) :-
  length([StaffNumber | RestStaff], Deadline),
  Deadline1 is Deadline-1,
  generate_staff(Deadline1, StaffNumber, RestStaff).

timetable_with_man_power([], _, _ ).
timetable_with_man_power([Ts | TLs], MachAvs, Staff) :-
  timetable_with_man_power1(Ts, Ts, MachAvs, 0, Staff, NewStaff),
  timetable_with_man_power(TLs, MachAvs, NewStaff).

timetable_with_man_power1([], _, _,_, Staff, Staff).
timetable_with_man_power1([T | Ts], Tasks, MachAvs, Shift, Staff, NewStaff) :-
  process_task_with_man_power(Tasks, T, MachAvs, Shift, NewShift, Staff, NewStaff1),
  timetable_with_man_power1(Ts, Tasks, MachAvs, NewShift, NewStaff1, NewStaff).

process_task_with_man_power(Tasks, T, [m(MachineNumber, L) | _], PrevShift, Shift, CurrStaff, Updated) :-
  task(T, MachineNumber, D, StaffNumber),
  extend(T, D, Extended),
  correct_duration(Extended, Tasks, Corrected, PrevShift),
  sublist(Corrected, L),
  shift_number(L, T, Shift1),
  Shift is Shift1 + D,
  check_and_update_staff(Shift1, D, StaffNumber, CurrStaff, Updated).

process_task_with_man_power(Tasks, T, [_ | RestMachines], PrevShift, Shift, CurrStaff, Updated) :-
  process_task_with_man_power(Tasks, T, RestMachines, PrevShift, Shift, CurrStaff, Updated).

% Checks if there is enough staff
check_and_update_staff(ShiftNumber, D, StaffNumber, Staff, Updated) :-
  skip(ShiftNumber, Staff, Skipped, Stuff1),
  check_and_update_staff1(D, StaffNumber, Stuff1, Updated1),
  append(Skipped, Updated1, Updated).

skip(0, L, [], L).
skip(S, [X | List1], [X | RestSkipped], NewList) :-
  S1 is S-1,
  skip(S1, List1, RestSkipped, NewList).

check_and_update_staff1(0, _, L, L).
check_and_update_staff1(D, StaffNumber, [S | RestStaff], [S1 | RestUpdated]) :-
  S1 is S - StaffNumber,
  S1 >= 0,
  D1 is D-1,
  check_and_update_staff1(D1, StaffNumber, RestStaff, RestUpdated).