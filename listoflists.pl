% File : listoflists.pl
% Author : Yiannis Zervakis
% 1st assignment for Logic Programming course
% There are 4 section, seperated with a line like this :
%------------------------------------------------------------%


% This section calculates the cartesian product of
% two sets, each one is represented as a list of lists (sets)

% Auxiliary predicate that distributes an element in every
% list that is in the main list (2nd arg)
distribute1(_, [], []).
distribute1(X, [L | M], [[X| L] | L1]) :- distribute1(X, M, L1).
  
% Auxiliary predicate that distribute each element of a list,
% in every list that is in the main list (2nd arg)
distribute([], _, []).
distribute([X|L], Lists, CP) :-
  distribute1(X, Lists, CP1),
  distribute(L, Lists, CP2),
  append(CP1, CP2, CP).

% Auxiliary cart_prod predicate that creates sets of two elements,
% that the 1st element is X and the second is an element of a list (2nd arg)
aux_cart_prod(_, [], []).
aux_cart_prod(X, [Y | L], [[X, Y] | Xs]) :- aux_cart_prod(X, L, Xs).

% Calulculates the cartesian product of a list of sets
cart_prod([[] | _] , []).
cart_prod([L1, L2], CP) :- L1 = [X1 | Tail],
  aux_cart_prod(X1, L2, CP1),
  cart_prod([Tail, L2], CP2),
  append(CP1, CP2, CP).
  
cart_prod([A | L], CP) :-  cart_prod(L, CP1), distribute(A, CP1, CP). 



%------------------------------------------------------------%



% This section transposes a matrix

% Aux predicate for taking the first column (2nd arg) of a matrix (1st arg)
% and returns the rest matrix (3rd arg) too
first_column([], [], []).
first_column([[X|Row] | M], [X|Xs], [Row|Rest]) :- first_column(M, Xs, Rest).

% Transposes a matrix by making all the rows columns and the converse
matr_transp([[] | _], []).
matr_transp(Matrix, [FirstCol | RestTransp]) :-
  first_column(Matrix, FirstCol, RestMatrix),
  matr_transp(RestMatrix, RestTransp).

%------------------------------------------------------------%



% This section does a multiplication between two matrices

% scalar_product/3 calulates the scalar product of two vectors
scalar_product([], [], 0).
scalar_product([X | L], [Y | L1], Result) :- 
  Result1 is X*Y,
  scalar_product(L,L1, Result2), Result is Result1 + Result2.

% Calulates a row (3rd arg) of the new matrix by taking the scalar product
% between the given row (1st arg) and all rows of the 2nd matrix
calculate_row(_, [[] | _], []).
calculate_row(Row, Matrix2, [X | Rest]) :- 
  first_column(Matrix2, FirstCol, RestMatr),
  scalar_product(Row, FirstCol, X), 
  calculate_row(Row, RestMatr, Rest).

% Multiplies two matrices
matr_mult([], _, []).
matr_mult([FirstRow | Rest], M, [FirstNewRow | L1]) :- 
  calculate_row(FirstRow, M, FirstNewRow),
  matr_mult(Rest, M, L1).



%------------------------------------------------------------%



% This section calculates the determinant of
% a NxN matrix.

%---------Auxiliary Predicates----------%
%that are not calculates any determinant%

oddlength([_ | L]) :- evenlength(L).

evenlength([]).
evenlength([_ | L]) :- oddlength(L).

% Takes the nth element of a list (1st arg)
nth_element([X | L], 1, X, L).
nth_element([X | L], Position, Element, [X | Rest]) :-
  Position > 1,
  TempPos is Position - 1,
  nth_element(L, TempPos, Element, Rest).

% Returns the nth_column of matrix (1st arg) and returns the rest Matrix (Last arg) too
nth_column([], N, [], []) :- N > 0.
nth_column([Row | RestRows], N, [X | L], [NewRow | Rest]) :- 
  N > 0,
  nth_element(Row, N, X, NewRow), 
  nth_column(RestRows, N, L, Rest).

%---------------------------------------%

% Auxiliary predicate that calculates the determinant of a matrix (1st arg) by expanding over
% the first row, from right to left
matr_det2(_, _, 0, 0).
matr_det2([[X1 | L1] | L2], Sign, CurrCol, Result) :-
  nth_column([[X1 | L1] | L2], CurrCol, [X | _], [_ | Rest]),
  matr_det1(Rest, Det1),
  TempResult1 is Sign*Det1*X,
  NewSign is Sign*(-1), NewCurrCol is CurrCol - 1,
  matr_det2([[X1 | L1] | L2], NewSign, NewCurrCol, TempResult2),
  Result is TempResult1 + TempResult2.

% Auxiliary predicate that calculates the determinant of a matrix (1st arg) by calling
% the right version of matr_det2, if the matrix is oddlength
% then the right sign is 1, else is -1
% Base case a matrix 2x2
matr_det1([[X1, X2], [X3, X4]], Det) :- Det is X1*X4 - X2*X3.

matr_det1([[X1 | L1] | L2], Det) :- 
  oddlength([[X1 | L1] | L2]),
  length([[X1 | L1] | L2], X), 
  matr_det2([[X1 | L1] | L2], 1, X, Det).

matr_det1([[X1 | L1] | L2], Det) :-
  evenlength([[X1 | L1] | L2]),
  length([[X1 | L1] | L2], X),
  matr_det2([[X1 | L1] | L2], -1, X, Det).

% Returns the determinant of a matrix (1st arg) with base case
% an 1x1 matrix
matr_det([[X]], X).
matr_det(Matrix, Det) :- matr_det1(Matrix, Det).
