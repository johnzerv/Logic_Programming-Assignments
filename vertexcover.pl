% File: vertexcover.pl
% This prolog program solves the vertex cover problem
% in a graph, a classic problem in computer science, using
% constraint logic programming.
% Author : Yiannis Zervakis
% Date : 25/4/2020


:- lib(ic).
:- lib(branch_and_bound).

vertexcover(NNodes, Density, Nodes) :-
    create_graph(NNodes, Density, Graph),
    def_vars(NNodes, Nodes1),
    state_constrs(Nodes1, Graph),
    Cost #= sum(Nodes1),  /*The cost is the number of nodes tha are in vertex*/
    bb_min(search(Nodes1, 0, input_order, indomain, complete, []),
          Cost, _),
    make_output(Nodes1, 1, Nodes).

def_vars(NNodes, Nodes) :-
    length(Nodes, NNodes),
    Nodes #:: 0..1.

state_constrs(_, []).
state_constrs(Nodes, [N1 - N2| Graph]) :-
    n_th(N1, Nodes, Node1),
    n_th(N2, Nodes, Node2),
    Node1 + Node2 #> 0, % At least N1 or N2 must be in vertex cover
    state_constrs(Nodes, Graph).

n_th(1, [Node| _], Node).
n_th(N, [_| Nodes], Node) :-
    N \= 1,
    N1 is N - 1,
    n_th(N1, Nodes, Node).

% Predicate for making the format we want
make_output([], _, []).
make_output([First | Nodes], Number, [Number | RestNodes]) :-
   First = 1,
   Number1 is Number + 1,
   make_output(Nodes, Number1, RestNodes).

make_output([First | Nodes], Number, Output) :-
   First = 0,
   Number1 is Number + 1,
   make_output(Nodes, Number1, Output).