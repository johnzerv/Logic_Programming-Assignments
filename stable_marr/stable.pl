:- lib(fd).

stable(Marriages) :-
  def_vars(MenVars, WomenVars),
  men(Mens),
  women(Womens),
  state_constr(Mens, MenVars, Womens, WomenVars),
  labeling(MenVars),
  make_output(Mens, MenVars, Marriages).

def_vars(MenVars, WomenVars) :-
  men(Mens),
  women(Womens),
  length(Mens, L),
  length(MenVars, L),
  def_men_vars(MenVars, Womens),
  length(Womens, L),
  length(WomenVars, L),
  def_women_vars(WomenVars, Mens).

def_men_vars([], _).
def_men_vars([MenVar | RestMenVars], Womens) :-
  MenVar :: Womens,
  def_men_vars(RestMenVars, Womens).

def_women_vars([], _).
def_women_vars([WomenVar | RestWomenVars], Mens) :-
  WomenVar :: Mens,
  def_women_vars(RestWomenVars, Mens).

state_constr(Mens, MenVars, Womens, WomenVars) :-
  state_men_constr(Mens, MenVars, Womens, WomenVars),
  state_women_constr(Womens, WomenVars, Mens, MenVars),
  constr(Mens, MenVars, Womens, WomenVars).

state_men_constr([], [], _, _).
state_men_constr([Men | RestMens], [MenVar | RestMVars], Womens, WomenVars) :-
  state_men_constr1(Womens, WomenVars, Men, MenVar),
  state_men_constr(RestMens, RestMVars, Womens, WomenVars).

state_men_constr1([], [], _, _).
state_men_constr1([Women | RestWomens], [WomenVar | RestWVars], Men, MenVar) :-
  prefers(Men, MenPref),
  prefers(Women, WomenPref),
  constr_men(Women, WomenVar, Men, MenVar, MenPref, WomenPref),
  state_men_constr1(RestWomens, RestWVars, Men, MenVar).

% Main constraint for mens
constr_men(CurrentWomen, WomenVar, Men, MenVar, MenPref, WomenPref) :-
  element(I1, MenPref, MenVar),
  element(I2, MenPref, CurrentWomen),
  element(I3, WomenPref, WomenVar),
  element(I4, WomenPref, Men),
  I1 #> I2 #=> I3 #< I4.

state_women_constr([] , [], _, _).
state_women_constr([Women | RestWomens], [WomenVar | RestWVars], Mens, MenVars) :-
  state_women_constr1(Mens, MenVars, Women, WomenVar),
  state_women_constr(RestWomens, RestWVars, Mens, MenVars).

state_women_constr1([], [], _, _).
state_women_constr1([Men | RestMens], [MenVar | RestMVars], Women, WomenVar) :-
  prefers(Women, WomenPref),
  prefers(Men, MenPref),
  constr_women(Men, MenVar, Women, WomenVar, WomenPref, MenPref),
  state_women_constr1(RestMens, RestMVars, Women, WomenVar).

% Main constraint for womens
constr_women(CurrentMen, MenVar, Women, WomenVar, WomenPref, MenPref) :-
  element(I1, WomenPref, WomenVar),
  element(I2, WomenPref, CurrentMen),
  element(I3, MenPref, MenVar),
  element(I4, MenPref, Women),
  I1 #> I2 #=> I3 #< I4.

% Every marriage must be stable from both sides
constr([], [], _, _).
constr([CurrentMen | RestMens], [CurrentMVar | RestMVars], Womens, WomenVars) :-
  constr1(Womens, WomenVars, CurrentMen, CurrentMVar),
  constr(RestMens, RestMVars, Womens, WomenVars).

constr1([], [], _, _).
constr1([CurrentWomen | RestWomens], [CurrentWVar | RestWVars], CurrentMen, CurrentMVar) :-
  CurrentMVar #= CurrentWomen #<=> CurrentWVar #= CurrentMen,
  constr1(RestWomens, RestWVars, CurrentMen, CurrentMVar).

make_output([], [], []).  
make_output([Men | RestMens], [Women | RestWomens], [Men-Women | RestMarriages]) :-
  make_output(RestMens, RestWomens, RestMarriages).