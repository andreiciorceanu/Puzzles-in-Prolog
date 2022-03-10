:- ensure_loaded('checker.pl').

test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un literal, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de literali reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un literal)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
%
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.
intrebari(integ(_,_,[],_), []).
intrebari(integ(_,_,[(_, Val) | T],_), L) :-
    Val == x,
    intrebari(integ(_,_,T,_), L).
intrebari(integ(_,_,[(_, S)| T],_),L) :- string(S), intrebari(integ(_,_,T,_), L).
intrebari(integ(_,_,[(Poz, [Val])| T],_), L) :-
    intrebari(integ(_,_,T,_), X),
    append([(Poz, Val)], X, L).
intrebari(integ(_,_,[(Poz, [A, B])| T],_), L) :-
    intrebari(integ(_,_,T,_), X),
    append([(Poz, A), (Poz, B)], X, L).

% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.
id_intrebare(integ(_,_,T,_), Int, Id) :-
    intrebari(integ(_,_,T,_), L),
    member((_,Int,_,Id), L).


% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvânt
% de completat; ambele sunt atomi (literali).
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).
completare(integ(H,W,L1,Vocab),Sol, integ(H,W,L6,Vocab)) :-
    functie(integ(H,W,L1,Vocab),Sol,ListaCelule),
    append(ListaCelule,L1,L5),
    remove_dups(L5, L6).

functie(_,[],[]) :- !.
functie(integ(H,W,L,Vocab),[(Q,R) | T], ListCelule) :-
    intrebari(integ(H,W,L,Vocab), ListIntrebari),
    member(((X,Y),Q,Dir,_), ListIntrebari),
    atom_chars(R,Rasp),
    raspuns(integ(H,W,L,Vocab), X,Y,Dir,Rasp,List),
    functie(integ(H,W,L,Vocab),T,L2),
    append(L2,List,ListCelule).

raspuns(integ(_,_,_,_),_,_,_,[],[]).
   
raspuns(integ(H,_,L,_),X,Y,Dir,[F | T],[((X1,Y),F) | List]) :-
    Dir = j,
    X1 is X + 1,
    raspuns(integ(H,_,L,_),X1,Y,Dir,T,List).

raspuns(integ(_,W,L,_),X,Y,Dir,[F | T],[((X,Y1),F) | List]) :-
    Dir = d,
    Y1 is Y + 1,
    raspuns(integ(_,W,L,_),X,Y1,Dir,T,List).


remove_dups([],[]).
remove_dups([First | Rest], NewRest) :- member(First, Rest), remove_dups(Rest, NewRest).
remove_dups([First | Rest], [First | NewRest]) :-  \+ member(First, Rest), remove_dups(Rest, NewRest).

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% pentru Bonus:
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), ?Intrebare, ?Lungime)
%
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).
lungime_spatiu(integ(H,W,L,_), Intrebare, Lungime) :-
    intrebari(integ(H,W,L,_), ListIntrebari),
    member(((X,Y),Intrebare,Dir,_), ListIntrebari),
    parcurgere(integ(H,W,L,_),X,Y,Dir,L3),
    length_1(Lungime, L3), !.

parcurgere(integ(H,_,L,_),X,Y,Dir,_) :-
    Dir = j,
    X1 is X + 1,
    X1 =< H,
    member(((X1,Y),_), L), !.
parcurgere(integ(_,W,L,_),X,Y,Dir,_) :-
    Dir = d,
    Y1 is Y + 1,
    Y1 =< W,
    member(((X,Y1),_), L), !.
parcurgere(integ(H,W,L,_),X,Y,Dir, [((X1,Y),_) | List]) :-
    Dir = j,
    X1 is X + 1,
    X1 =< H,
    \+ member(((X1,Y),_), L),
    parcurgere(integ(H,W,L,_),X1,Y,Dir,List).

parcurgere(integ(H,W,L,_),X,Y,Dir,[((X,Y1),_) | List]) :-
    Dir = d,
    Y1 is Y + 1,
    Y1 =< W,
    \+ member(((X,Y1),_), L),
    parcurgere(integ(H,W,L,_),X,Y1,Dir,List).

length_1(0,[]).
length_1(L1, [_|T]) :- length_1(L,T), L1 is L + 1.

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% pentru Bonus:
% intersectie(integ(+H, +W, +Lista, +Voc), ?I1, ?Poz1, ?I2, ?Poz2)
%
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).
intersectie(integ(H,W,L,_), Intrebare1, Poz1, Intrebare2, Poz2) :-
    intrebari(integ(H,W,L,_), ListIntrebari),
    member(((X,Y),Intrebare1,Dir1,_), ListIntrebari),
    member(((A,B),Intrebare2,Dir2,_), ListIntrebari),
    iteratie(integ(H,W,_,_),X,Y,Dir1,A,B,Dir2,Pos1,Pos2),
    Poz1 is Pos1,
    Poz2 is Pos2.

iteratie(integ(_,_,_,_),X,Y,Dir1,A,B,Dir2,Pos1,Pos2) :-
    Dir1 = d,
    Dir2 = j,
    Pos1 is B - Y - 1,
    Pos2 is X - A -1,!.

iteratie(integ(_,_,_,_),X,Y,Dir1,A,B,Dir2,Pos1,Pos2) :-
    Dir1 = j,
    Dir2 = d,
    Pos1 is A - X - 1,
    Pos2 is Y - B - 1, !.

iteratie(integ(_,W,_,_),X,Y,Dir1,A,B,Dir2,Pos1,Pos2) :-
    Dir1 = d,
    Dir2 = j,
    Y1 is Y + 1,
    Y1 =< W,
    Y1 \= B,
    iteratie(integ(_,W,_,_),X,Y1,Dir1,A,B,Dir2,Pos1,Pos2).

iteratie(integ(H,_,_,_),X,Y,Dir1,A,B,Dir2,Pos1,Pos2) :-
    Dir1 = j,
    Dir2 = d,
    X1 is X + 1,
    X1 =< H,
    X1 \= A,
    iteratie(integ(H,_,_,_),X1,Y,Dir1,A,B,Dir2,Pos1,Pos2).

% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de atomi, fiecare atom
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])
solutii_posibile(integ(H,W,L,Vocab), List) :-
    intrebari(integ(H,W,L,Vocab), ListIntrebari),
    int(integ(H,W,L,Vocab),ListIntrebari,List).

int(integ(_,_,_,_),[],[]) :- !.
int(integ(H,W,L,Vocab),[(_,Intrebare,_,_) | T], [(Intrebare,Lista) | L1]) :-
    lungime_spatiu(integ(H,W,L,Vocab),Intrebare,Lungime),
    cuvinte(Vocab,Lungime,Lista),
    int(integ(H,W,L,Vocab),T,L1).

cuvinte([],_,[]) :- !.
cuvinte([F | R],Lungime,L) :-
    atom_chars(F, Cuvant),
    length_1(N, Cuvant),
    N =\= Lungime,
    cuvinte(R,Lungime,L).
cuvinte([F | R],Lungime,[Cuvant | L]) :-
    atom_chars(F, Cuvant),
    length_1(N, Cuvant),
    N == Lungime,
    cuvinte(R,Lungime,L).


% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de literali, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca literal) care este
% răspunsul la întrebare.
%
% BONUS: rezolvare nu oferă soluții duplicate - numărul de soluții ale 
% predicatului este chiar numărul de completări posibile ale integramei.
rezolvare(_, _) :- false.
