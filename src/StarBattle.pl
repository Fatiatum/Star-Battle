% 0 - space / 1 - star / 2_N+1 - divisions
% board size between 2 and 20(just for output purposes)
% square board
% if (N%2==0) -> (N/2)^2 stars
% else -> ((N/2)+0.5)^2 stars

:- use_module(library(clpfd)).
:-use_module(library(random)).

%starBattle:-
       
boardSize(S):-
        write('What size do you want the board game to have? (it must be between 2 and 20)'), nl,
        read(Size),
        (
           integer(Size),
           Size>2, Size<20,!,
           S=Size
        ;
           write('invalid size!'),
           boardSize(S)
        ).
makeBoard(Nb):-
        boardSize(S),
        fillBoard(S, Rows, 0),
        fillBoard(S, B, Rows),
        makeDivs(S, B, New),
        Nb=New.

makeDivs(S, B, Nb):-
        X #= (S * S),
        div(S, B, 0, X, Nb).

div(_,_,6,0,_).
div(_S, B, 5, X, Nb):-
        makeRandDiv(B, X, 5, Nb).
div(S, B, C, X, Nb):-
        Nc #= (C+1),
        R = (X - (S-C)),
        random(1,R+1,F),
        makeRandDiv(B, F, Nc, Nb),
        Nx = X-R,
        div(S, B, Nc, Nx, Nb).

makeRandDiv(B, F, Nc, Nb):-
        
                       

fillBoard(0, [], _).
fillBoard(S, [V|Tail], V):-
        S>0,
        Ns is S-1,
        fillBoard(Ns, Tail, V).
        


