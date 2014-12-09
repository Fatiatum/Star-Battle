% 0 - space / 1 - star / 2_N+1 - divisions
% board size between 2 and 20(just for output purposes)
% square board
% if (N%2==0) -> (N/2)^2 stars
% else -> ((N/2)+0.5)^2 stars

:- use_module(library(clpfd)).
:-use_module(library(random)).
:-consult(boards).

starBattle:-init.

init:-
        chooseBoard(B),
        printBoard(B).

chooseBoard(B):-
        board1(B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printStar(' O ').
printSpace('   ').

printElement(X,Y):-
        X =:= 0,!,
          printStar(Y) 
        ;
          printSpace(Y)
        .

printBoard(B):-
        length(B,S),
        write('        '),
        F is S+2,
        printBorder(0,F),nl,
        printBoard(B,0,S),
        write('        '),
        printBorder(0,F).

printBoard([X|_], H, S):-
        H>S-2,
        write('        '),
        write('X'),
        printLine(X, 0, 1, S),
        write('X'),nl.
        
printBoard([X|Nb], H, S):-
        Nh is H+1,
        write('        '),
        write('X'),
        printLine(X, 0, 1, S),
        write('X'),nl,
        write('        '),
        write('X'),
        printDiv(0,S),
        write('X'),nl,
        printBoard(Nb,Nh,S).


printBorder(0,S):-
        Nh is 1,
        write('X'),
        printBorder(Nh,S).

printBorder(H,S):-
        H>S-2.

printBorder(H,S):-
        Nh is H+1,
        write('XXXX'),
        printBorder(Nh,S).
      
printLine([], S, _O, S).

printLine([X|Nb], H, _O, S):-
        H<1,
        Nh is H+1,
        printElement(X,Y),
        write(Y),
        printLine(Nb,Nh,X,S).
printLine([X|Nb], H, O, S):-
        Nh is H+1,
        printElement(X,Y),
        (
        O #\= X,!,
        write('X'),
        write(Y),
        printLine(Nb,Nh,X,S)
        ;
        write('|'),
        write(Y),
        printLine(Nb,Nh,X,S)
        ). 


printDiv(S,S).
printDiv(0,S):-
        Nh is 1,
        write('---'),
        printDiv(Nh,S).

printDiv(H,S):-
        Nh is H+1,
        write('|---'),
        printDiv(Nh,S).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%makeBoard(B):-
%        fillBoard(S, Rows, 'A'),
%        fillBoard(S, B, Rows).                 
%
%fillBoard(0, [], _).
%fillBoard(S, [V|Tail], V):-
%        S>0,
%        Ns is S-1,
%        fillBoard(Ns, Tail, V).
%       
%boardSize(S):-
%        write('What size do you want the board game to have? (it must be between 2 and 20)'), nl,
%        read(Size),
%        (
%           integer(Size),
%           Size>2, Size<20,!,
%           S=Size
%        ;
%           write('invalid size!'),
%           boardSize(S)
%        ).
%makeBoard(Nb):-
%        boardSize(S),
%        fillBoard(S, Rows, 0),
%        fillBoard(S, B, Rows),
%        makeDivs(S, B, New),
%        Nb=New.
%makeDivs(S, B, Nb):-
%        E #= (S * S),
%        div(S, B, 0, E, 0, 0, Nb).
%
%div(_,_,6,0,_).
%div(_S, B, 5, E, X, Y, Nb):-
%        makeRandDiv(B, E, 5, X, Y, Nb).
%div(S, B, C, E, X, Y, Nb):-
%        Nc #= (C+1),
%        R = (E - (S-C)),
%        random(1,R+1,F),
%        makeRandDiv(B, F, Nc, X, Y, Nb),
%        Ne = E-R,
%        div(S, B, Nc, Ne, Nb).
%
%makeRandDiv(B, F, Nc, Nb):-
