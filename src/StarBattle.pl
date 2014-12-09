% 0 - space / 1 - star / 2_N+1 - divisions
% board size between 2 and 20(just for output purposes)
% square board
% if (N%2==0) -> (N/2)^2 stars
% else -> ((N/2)+0.5)^2 stars

:- use_module(library(clpfd)).
:-use_module(library(random)).
:-use_module(library(lists)).
:-consult(boards).

starBattle:-init.

init:-
        chooseBoard(B),
        printBoard(B),
        write('How many stars?'), %mudar output
        read(Stars),
        rules(B,Stars,Cb),
        printBoard(Cb).
        
chooseBoard(B):-
        board1(B).

checkKeys([],Y,_Stars,Ny):- Ny is Y.
checkKeys([K|Tail],Y,Stars,Ny):-
        C is 0,
        checkElem([K|Tail], K, C, Nc),
        Nc =\= Stars, !,
        Ny = 1
        ;
        checkKeys(Tail, Y, Stars,Ny).
        

checkElem([],_E,C,N):-N is C.
checkElem([K|Tail],E,C,N):-
        E =:= K,!,
        Nc is C+1,
        checkElem(Tail, E,Nc,N)
        ;
        checkElem(Tail,E,C,N).
        
               

rules(B,Stars,_Cb):-
        length(B,S),
        domain(lineKey, 1, S),
        domain(colKey, 1, S),
        Y1 is 0, Y2 is 0,
        checkKeys(lineKey,Y1,Stars,Ny1),
        checkKeys(colKey,Y2,Stars,Ny2),
        Ny1 =:=0,
        Ny2 =:=0.
        
          






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
