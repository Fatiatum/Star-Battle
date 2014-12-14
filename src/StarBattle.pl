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
        nl,write('How many stars?'),nl, %mudar output
        read(Stars),
        rules(B,Stars,[]).
        %printBoard(Cb).
        
chooseBoard(B):-
        board0(B).

checkList(List,Size,Size,N):-
        exactly(Size,List,N).
checkList(List,Size,H,N):-
        exactly(H,List,N),
        Nh is H+1,
        checkList(List,Size,Nh,N).
        
exactly(_,[],0).
exactly(X,[Y|L],N):-
        X #= Y #<=> B,
        N #= M+B,
        exactly(X,L,M).

%tested
add(X,[],[X]).
add(X,[A|L],[A|L1]):-
        add(X,L,L1).

%tested
divKey(_B,[],_Stars,DivKey,K):-K = DivKey.
divKey([B|Board], LineKey, Stars, DivKey,Div):-
        getDiv(B, LineKey, Tail, Stars,0,[],K),
        append(DivKey,K,Key),
        divKey(Board,Tail,Stars,Key,Div).

%tested
getDiv(_B, Key,Tail,Stars,Stars,DivKey, K):-K = DivKey,Tail = Key.
getDiv(B, [T|LineKey], Tail, Stars, H, DivKey,K):-
        element(T,B,E),
        add(E,DivKey,Key),
        Nh is H+1,
        getDiv(B,LineKey,Tail,Stars,Nh,Key,K).

                    
rules(B,Stars,Key):-
        length(B,S),
        Size is S*Stars,
        length(LineKey,Size),
        length(ColKey,Size),
        length(DivKey,Size),
        domain(LineKey, 1, S),
        domain(ColKey, 1, S),
        domain(DivKey,1,S),
        divKey(B,LineKey,Stars,[],DivKey),
        checkList(LineKey,S,1,Stars),
        checkList(ColKey,S,1,Stars),
        checkList(DivKey, S,1,Stars),
        labeling([],LineKey),
        labeling([],ColKey),
        labeling([],DivKey),
        Key=LineKey,
        write(LineKey),write('---'),write(ColKey),write('---'),write(DivKey).
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%test

test(B,LineKey):-
        length(LineKey, 8),
        domain(LineKey, 1,4),
        checkList(LineKey,4,1,2),
        divKey(B,LineKey,1,[],Key),
        checkList(Key,4,1,2),
        labeling([],LineKey),
        labeling([],Key),
        write(LineKey),write('----'),write(Key).

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
