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
        printBoard(B,B),
        nl,write('How many stars?'),nl, %mudar output
        read(Stars),
        rules(B,Stars,Key),
        write(Key),nl,nl,
        changeBoard(B,B,Stars,0,Key,Nb),
        printBoard(Nb,B).

changeBoard(B,_Board,_Stars,_H,[],Board):-Board=B.
changeBoard(B,[H|Tail],Stars,C,Key,Board):-
        Nc is C+1,
        changeRow(H,Stars,0,Key,Rest,NLine),
        replace(NLine, B, Nc, Nb),
        changeBoard(Nb,Tail,Stars,Nc,Rest,Board).

changeRow(Line,Stars,Stars,Key,Rest,NLine):- NLine=Line,Rest=Key.
changeRow(Line,Stars,H,[X|Rest],R,Nline):-
        Nh is H+1,
        P is 0,
        replace(P,Line,X,Row), %change piece in row
        changeRow(Row,Stars,Nh,Rest,R,Nline).

replace(P, [_|List], 1, [P|List]).
replace(P, [Head|List], X, [Head|Rest]):- 
        X1 is X-1,
        replace(P,List,X1,Rest).  
        
chooseBoard(B):-
        board0(B).
        
exactly(_,[],0).
exactly(X,[Y|L],N):-
        X #= Y #<=> B,
        N #= M+B,
        exactly(X,L,M).

add(X,[],[X]).
add(X,[A|L],[A|L1]):-
        add(X,L,L1).

divKey(_B,[],_Stars,DivKey,K):-K = DivKey.
divKey([B|Board], LineKey, Stars, DivKey,Div):-
        getDiv(B, LineKey, Tail, Stars,0,[],K),
        append(DivKey,K,Key),
        divKey(Board,Tail,Stars,Key,Div).

getDiv(_B, Key,Tail,Stars,Stars,DivKey, K):-K = DivKey,Tail = Key.
getDiv(B, [T|LineKey], Tail, Stars, H, DivKey,K):-
        element(T,B,E),
        add(E,DivKey,Key),
        Nh is H+1,
        getDiv(B,LineKey,Tail,Stars,Nh,Key,K).

mult([],_Stars,_H,SubK):-
        multStars(SubK).
mult([_X|R],Stars,Stars,SubK):-
        multStars(SubK),
        mult(R,Stars,0,[]).    
mult([X|R],Stars,H,SubK):-
        Nh is H+1,
        add(X,SubK,Sub),
        mult(R,Stars,Nh,Sub).
        
multStars(SubK):-
        all_different(SubK).

                    
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
        mult(LineKey, Stars, 1, []),
        checkList(ColKey,S,1,Stars),
        checkList(DivKey, S,1,Stars),
        labeling([],LineKey),
        labeling([],ColKey),
        labeling([],DivKey),
        Key=LineKey.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Display
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


printStar(' * ').
printSpace('   ').
printElement(X,Y):-
        X =:= 0,!,
          printStar(Y) 
        ;
          printSpace(Y)
        .

boardDiv([R|_Rest],Line,Key):-
        div(R,Line,[],K),
        Key=K.
                                         
div([],[],Key,FKey):-FKey=Key.
div([R|Rest],[X|Tail],Key,FKey):-
        R==X,!,
        F = 0,
        add(F,Key,K),
        div(Rest,Tail,K,FKey)
        ;
        F = 1,
        add(F,Key,K),
        div(Rest,Tail,K,FKey).
        
printBoard(B,NB):-
        length(B,S),
        write('        '),
        F is S+2,
        printBorder(0,F),nl,
        printBoard(B,NB,0,S),
        write('        '),
        printBorder(0,F).
printBoard([X|_],_B, H, S):-
        H>S-2,
        write('        '),
        write('X'),
        printLine(X, 0, 1, S),
        write('X'),nl.       
printBoard([X|Nb],[X1|Nb1], H, S):-
        Nh is H+1,
        write('        '),
        write('X'),
        printLine(X, 0, 1, S),
        write('X'),nl,
        write('        '),
        write('X'),
        boardDiv(Nb1,X1,D),
        printDiv(0,S,D),
        write('X'),nl,
        printBoard(Nb,Nb1,Nh,S).

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
        O == X,!,
        write('|'),
        write(Y),
        printLine(Nb,Nh,X,S)
        ;
        X == 0,!,
        write('|'),
        write(Y),
        printLine(Nb,Nh,X,S)
        ;
        write('X'),
        write(Y),
        printLine(Nb,Nh,X,S)
        ). 

printDiv(S,S,_D).
printDiv(0,S,[D|Rest]):-
        D == 0,!,
        Nh is 1,
        write('---'),
        printDiv(Nh,S,Rest).
printDiv(0,S,[D|Rest]):-
        D == 1,!,
        Nh is 1,
        write('XXX'),
        printDiv(Nh,S,Rest).
printDiv(H,S,[D|Rest]):-
        D == 0,!,
        Nh is H+1,
        write('|---'),
        printDiv(Nh,S,Rest).
printDiv(H,S,[D|Rest]):-
        D == 1,!,
        Nh is H+1,
        write('|XXX'),
        printDiv(Nh,S,Rest).
