reportHelper([],[],BR,LU,DI,BR,LU,DI).

reportHelper(PQ,PR,ABR,ALU,ADI,BR,LU,DI):-
    PQ=[H1|T1],
    PR=[_|T2],
    H1=[i,ate,X,for,breakfast],
    reportHelper(T1,T2,[X|ABR],ALU,ADI,BR,LU,DI).

reportHelper(PQ,PR,ABR,ALU,ADI,BR,LU,DI):-
    PQ=[H1|T1],
    PR=[_|T2],
    H1=[i,ate,X,for,dinner],
    reportHelper(T1,T2,ABR,ALU,[X|ADI],BR,LU,DI).

reportHelper(PQ,PR,ABR,ALU,ADI,BR,LU,DI):-
    PQ=[H1|T1],
    PR=[_|T2],
    H1=[i,ate,X,for,lunch],
    reportHelper(T1,T2,ABR,[X|ALU],ADI,BR,LU,DI).

reportHelper(PQ,PR,ABR,ALU,ADI,BR,LU,DI):-
    PQ=[H1|T1],
    PR=[H2|T2],
    H1=[can,i,have,X,for,breakfast],
    H2=["You",can,have,X,for,breakfast],
    reportHelper(T1,T2,[X|ABR],ALU,ADI,BR,LU,DI).

reportHelper(PQ,PR,ABR,ALU,ADI,BR,LU,DI):-
    PQ=[H1|T1],
    PR=[H2|T2],
    H1=[can,i,have,X,for,lunch],
    H2=["You",can,have,X,for,lunch],
    reportHelper(T1,T2,ABR,[X|ALU],ADI,BR,LU,DI).

reportHelper(PQ,PR,ABR,ALU,ADI,BR,LU,DI):-
    PQ=[H1|T1],
    PR=[H2|T2],
    H1=[can,i,have,X,for,dinner],
    H2=["You",can,have,X,for,dinner],
    reportHelper(T1,T2,ABR,ALU,[X|ADI],BR,LU,DI).

reportHelper(PQ,PR,ABR,ALU,ADI,BR,LU,DI):-
    PQ=[_|T1],
    PR=[_|T2],
    reportHelper(T1,T2,ABR,ALU,ADI,BR,LU,DI).
reportHelper2([],[-]).
reportHelper2(A,A):-
    A\=[].
report(PQ,PR):-
    reportHelper(PQ,PR,[],[],[],BR,LU,DI),
    reportHelper2(BR,BR2),
    reportHelper2(LU,LU2),
    reportHelper2(DI,DI2),
    ws(["You",ate,""]),
    ws(BR2),
    writeln(' for breakfast.'),
    ws(["You",ate,""]),
    ws(LU2),
    writeln(' for lunch.'),
    ws(["You",ate,""]),
    ws(DI2),
    writeln(' for dinner.').

totalCal(1800).

readInputTillQuit:-
    writeln(">This is your personal assistant how can i help you?"),
    readInputTillQuit([],[]),!.

readInputTillQuit(PQ,PR):-
    write('>'),
    res(Q1),
    append(Q,[_],Q1),
    Q\=[quit],
    response(Q,PQ,PR,R),
    write('>'),
    ws(R),
    writeln(""),
    branch(Q,R,PQ,PR).
readInputTillQuit(PQ,PR):-
    branch([quit],_,PQ,PR),
    writeln("Bye").

branch([quit],_,PQ,PR):-
     report(PQ,PR),!.

branch(Q,R,PQ,PR):-
    %isValid(Q),
    Q\=[quit],
     PR2=[R|PR],
     PQ2=[Q|PQ],
     readInputTillQuit(PQ2,PR2).

%start of reply rules

response(Q,_,_,R):-
     Q=['how','many','calories','does',F,'contain'],
     foodCal(F,C),
     R=[C,"Calories"].
response(Q,_,_,R):-
     Q=['how','many','calories','does',F,'contain'],
     \+foodCal(F,_),
     R=["I",do,not,know].

%end of question a
response(Q,PQ,PR,[R]):-
     Q=['what','does',F,'contain'],
     %setof(X,prop(F,contain,X),CR),
     filterProp(contain,L1),
     matchFirst(F,L1,L2),
     bestMatchesMin(L2,1,CR),
     length(CR,N),
     N>0,
     getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,R):-
     Q=['what','does',F,'contain'],
     %setof(X,prop(F,contain,X),CR),
     filterProp(contain,L1),
     matchFirst(F,L1,L2),
     bestMatchesMin(L2,1,CR),
     length(CR,N),
     N>0,
     \+getDiffAnswer(Q,PQ,PR,CR,_),
     R=["I",told,you,that,before].

response(Q,_,_,R):-
     Q=['what','does',F,'contain'],
     \+prop(F,contain,_),
     R=["I",do,not,know].

%end of question b


response(Q,PQ,PR,R):-
    Q=['can','i','have',F,'for',T],
    calcCalories(F,PQ,PR,C),
    meal(T),
    C>=0,
    \+prop(F,not,T),
    R=["You",can,have,F,for,T].

response(Q,_,_,R):-
     Q=['can','i','have',F,'for',T],
     prop(F,not,T),
     R=[F,"is",not,suitable,for,T].
response(Q,PQ,PR,R):-
     Q=['can','i','have',F,'for',T],
     meal(T),
     calcCalories(F,PQ,PR,C),
     C<0,
     \+prop(F,not,T),
     R=["No"].
response(Q,PQ,PR,R):-
     Q=['can','i','have',F,'for',T],
     \+prop(F,not,T),
     (\+calcCalories(F,PQ,PR,_);\+meal(T)),
     R=["I",do,not,know].

%end of question c

response(Q,PQ,PR,[R]):-
     Q=['what',is,F],
     %setof(X,prop(F,is,X),CR),
     filterProp(is,L1),
     matchFirst(F,L1,L2),
     bestMatchesMin(L2,1,CR),
     length(CR,N),
     N>0,
     getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,PQ,PR,R):-
     Q=['what',is,F],
     %setof(X,prop(F,is,X),CR),
     filterProp(is,L1),
     matchFirst(F,L1,L2),
     bestMatchesMin(L2,1,CR),
     length(CR,N),
     N>0,
     \+getDiffAnswer(Q,PQ,PR,CR,_),
     R=["I",told,you,that,before].

response(Q,_,_,R):-
     Q=['what','is',X],
     \+prop(X,is,_),
     R=["I",do,not,know].

%end of question d

response(Q,PQ,PR,R):-
    Q=[how,many,calories,do,i,have,left],
    foodFromHistory(PQ,PR,FL),
    foodCalList(FL,C),
    totalCal(T),
    C2 is T-C,
    R=[C2,"Calories"].

response(Q,PQ,PR,R):-
     Q=[how,many,calories,do,i,have,left],
     foodFromHistory(PQ,PR,FL),
     \+foodCalList(FL,_),
     R=["I",do,not,know].
%end of question e

%response(Q,_,_,["I",do,not,know]) :-
%    Q = ['what',kind,of,FC,does,F,contain],
%    ((\+ prop(_,_,FC));
%    (\+food(F))).
%
%response(Q,_,_,["Nothing",from,what,i,know]) :-
%    Q = ['what',kind,of,FC,does,F,contain],
%    prop(_,_,FC),
%    food(F),
%    filterProp(contain,L1),
%    filterProp(is,L2),
%    matchFirst(F,L1,R1),
%    matchSecond(FC,L2,R2),
%    mergeMatchLists(R1,R2,L3),
%    bestMatchesMin(L3,2,CR),
%    length(CR,0).
%
%response(Q,PQ,PR,[R]) :-
%    Q = ['what',kind,of,FC,does,F,contain],
%    prop(_,_,FC),
%    food(F),
%    filterProp(contain,L1),
%    filterProp(is,L2),
%    matchFirst(F,L1,R1),
%    matchSecond(FC,L2,R2),
%    mergeMatchLists(R1,R2,L3),
%    bestMatchesMin(L3,2,CR),
%    length(CR,N),
%    N >= 1,
%    getDiffAnswer(Q,PQ,PR,CR,R).
%
%response(Q,PQ,PR,["I",told,you,that,before]) :-
%    Q = ['what',kind,of,FC,does,F,contain],
%    prop(_,_,FC),
%    food(F),
%    filterProp(contain,L1),
%    filterProp(is,L2),
%    matchFirst(F,L1,R1),
%    matchSecond(FC,L2,R2),
%    mergeMatchLists(R1,R2,L3),
%    bestMatchesMin(L3,2,CR),
%    length(CR,N),
%    N >= 1,
%    \+getDiffAnswer(Q,PQ,PR,CR,_).

response(Q,PQ,PR,[R]):-
    Q=[what,kind,of,X,does,F,contain],
    %prop(F,contain,R),
    %prop(R,is,X),
    setof(R,(prop(F,contain,R),prop(R,is,X)),CR),
    getDiffAnswer(Q,PQ,PR,CR,R).

response(Q,_,_,R):-
    Q=[what,kind,of,X,does,F,contain],
    food(F),
    foodCategory(X),
    \+setof(R,(prop(F,contain,R),prop(R,is,X)),_),
    R=["Nothing",from,what,i,know].

response(Q,PQ,PR,R):-
    Q=[what,kind,of,X,does,F,contain],
    setof(R,(prop(F,contain,R),prop(R,is,X)),CR),
    \+getDiffAnswer(Q,PQ,PR,CR,R),
    R=["I",told,you,that,before].

response(Q,_,_,R):-
    Q=[what,kind,of,X,does,F,contain],
    (\+food(F);\+foodCategory(X)),
    R=["I",do,not,know].
%End of question f

response(Q,_,_,["Yes"]):-
   Q=['is',X,'a',Y,'in',Z],
   prop(X,is,Y),
   prop(Z,contain,X).

response(Q,_,_,["No"]):-
   Q=['is',X,'a',Y,'in',Z],
   food(Z),
   foodCategory(Y),
   food(X),
  (\+prop(X,is,Y); \+ prop(Z,contain,X)).


response(Q,_,_,["I",do,not,know]):-
   Q=['is',X,'a',Y,'in',Z],
   ( \+food(Z);
   \+foodCategory(Y);
  \+ food(X)).

%end of question g



response(Q,PQ,PR,[R]):-
    Q=['what','can',i,'have','for',Z,'that','contains',Y],
    getUnlikedIngredients(PQ,UI),
    meal(Z),
    setof(A,possiblefood(UI,Y,Z,PQ,PR,A),X),
    getDiffAnswer(Q,PQ,PR,X,R).

response(Q,PQ,PR,["I",told,you,that,before]):-
    Q=['what','can',i,'have','for',Z,'that','contains',Y],
    getUnlikedIngredients(PQ,UI),
    meal(Z),
    setof(A,possiblefood(UI,Y,Z,PQ,PR,A),X),
    \+getDiffAnswer(Q,PQ,PR,X,_).

response(Q,PQ,PR,R):-
    Q=['what','can',i,'have','for',X,'that','contains',Y],
    getUnlikedIngredients(PQ,UI),
    food(Y),
    meal(X),
    calcCalories(pizza,PQ,PR,_),
    \+ setof(I,possiblefood(UI,Y,X,PQ,PR,I),_),
    R=["Nothing",from,what,i,know].

response(Q,PQ,PR,["I",do,not,know]):-
    Q=['what','can',i,'have','for',X,'that','contains',Y],
   (\+ meal(X);\+food(Y);\+calcCalories(pizza,PQ,PR,_)).
%end of question h

response(Q,_,_,["Ok"]):-
    Q=[i,ate,_,for,_].

response(Q,_,_,["Ok"]):-
    Q=[i,do,not,eat,_].

response([quit],_,_,["Bye"]).

response(Q,_,_,R):-
    \+isValid(Q),
    R=["I",do,not,understand,you].

% end of replies
filterUnliked(UI,F):-
    setof(I,prop(F,contain,I),S),
    intersection(UI,S,[]).
possiblefood(UI,Ingredient,Meal,PQ,PR,A):-
    prop(A,contain,Ingredient),
    \+prop(A,not,Meal),
    filterUnliked(UI,A),
    calcCalories(A,PQ,PR,C),
    C>=0.

food(A):-
   setof(X,(prop(X,contain,_,_);prop(X,contain,_);prop(X,is,_);prop(X,not,_)),Y),
   member(A,Y).
meal(X):-
    X=breakfast;X=dinner;X=lunch.

foodCategory(X):-
    prop(_,is,X).

isValid(L):-
    L=['how','many','calories','does',_,'contain'];
    L=['what','does',_,'contain'];
    L=['can','i','have',_,'for',_];
    L=['what','is',_];
    L=['how','many','calories','do','i','have','left'];
    L=['what','kind','of',_,'does',_,'contain'];
    L=['is',_,a,_,in,_];
    L=['what','can','i','have','for',_,'that','contains',_];
    L=['i','ate',_,'for',_];
    L=['i','do','not','eat',_];
    L=['quit'].


filterProp(Relation,Result):-
     setof(L,filterPropHelper(Relation,L),Result).
filterPropHelper(Relation,Result):-
     prop(A,Relation,B),
     Result=(A,B).

foodCal(F,C):-
   setof(I,prop(F,contain,I),L),
   foodCalList(L,C).
foodCal(F,C):-
     prop(F,contain,C,cal).

foodCalList(FL,R):-
    myCal(FL,R).

myCal([],0).
myCal([H|T],R):-
    foodCal(H,R1),
    myCal(T,R2),
    R is R1+R2.

calcCalories(F,PQ,PR,C):-
    foodFromHistory(PQ,PR,FL),
    foodCalList(FL,C2),
    totalCal(T),
    foodCal(F,C3),
    C is T-C2-C3.

matchFirst(_,[],[]).
matchFirst(T1,LF,LM):-
    LM=[E-1|Tail1],
    LF=[(T1,E)|Tail2],
    matchFirst(T1,Tail2,Tail1).

matchFirst(T1,LF,LM):-
    LM=[Y-0|Tail1],
    LF=[(X,Y)|Tail2],
    T1\=X,
    matchFirst(T1,Tail2,Tail1).

matchSecond(_,[],[]).
matchSecond(T1,LF,LM):-
    LM=[E-1|Tail1],
    LF=[(E,T1)|Tail2],
    matchSecond(T1,Tail2,Tail1).

matchSecond(T1,LF,LM):-
    LM=[X-0|Tail1],
    LF=[(X,Y)|Tail2],
    T1\=Y,
    matchSecond(T1,Tail2,Tail1).

mergeMatchLists(ML1,ML2,R):-
    getNOdub(ML1,R1),
    getNOdub(ML2,R2),
    getAc(R1,R2,R).

getAc([],ML2,ML2).
getAc([H|T],ML2,R):-
    getNOdub([H|ML2],R1),
    getAc(T,R1,R).

getNOdub([],[]).
getNOdub([H|T],R):-
    getNOdubHelper(H,T,R1),
    rem(H,T,R2),
    getNOdub(R2,R3),
    R =[R1|R3].

getNOdubHelper(X,[],X).
getNOdubHelper(X1-Y1,[X1-Y2|T],R):-
    Y3 is Y1+Y2,
    getNOdubHelper(X1-Y3,T,R).
getNOdubHelper(X1-Y1,[X2-_|T],R):-
    X1\=X2,
    getNOdubHelper(X1-Y1,T,R).

rem(_,[],[]).
rem(X-Y,[X-_|T],R):-
    rem(X-Y,T,R).
rem(X-Y,[X1-Y2|T],[X1-Y2|R]):-
    X\=X1,
    rem(X-Y,T,R).

bestMatches(ML,BL):-
    getNOdub(ML,R1),
    listOrderDesc(R1,R2),
    splitter(R2,BL).

splitter([X-Y|T],[X|R]):-
    splitterHelper(T,Y,[],R).

splitterHelper([],_,Ac,Ac).
splitterHelper([X-Y|T],Y,Ac,R):-
    splitterHelper(T,Y,[X|Ac],R).
splitterHelper([_-Y|_],Z,Ac,Ac):-
    Y\=Z.

bestMatchesMin(ML,Min,BL):-
    getNOdub(ML,R1),
    listOrderDesc(R1,R2),
    R2 = [_-Y|_],
    Y >= Min,
    splitter(R2,BL).
bestMatchesMin(ML,Min,[]):-
    getNOdub(ML,R1),
    listOrderDesc(R1,R2),
    R2 = [_-Y|_],
    \+ (Y >= Min).

insertionSort(List,Sorted):-
     i_sort(List,[],Sorted).
i_sort([],X,X).
i_sort([H|T],Accumulator,Sorted):-
     insert(H,Accumulator,N),i_sort(T,N,Sorted).
insert(X1-X2,[Y1-Y2|T],[Y1-Y2|NT]):-X2=<Y2,insert(X1-X2,T,NT).
insert(X1-X2,[Y1-Y2|T],[X1-X2,Y1-Y2|T]):-X2>Y2.
insert(X,[],[X]).

listOrderDesc(LP,OLP):-
    insertionSort(LP,OLP).

getUnlikedIngredients([],[]).
getUnlikedIngredients(PQ,FL):-
     PQ=[H|T],
     H=[i,do,not,eat,X],
     FL=[X|R],
     getUnlikedIngredients(T,R).
getUnlikedIngredients(PQ,FL):-
     PQ=[H|T],
     H\=[i,do,not,eat,_],
     getUnlikedIngredients(T,FL).

foodFromHistory(PQ,PR,FL):-
    append(PQ,PR,HL),
    foodFromHistory(HL,FL).
foodFromHistory([],[]).
foodFromHistory(HL,FL):-
    HL=[H|T],
    H=[i,'ate',X,'for',_],
    FL=[X|R],
    foodFromHistory(T,R).

foodFromHistory(HL,FL):-
    HL=[H|T],
    H=["You",can,have,F,for,_],
    FL=[F|R],
    foodFromHistory(T,R).
foodFromHistory(HL,FL):-
    HL=[H|T],
    H\=[i,ate,_,for,_],
    H\=["You",can,have,_,for,_],
    foodFromHistory(T,FL).
getDiffAnswer(Q,PQ,PR,CR,FR):-
      gDAhelper(Q,PQ,PR,AR),
      member(FR,CR),
     \+member(FR,AR),!.

% get all responses in PR that correspond to Q
gDAhelper(_,[],[],[]).
gDAhelper(Q,PQ,PR,R):-
     PQ=[Q|T1],
     PR=[[R1]|T2],
     R=[R1|R2],
     gDAhelper(Q,T1,T2,R2).
gDAhelper(Q,PQ,PR,R):-
     PQ=[H1|T1],
     PR=[_|T2],
     H1\=Q,
     gDAhelper(Q,T1,T2,R).
responseO(Q,PQ,PR,LR):-
    Q=[what,can,i,have,for,M,that,contains,I],
    setof(F-S,(food(F),getScore(F,M,I,PQ,PR,S)),LR).
getScore(F,M,I,PQ,PR,R):-
    getScoreH1(F,I,R1),
    getScoreH2(F,M,R2),
    getScoreH3(F,PQ,R3),
    getScoreH4(F,PQ,PR,R4),
    R is R1+R2+R3+R4.
 %1 if f contains I
getScoreH1(F,I,1):-
    prop(F,contain,I).

getScoreH1(F,I,0):-
    \+prop(F,contain,I).
%1 if F is suitable for M
getScoreH2(F,M,1):-
    \+prop(F,not,M).
getScoreH2(F,M,0):-
    prop(F,not,M).
%1 if F doesnt contain anything in UI
getScoreH3(F,PQ,1):-
    getUnlikedIngredients(PQ,UI),
    filterUnliked(UI,F).
getScoreH3(F,PQ,0):-
    getUnlikedIngredients(PQ,UI),
    \+filterUnliked(UI,F).
%1 if eating F will not exceed calories limit
getScoreH4(F,PQ,PR,1):-
        calcCalories(F,PQ,PR,C),
        C>=0.
getScoreH4(F,PQ,PR,0):-
        calcCalories(F,PQ,PR,C),
        C<0.



:-discontiguous(prop/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: INGREDIENTS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% VEGETABLES %%%%

prop(tomato,is,vegetable).
prop(onion,is,vegetable).
prop(bell_pepper,is,vegetable).
prop(chili_pepper,is,vegetable).
prop(carrot,is,vegetable).
prop(pea,is,vegetable).
prop(artichoke,is,vegetable).
prop(eggplant,is,vegetable).
prop(cucumber,is,vegetable).
prop(lettuce,is,vegetable).
prop(okra,is,vegetable).
prop(cauliflower,is,vegetable).
prop(cabbage,is,vegetable).
prop(broccoli,is,vegetable).
prop(mushroom,is,vegetable).
prop(potato,is,vegetable).
prop(zucchini,is,vegetable).
prop(broccoli,is,vegetable).
prop(spinach,is,vegetable).
prop(corn,is,vegetable).

%%%% FRUITS %%%%

prop(strawberry,is,fruit).
prop(blackberry,is,fruit).
prop(blueberry,is,fruit).
prop(banana,is,fruit).
prop(orange,is,fruit).
prop(grape,is,fruit).
prop(pineapple,is,fruit).
prop(apple,is,fruit).
prop(kiwi,is,fruit).
prop(peaches,is,fruit).
prop(guava,is,fruit).
prop(pear,is,fruit).
prop(mango,is,fruit).
prop(apricot,is,fruit).
prop(avocado,is,fruit).
prop(cherry,is,fruit).
prop(fig,is,fruit).
prop(coconut,is,fruit).
prop(lemon,is,fruit).
prop(watermelon,is,fruit).
prop(cantaloupe,is,fruit).

%%%% DIARY %%%%

prop(cheese,is,diary).
prop(milk,is,diary).
prop(yogurt,is,diary).

%%%% CARBS %%%%

prop(flour,is,carb).
prop(rice,is,carb).
prop(pasta,is,carb).
prop(chocolate,is,carb).

%%%% FATS %%%%

prop(oil,is,fat).
prop(butter,is,fat).

%%%% PROTEINS %%%%

prop(egg,is,protein).
prop(fish,is,protein).
prop(chicken,is,protein).
prop(meat,is,protein).
prop(shrimp,is,protein).
prop(minced_meat,is,protein).

%%%% DRESSING %%%%

prop(mayonnaise,is,dressing).
prop(vinegar,is,dressing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: INGREDIENTS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: RECIPES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(chicken_caesar_salad,contain,chicken).
prop(chicken_caesar_salad,contain,oil).
prop(chicken_caesar_salad,contain,lettuce).
prop(chicken_caesar_salad,contain,cheese).
prop(chicken_caesar_salad,contain,mayonnaise).
prop(chicken_caesar_salad,contain,vinegar).
prop(chicken_caesar_salad,contain,bread).

prop(green_salad,contain,carrot).
prop(green_salad,contain,bell_pepper).
prop(green_salad,contain,lettuce).
prop(green_salad,contain,onion).
prop(green_salad,contain,tomato).
prop(green_salad,contain,cucumber).

prop(coleslaw_salad,contain,carrot).
prop(coleslaw_salad,contain,cabbage).
prop(coleslaw_salad,contain,mayonnaise).
prop(coleslaw_salad,contain,oil).

prop(pasta_salad,contain,bell_pepper).
prop(pasta_salad,contain,mayonnaise).
prop(pasta_salad,contain,pasta).
prop(pasta_salad,contain,corn).

prop(fruit_salad,contain,strawberry).
prop(fruit_salad,contain,banana).
prop(fruit_salad,contain,orange).
prop(fruit_salad,contain,apple).

prop(croissant,contain,butter).
prop(croissant,contain,flour).
prop(croissant,contain,milk).
prop(croissant,contain,oil).
prop(croissant,contain,egg).

prop(spanish_omelette,contain,egg).
prop(spanish_omelette,contain,oil).
prop(spanish_omelette,contain,potato).

prop(boiled_egg,contain,egg).

prop(grilled_chicken,contain,chicken).
prop(grilled_chicken,contain,lemon).
prop(grilled_chicken,contain,onion).

prop(fried_chicken,contain,chicken).
prop(fried_chicken,contain,oil).
prop(fried_chicken,contain,onion).
prop(fried_chicken,contain,flour).

prop(cake,contain,flour).
prop(cake,contain,butter).
prop(cake,contain,milk).
prop(cake,contain,egg).

prop(chocolate_cake,contain,cake).
prop(chocolate_cake,contain,chocolate).

prop(white_rice,contain,rice).
prop(white_rice,contain,butter).

prop(mexican_rice,contain,rice).
prop(mexican_rice,contain,oil).
prop(mexican_rice,contain,onion).
prop(mexican_rice,contain,tomato).

prop(ratatouille,contain,zucchini).
prop(ratatouille,contain,eggplant).
prop(ratatouille,contain,tomato).
prop(ratatouille,contain,bell_pepper).
prop(ratatouille,contain,onion).
prop(ratatouille,contain,lemon).
prop(ratatouille,contain,oil).
prop(ratatouille,contain,vinegar).

prop(lasagne,contain,pasta).
prop(lasagne,contain,milk).
prop(lasagne,contain,flour).
prop(lasagne,contain,butter).
prop(lasagne,contain,minced_meat).
prop(lasagne,contain,cheese).

prop(pasta_white_sauce,contain,pasta).
prop(pasta_white_sauce,contain,milk).
prop(pasta_white_sauce,contain,flour).
prop(pasta_white_sauce,contain,butter).

prop(pasta_red_sauce,contain,pasta).
prop(pasta_red_sauce,contain,tomato).
prop(pasta_red_sauce,contain,oil).

prop(pasta_alfredo,contain,pasta).
prop(pasta_alfredo,contain,milk).
prop(pasta_alfredo,contain,flour).
prop(pasta_alfredo,contain,butter).
prop(pasta_alfredo,contain,chicken).

prop(pasta_negresco,contain,pasta).
prop(pasta_negresco,contain,milk).
prop(pasta_negresco,contain,flour).
prop(pasta_negresco,contain,butter).
prop(pasta_negresco,contain,chicken).
prop(pasta_negresco,contain,cheese).

prop(shrimp_pasta,contain,pasta).
prop(shrimp_pasta,contain,shrimp).
prop(shrimp_pasta,contain,butter).
prop(shrimp_pasta,contain,milk).

prop(pizza,contain,tomato).
prop(pizza,contain,cheese).
prop(pizza,contain,flour).
prop(pizza,contain,oil).

prop(bread,contain,milk).
prop(bread,contain,flour).
prop(bread,contain,butter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: RECIPES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: CALORIES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(tomato,contain,11,cal).
prop(onion,contain,44,cal).
prop(cheese,contain,431,cal).
prop(egg,contain,78,cal).
prop(pasta,contain,131,cal).
prop(bell_pepper,contain,24,cal).
prop(chili_pepper,contain,18,cal).
prop(carrot,contain,25,cal).
prop(pea,contain,81,cal).
prop(artichoke,contain,120,cal).
prop(eggplant,contain,25,cal).
prop(cucumber,contain,32,cal).
prop(lettuce,contain,15,cal).
prop(okra,contain,33,cal).
prop(cauliflower,contain,25,cal).
prop(cabbage,contain,25,cal).
prop(broccoli,contain,31,cal).
prop(mushroom,contain,5,cal).
prop(potato,contain,163,cal).
prop(zucchini,contain,33,cal).
prop(spinach,contain,23,cal).
prop(corn,contain,86,cal).
prop(strawberry,contain,33,cal).
prop(blackberry,contain,43,cal).
prop(blueberry,contain,57,cal).
prop(banana,contain,89,cal).
prop(orange,contain,47,cal).
prop(grape,contain,62,cal).
prop(pineapple,contain,42,cal).
prop(apple,contain,92,cal).
prop(kiwi,contain,42,cal).
prop(peaches,contain,59,cal).
prop(guava,contain,38,cal).
prop(pear,contain,85,cal).
prop(mango,contain,99,cal).
prop(apricot,contain,48,cal).
prop(avocado,contain,160,cal).
prop(cherry,contain,50,cal).
prop(fig,contain,107,cal).
prop(coconut,contain,283,cal).
prop(lemon,contain,24,cal).
prop(watermelon,contain,30,cal).
prop(cantaloupe,contain,34,cal).
prop(milk,contain,124,cal).
prop(yogurt,contain,218,cal).
prop(flour,contain,364,cal).
prop(rice,contain,150,cal).
prop(oil,contain,240,cal).
prop(butter,contain,204,cal).
prop(fish,contain,305,cal).
prop(chicken,contain,335,cal).
prop(meat,contain,250,cal).
prop(shrimp,contain,85,cal).
prop(minced_meat,contain,332,cal).
prop(mayonnaise,contain,188,cal).
prop(vinegar,contain,3,cal).
prop(chocolate,contain,137,cal).
%prop(,contain,,cal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: CALORIES INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% START: MEALS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop(cheese,not,lunch).
prop(yogurt,not,lunch).
prop(boiled_egg,not,lunch).
prop(boiled_egg,not,dinner).
prop(spanish_omelette,not,lunch).
prop(spanish_omelette,not,dinner).
prop(croissant,not,lunch).
prop(chicken_caesar_salad,not,breakfast).
prop(chicken_caesar_salad,not,dinner).
prop(pizza,not,breakfast).
prop(shrimp_pasta,not,breakfast).
prop(shrimp_pasta,not,dinner).
prop(pasta_negresco,not,breakfast).
prop(pasta_negresco,not,dinner).
prop(pasta_alfredo,not,breakfast).
prop(pasta_alfredo,not,dinner).
prop(pasta_red_sauce,not,breakfast).
prop(pasta_red_sauce,not,dinner).
prop(pasta_white_sauce,not,breakfast).
prop(pasta_white_sauce,not,dinner).
prop(fried_chicken,not,breakfast).
prop(fried_chicken,not,dinner).
prop(grilled_chicken,not,breakfast).
prop(grilled_chicken,not,dinner).
prop(lasagne,not,breakfast).
prop(lasagne,not,dinner).
prop(ratatouille,not,breakfast).
prop(ratatouille,not,dinner).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% END: MEALS INFORMATION %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






%-------------------------------------------------------------
% res(-Sentence)
%-------------------------------------------------------------

res([FirstWord|RestOfSentence]) :-
  reSe([FirstWord|RestOfSentence]).

reSe([FirstWord|RestOfSentence]) :-
  get0(Char),
  readWord(Char,FirstWord,NextChar),
  readRestOfSentence(FirstWord,NextChar,RestOfSentence).

   %--- ancillaries to res -------------------------
   readRestOfSentence(Word,_,[]) :-
     endOfSentenceWord(Word),!.
   readRestOfSentence(_,Char,[NextWord|RestOfSentence]) :-
     readWord(Char,NextWord,NextChar),
     readRestOfSentence(NextWord,NextChar,RestOfSentence).

   readWord(Char,Word,NextChar) :-
     singleCharWord(Char),!,name(Word,[Char]),get0(NextChar).
   readWord(Char,Word,NextChar) :-
     componentChar(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord(TempNextChar,RestWord,NextChar),
     name(Word,[NewChar|RestWord]).
   readWord(_,Word,NextChar) :-
     get0(TempChar),
     readWord(TempChar,Word,NextChar).

   restWord(Char,[NewChar|RestWord],NextChar) :-
     componentChar(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord(TempNextChar,RestWord,NextChar).
     restWord(Char,[],Char).

   singleCharWord(44).  /* , */
   singleCharWord(59).  /* ; */
   singleCharWord(58).  /* : */
   singleCharWord(63).  /* ? */
   singleCharWord(33).  /* ! */
   singleCharWord(46).  /* . */

   componentChar(Char,Char) :- Char>96,Char<123.

   componentChar(Char,L) :- Char>64,Char<91,L is Char+32.

   componentChar(Char,L) :- Char>64,Char<91,L is Char+32.
   componentChar(Char,Char) :- Char>47,Char<58.
   componentChar(39,39).  /* ' */
   componentChar(45,45).  /* - */
   componentChar(95,95).  /* _ */

   endOfSentenceWord('.').
   endOfSentenceWord('!').
   endOfSentenceWord('?').

%-------------------------------------------------------------
% res_pc(-Sentence)
%-------------------------------------------------------------

res_pc([FirstWord|RestOfSentence]) :-
  reSe_pc([FirstWord|RestOfSentence]).

reSe_pc([FirstWord|RestOfSentence]) :-
  get0(Char),
  readWord_pc(Char,FirstWord,NextChar),
  readRestOfSentence_pc(FirstWord,NextChar,RestOfSentence).

   %--- ancillaries to res_pc -------------------------
   readRestOfSentence_pc(Word,_,[]) :-
     endOfSentenceWord(Word),!.
   readRestOfSentence_pc(_,Char,[NextWord|RestOfSentence]) :-
     readWord_pc(Char,NextWord,NextChar),
     readRestOfSentence_pc(NextWord,NextChar,RestOfSentence).

   readWord_pc(Char,Word,NextChar) :-
     singleCharWord(Char),!,name(Word,[Char]),get0(NextChar).
   readWord_pc(Char,Word,NextChar) :-
     componentChar_pc(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord_pc(TempNextChar,RestWord,NextChar),
     name(Word,[NewChar|RestWord]).
   readWord_pc(_,Word,NextChar) :-
     get0(TempChar),
     readWord_pc(TempChar,Word,NextChar).

   restWord_pc(Char,[NewChar|RestWord],NextChar) :-
     componentChar_pc(Char,NewChar),
     !,
     get0(TempNextChar),
     restWord_pc(TempNextChar,RestWord,NextChar).
     restWord_pc(Char,[],Char).

   componentChar_pc(Char,Char) :- Char>96,Char<123.

   componentChar_pc(Char,Char) :- Char>64,Char<91.

   componentChar_pc(Char,L) :- Char>64,Char<91,L is Char+32.
   componentChar_pc(Char,Char) :- Char>47,Char<58.
   componentChar_pc(39,39).  /* ' */
   componentChar_pc(45,45).  /* - */
   componentChar_pc(95,95).  /* _ */

%-------------------------------------------------------------
% ws(+Sentence)
%-------------------------------------------------------------

ws([F|R]) :-
   write(F),
   wrs(R).

   %--- ancillaries to ws ------------------------
   wrs([F|R]) :-
     write(' '),
     write(F),
     wrs(R).
   wrs([]).

%-------------------------------------------------------------
% space/0
%-------------------------------------------------------------

space :- write(' ').

%-------------------------------------------------------------
% rs(-String)
%-------------------------------------------------------------

rs(S) :-
   get0(C),
   (
      C == -1,  S = [], !, fail;
      C == 10,  S = [], ! ;
      C == 32, !, rs(S);
      !, rs(C,S)
   ).

rs(C,[C|Cs]) :-
   get0(D),
   (
      D == -1,  Cs = [], !, fail;
      D == 10,  Cs = [], ! ;
      D == 32,  Cs = [], ! ;
      !, rs(D,Cs)
   ).


%-------------------------------------------------------------
% wrst(+String)
%-------------------------------------------------------------

wrst([]) :- !.
wrst([C|Cs]) :- put(C), wrst(Cs).
