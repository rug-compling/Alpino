
tsentence(1,[a,dog,barks],4).
tsentence(2,[a,cat,barks],3).
tsentence(3,[a,dog,barked],1).
tsentence(4,[a,cat,barked],6).
tsentence(5,[the,dog,barks],2).
tsentence(6,[the,cat,barks],5).
tsentence(7,[the,dog,barked],7).
tsentence(8,[the,cat,barked],1).
tsentence(9,[the,dogs,bark],6).
tsentence(10,[the,cats,bark],1).
tsentence(11,[the,dogs,barked],8).
tsentence(12,[the,cats,barked],6).


sentence(A,B) :-
    tsentence(A,B,_).
