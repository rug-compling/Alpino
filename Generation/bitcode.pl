:- module(alpino_bitcode,[ give_bits/2,
			   single_bitcode/1,
			   combine_bitcodes/3,
			   next_bitcode/3,
			   merge_bc_list/2
			 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Bitcoding helper predicates %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Combine two bitcodes, fail if there is bitcode overlap.
combine_bitcodes(BcA,BcB,NewBc) :-
    BcA /\ BcB =:= 0,
    NewBc is BcA \/ BcB.

%% Give N bits with a value of 1.
give_bits(N,Bits) :-
    (   N =:= 0
    ->  Bits = 0
    ;  	NNew is N - 1,
	give_bits(NNew,NewBits),
	Bits0 is NewBits << 1,
	Bits is Bits0 \/ 1
    ).

%% During bitcoding, a verb also get bitcodes for particles associated
%% with it. When the particles should be a separate lexical item, we'll
%% want to 1. assign bitcodes to the particles, 2. don't use these
%% particles for the verb lexical item. Since we have assigned consecutive
%% bitcodes, we can determine the next free bitcode to be given to a
%% particle easily:
%%
%% Bitcode: 0011100
%% << 1   : 0111000
%% ---------------- and 
%% After  : 0011000
%% Bitcode: 0011100
%% ---------------- xor
%% Next   : 0000100
%%
%% next_bitcode gives the next bitcode to use for a particle and the
%% bitcode after subtraction of the particle bitcode. When all particle
%% bitcodes are extracted in this manner, the leftmost bit of the
%% original bit becomes the verb bit.
next_bitcode(Bitcode,BitcodeAfter,NextBitcode) :-
    BitcodeShl is Bitcode << 1,
    BitcodeAfter is Bitcode /\ BitcodeShl,
    NextBitcode is Bitcode # BitcodeAfter.

%% Combine all bitcodes in a list. Fails when bitcodes overlap.
merge_bc_list(Bcs,Bc) :-
    merge_bc_list(Bcs,0,Bc).

merge_bc_list([],Bc,Bc).
merge_bc_list([H|T],Bc0,Bc) :-
    H /\ Bc0 =:= 0,
    Bc1 is Bc0 \/ H,
    merge_bc_list(T,Bc1,Bc).

%% bitcode represents presence of a single root
single_bitcode(Bitcode) :-
    BitcodeShl is Bitcode << 1,
    0 is Bitcode /\ BitcodeShl.

