:- module(hdrug_feature_ops,[]).
%% to prevent 
%% {Warning: This file, loaded into module hdrug_feature,
%%           is compiled in context of module user}


:- op(402,xfy,(:)).	      	             % paths
:- op(800,xfy,['<?=?>', '/=>', '=>', '==>', '<=>','<==>','===>', '=*>','=?>', '==?>']).  
:- op(300,fx,['@','`','=']).                 % functional expressions
:- op(400,xfy,&).                            % boolean expressions
:- op(400,xfy,&&).                            % boolean expressions
:- op(390,fx,~).                             % boolean expressions


