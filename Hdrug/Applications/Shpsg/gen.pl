:- multifile macro/2.

macro(sg_d, {the,a}).
macro(pl_d, {the}).

macro(sg_n, {dog,cat,park,book,woman,man,person,telescope,fountain}).
macro(pl_n, {dogs,cats,parks,books,women,men,people,telescopes,fountains}).

macro(p, {in,on,with}).

macro(vi_sg, {barks,barked}).
macro(vi_pl, {bark,barked}).

macro(vt_sg, {chases,sees,chased,saw}).
macro(vt_pl, {chase,chased,see,saw}).

macro(vd_sg, {puts,put}).
macro(vd_pl, {put}).

macro(np_s,[sg_d,' ',sg_n]).
macro(np_p,[pl_d,' ',pl_n]).

macro(pp, [[p,' ',{np_s,np_p}],' ']).
macro(pps, [[' ',p,' ',{np_s,np_p}]^,' ']).

%macro(si_sg, [np_s,pps,vi_sg,pps,'.']).
%macro(si_pl, [np_p,pps,vi_pl,pps,'.']).

macro(si_sg, [np_s,pps,vi_sg,pps,pp^]).
macro(si_pl, [np_p,pps,vi_pl,pps,pp^]).
macro(st_sg, [np_s,pps,vt_sg,' ',{np_s,np_p},pps,pp^]).
macro(st_pl, [np_p,pps,vt_pl,' ',{np_s,np_p},pps,pp^]).
macro(sd_sg, [np_s,pps,vd_sg,' ',{np_s,np_p},pps,pp]).
macro(sd_pl, [np_p,pps,vd_pl,' ',{np_s,np_p},pps,pp]).

macro(s,{si_sg,si_pl,st_sg,st_pl,sd_sg,sd_pl}).
%macro(s,{si_sg,si_pl,st_sg,st_pl}).

	  
     
