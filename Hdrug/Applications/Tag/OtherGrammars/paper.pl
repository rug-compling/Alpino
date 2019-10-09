%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% TAG GRAMMAR %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% DATA STRUCTURES %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% DECLARATIONS FOR PARSER %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

address(node(_,Address),Address).

user_top_category(Name,node(Name,_)).

ignore_semantics(Node,Node).

user_syn(node(Syn,_),Syn).

user_top(node(Top,_),Top).

user_bottom(node(Bot,_),Bot).

user_sem(node(_,_),_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% PRETTY PRINT %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user_show(syn,Cat,Res):-
	user_syn(Cat,Res).

:- user:usr_cmd([alias,s,show,-,g,syn]).
:- user:usr_cmd([alias,sg,show,-,gm,syn]).
:- user:usr_cmd([alias,w,w,-,g,syn]).
:- user:usr_cmd([alias,wg,w,-,gm,syn]).
:- user:usr_cmd([alias,r,c,'../tag','../tag_bt','../tag_bt_d']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% The elementary trees %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(alpha_1,node(wh,_):who).

init(alpha_2,node(np,_):john).

init(alpha_3,node(np,_):mary).

init(alpha_4,node(s,_):
      [* node(wh,_),
       + node(sbar,_):
         [  node(c,_):[],
          + node(s,_):
            [* node(np,_),
             + node(vp,_):
               [node(v,_):saw]]]]).

aux(beta_1,node(sbar,_):
      [  node(aux,_):did,
       + node(s,_):
         [* node(np,_),
          + node(vp,_):
            [  node(vp,_):
                 [node(v,_):say],
             + = node(sbar,_)]]]).

aux(beta_2,node(vp,_):
     [ + =node(vp,_),
          node(pp,_) :
          [ + node(p,_):with,
            * node(np,_) ] ] ).

aux(beta_3,node(vp,_):
     [ + = node(vp,_),
           node(adv,_):today ]).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% end of file %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

