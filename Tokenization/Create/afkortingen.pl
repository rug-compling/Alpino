
:- multifile macro/2.
:- multifile rx/2.

%%% tokenizer
%%% input: every paragraph is on a single line
%%% output: every sentence is on a single line; word-tokens are
%%%         separated by a single space

macro( words_afkortingen,
       cap_words([
		  %% these were in words_afkortingen_after before, but that led to too many sentence breaks
                  %% aug already was here anyway
		  %% jan is now in words_afkorting_with_number since it is often followed by a day or year
		  apr,dec,feb,febr,jul,jun,maa,okt,nov,sep,sept,

		  
   aant,acad,acc,adm,'Am',adr,adv,afb,afd,afgel,afk,afl,aggl,alc,ald,ambas,ann,ant,
                 antr,amerik,'Arab',arr,artill,artt,aug,av,'Ave',
   beg,beh,'Bel',beleidsmedew,bep,ber,bet,betr,bew,bez,bijg,bijv,bijw,bijz,
                 biol,bl,blvd,bladz,bldz,blz,bn,br,'Brab','Bros','Bulg',bvb,
   ca,'Can',cat,caval,centr,cf,cfr,ch,chem,'Chin',chor,'Chr',chr,'Cic',cie,cit,cl,'Col',
                 commerc,comp,'Conn',cons,corp,cq,cs,ct,
   'D.C',dbl,dd,decb,decemb,deelw,'Del','Den',dep,dept,'Deut',dgl,di,dir,diss,distr,div,dl,dld,dln,dmv,doct,
                 doelp,doopsgez,dtsl,'Du',dui,duitsl,dwz,
   echtgen,econ,ed,edd,eds,eendr,eetl,eff,eig,eigenn,el,
                   elektr,electr,'em.pred',
                   'Eng','Ex',enkv,ent,enw,ev,evang,
		 evt,exec, excl,exx,
   fa,fam,feat,ffrs,fig,fin,fiz,fl,'FL',fr,frs,'Fra',ft,
   'Gal',geb,gebr,ged,geïll,geill,'g.mv',
                   gelijkn,geh,gem,'Gen',gepens,gepubl,geref,gereform,
                   gespr,gest,gew,gl,gld,gr,
   'Had','Hand','Hebr',herh,herv,hfd,hon,'Hong',hoofdtelw,hoogl,hs,hss,hst,huisvr,huw,hyp,hz,'Hz',
   icm,idd,ill,incl,inc,ind,indiv,inf,infant,ing,ingel,inl,inst,inspect,
                   instell,int,intend,inv,inw,inz,ipv,'Isr','It','Ital',ivm,
   'Jak','Jer','Jes','Joeg',jrg,
   kand,kapt,kath,kcal,'Kerkgem',kerkgem,kl,kol,'Kon',kr,kw,
   'Lat','Lc',lett,'Lev',lib,lidw,losbl,lt,ltd,'Luc','Luk','luit.kol',luth,
   ma,mass,max,
          'Matt','Matth',mbt,'Md',med,medew,meerv,mg,mgr,mhd,'Mich',mil,milj,'Miss',ml,mld,mln,
	   mnd,mnl,mog,mrg,mrt,ms,mt,mtr,muz,mv,
   naamw,'niet-chr','niet-Chr','Nat.Lab',
                   nation,nav,'NB',nbr,ndl,ned,nederl,'Neh',niem,nl,nmd,nml,nom,
		 nr,nrs,'Num',nvdr,
   oa,octb,octob,ol,olv,ond,ondertit,onderz,oneig,ong,ongeh,oorspr,'Opb',openb,opn,opp,'oud-dir',oudfr,oudholl,
		 ouderl,ov,overl,overtr,
   'Pa',partn,pct,pCt,penningm,'1-pers','2-pers','1pers','2pers',pf,pl,plc,plm,plv,pnt,posth,pp,
		 praep,pred,pres,prod,prot,prov,ps,pseud,psych,pt,ptas,ptn,
   'radio-uitz',reb,'rector-magn',red,redbl,ref,reg,res,resp,ret,
		   rijksuniv,'r.-k','Rom','Russ',
   'S.A',samenst,samenw,'Sanskr',sc,scen,schr,sp,spec,spr,st,'ST','Ste',str,subsp,subst,symf,syn,
		  
   'Tac',techn,tek,tel,'tel.nr',temp,th,theel,tl,toegel,toep,toez,tov,
                   'Tsj','Tu','TV-uitz','tv-uitz',
   'U',uitg,uitk,ult,'vakantie-uitk',uitdr,uitgeg,uitz,univ,
   'V.S',var,vb,vbb,'Ver',verd,verl,verm,vert,vervr,verz,vgl,vlg,vlgg,'vice-vrz',vlp,
		 vm,vml,vnl,vnw,vols,volw,voorh,voorm,vr,vriendsch,
                 vrijg,vrijw,vrz,vs,vt,vz,'vice-vz','fr.vz',
   wd,wdb,wed,weled,werkw,werkz,wetensch,weth,wijkgem,wnd,wo,
   za,zach,zelfst,zg,zgn,zn,znw,zr,zw,'z.mv'
   ]) ).

macro(words_titel,cap_words([bc,dhr,dipl,dr,ds,drs,ir,jhr,mgr,mrs,mw,mej,mevr,mr,hr,prof])).

macro(words_afkorting_with_number,cap_words([art,no,op,zo,jan])).

macro(words_afkorting_after,cap_words(
  [ %apr,
    bv,bw,
    'n.Chr', 'v.Chr','v.C','n.C',
    cm,co,
    %dec,
   'e.a','e.d','e.v','e.v.a',
    enz,etc,
    %feb,febr,
    'Jap',jl,'j.l',jr,%jul,jun,
    kg,km,
    m,	%maa,
    mm,
    % nov,
    'o.i.d', %okt,
    pag,
    sec,%sep,sept,
    sr,
    tek
    ])).

macro(words_poule,cap_words(
  [formule,groep,poule,serie])).

macro(words_ambiguous_afkorting_after,cap_words(
  [
   ad,
   alg,alt,as,
   'Belg','Burg',
   bros,
   cat,christ,cult,
   dag,diss,do,dom,
   eng,ex,
   geest,
   ha,herz,
   infant,
   % jan,
   'Jap',
   kap,kon,kwal,
   lat,
   min,
   nat,nation,
   off,
   part,pol,
   'Petr','Port',
   'Rus',
   sticht,
   tent,'Tsjech',trad,
   vak,
   veren,verg,vol,volt,
   ww
  ])).

rx(cap_words(List),Fa) :-
    fsa_regex:list_atom_chars(List,CharList0),
    add_capitalization(CharList0,CharList,[]),
    fsa_dict:fsa_dict_to_fsa(CharList,Fa).

add_capitalization([],L,L).
add_capitalization([H0|T],L0,L) :-
    findall(H,add_cap(H0,H),L0,L1),
    add_capitalization(T,L1,L).

add_cap(X,X).
add_cap([H0|T],[NH|T]) :-
    atom_codes(H0,[H]),
    H >= 97,
    H =< 122,
    is(NH0,-(H,32)),
    atom_codes(NH,[NH0]).

