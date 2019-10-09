sentence(1,[john,leaves]).
sentence(2,[john,sees,mark]).
sentence(3,[mark,calls,you,up,often]).

lf(1,(pred:decl,
      args:[(pred:call_up,
	     args:[pred:mary,
		   pred:john
		  ]
	    )
	   ]
     )).


  
