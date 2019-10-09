include ../Makefile.include

PLS=\
 hdrug_chart.pl  hdrug_feature.pl   hdrug_show.pl\
 hdrug_feature_ops.pl  hdrug_stats.pl\
 hdrug_call_tree.pl    hdrug_gui.pl          hdrug_tk.pl\
 hdrug_clig.pl         hdrug_latex.pl        hdrug_txt.pl\
 hdrug_cmdint.pl       hdrug_main.pl         hdrug_util.pl\
 help.pl               hdrug_dot.pl          clp_assert.pl

ifeq "$(PROLOG)" "sicstus"
hdrug.sav: $(PLS)
	-echo "use_module(hdrug_main),\
               save_program('hdrug.sav',hdrug_main)." | $(SICSTUS) 
else

hdrug: $(PLS) swi.pl
	swipl -o hdrug --goal=hdrug_main -c swi.pl

endif

xref:
	spxref  -i ../../xref.pl -w - hdrug_main.pl hooks.pl \
        | grep -v 'can not follow'
	@echo "12 warnings expected"
