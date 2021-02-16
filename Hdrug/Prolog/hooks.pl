%% purpose of this file is twofold
%% -provide dummy definitions of hook predicates
%% -provide public declarations of unused (but useful) (library) predicates

:- public lists:append/3, lists:delete/3, lists:is_list/1, lists:last/2, 
    lists:member/2, lists:memberchk/2, lists:nextto/3, lists:no_doubles/1, 
    lists:non_member/2, lists:nth/3, lists:nth/4, lists:nth0/3, lists:nth0/4, 
    lists:permutation/2, lists:prefix/2, lists:remove_duplicates/2, 
    lists:reverse/2, lists:same_length/2, lists:same_length/3, lists:select/3, 
    lists:sublist/2, lists:substitute/4, lists:suffix/2, lists:max_list/2, 
    lists:min_list/2, lists:sum_list/2.

:- public assoc:assoc_to_list/2, assoc:empty_assoc/1, assoc:del_assoc/4, 
    assoc:del_max_assoc/4, assoc:del_min_assoc/4, assoc:gen_assoc/3, 
    assoc:get_assoc/3, assoc:get_assoc/5, assoc:get_next_assoc/4, 
    assoc:get_prev_assoc/4, assoc:is_assoc/1, assoc:list_to_assoc/2, 
    assoc:map_assoc/2, assoc:map_assoc/3, assoc:max_assoc/3, 
    assoc:min_assoc/3, assoc:ord_list_to_assoc/2, assoc:put_assoc/4.

:- public arrays:new_array/1, arrays:is_array/1, arrays:aref/3,
    arrays:arefa/3, arrays:arefl/3, arrays:aset/4, arrays:array_to_list/2.

:- public terms:subsumes_chk/2, terms:subsumes/2, terms:variant/2,
    terms:term_subsumer/3, terms:term_hash/2, terms:term_hash/4,
    terms:term_variables/2, terms:term_variables_bag/2, terms:acyclic_term/1,
    terms:cyclic_term/1.

:- public system:signal/2, system:handle_signal/1.

system:signal_handler(_,_).

:- public timeout:time_out_rec/3, timeout:time_out_rec1/3,
    timeout:has_not_expired/2, timeout:'$timer_now'/1.

timeout:time_out_rec(_,_,_,_) :- fail.

application_version.
after_timeout_options(_).
undo_timeout_options(_).
allow_sentence_key_for_parser(_).
call_build_lab(_,_,_).
call_clause(_,_).
call_default(_).
call_ignore_clause(_).
call_leaf(_,_).
catch_print_error(_,_,_).
change_tree(_,_).
clig_tree_user_node(_,_,_).
compile_test_suite(_).
create_object_hook(_,_).
display_extern_phon(_).
display_extern_sem(_).
dot_tree_user_node(_,_,_).
end_hook(_,_,_,_).
end_hook0(_,_,_,_).
exceptional_sentence_length(_,_).
exceptional_lf_length(_,_).
extern_phon(_,_).
extern_sem(_,_).
gram_startup_hook_begin.
gram_startup_hook_end.
hdrug_command(_,_,_).
hdrug_command_help(_,_,_).
hdrug_initialization.
latex_tree_user_node(_).
lf(_,_).
lf(_,_,_).
phonology(_,_).
pp_chart_show_node_help(_).
pp_chart_item(_).
pp_chart_item2(_).
pp_chart_item3(_).
pp_chart_item_b(_).
pp_chart_item_b2(_).
pp_chart_item_b3(_).
reconsult_test_suite(_).
result_hook(_,_,_,_).
semantics(_,_).
sentence(_,_).
sentence(_,_,_).
shorten_label(_,_).
show_node(_,_,_).
show_node2(_,_,_).
show_node3(_,_,_).
show_object_default2(_).
show_object_default3(_).
show_relation(_,_).
start_hook0(_,_,_,_).
start_hook(_,_,_,_).
tk_portray(_,_).
tk_tree_show_node_help(_,_).
tk_tree_user_node(_,_).
top(_,_).
use_canvas(_,_).
user_max(_,_).

hdrug_feature:attribute(_).
hdrug_feature:has_type(_,_,_).
hdrug_feature:e(_,_,_).
hdrug_feature:not_e(_,_).
hdrug_feature:btype(_,_,_,_).
hdrug_feature:xtype(_,_,_,_).
hdrug_feature:eval_atom(_,_).

:- public system:setenv/2, system:c_setenv/3, system:sp_win32_times/3,system:signal/2,
    system:handle_signal/1,system:sp_signal/3.

hdrug_gui:thread_exit(_).
hdrug_gui:thread_self(_).

:- public
    hdrug_util:flag/1,
    hdrug_util:flag/2,
    hdrug_util:flag/3.

:- public
    user:flag/1,
    user:flag/2,
    user:flag/3.

:- public tcltk:tcl_new/1, tcltk:tcl_delete/1, tcltk:tcl_eval/3, tcltk:tcl_event/3,
    tcltk:tk_new/2, tcltk:tk_main_window/2, tcltk:tk_destroy_window/1,
    tcltk:tk_make_window_exist/1, tcltk:tk_num_main_windows/1, tcltk:tk_do_one_event/0,
    tcltk:tk_do_one_event/1, tcltk:tk_next_event/2, tcltk:tk_next_event/3,
    tcltk:tk_main_loop/0, tcltk:tk_terminal/5, tcltk:call_from_tcl/1,
    tcltk:read_sc/2, tcltk:write_sc/2, tcltk:writeq_sc/2, tcltk:write_canonical_sc/2,
    tcltk:format_sc/3.

