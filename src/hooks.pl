%% purpose of this file is twofold
%% -provide dummy definitions of hook predicates
%% -provide public declarations of unused (but useful) (library) predicates

:- public sockets:socket/2, sockets:socket_bind/2, sockets:socket_connect/3,
    sockets:socket_listen/2, sockets:socket_accept/2,
    sockets:socket_accept/3, sockets:socket_select/5, 
    sockets:socket_select/6, sockets:socket_close/1,
    sockets:socket_buffering/4, sockets:current_host/1, 
    sockets:hostname_address/2, sockets:socket_shutdown/2. 

:- public alpino_latin1:toupper/2, alpino_latin1:accent_chars/2.

:- public random:getrand/1, random:setrand/1, random:random/1, 
    random:random/3, random:randseq/3, random:randset/3.

:- public ordsets:is_ordset/1, ordsets:is_ordset/2, ordsets:ord_disjoint/2,
    ordsets:ord_intersect/2, ordsets:ord_intersect/5,
    ordsets:ord_intersection/4, ordsets:ord_intersection4/7, ordsets:ord_member/3,
    ordsets:ord_intersection4/5, ordsets:ord_setproduct/3, ordsets:ord_member/2,
    ordsets:ord_setproduct/4, ordsets:ord_subset/2, ordsets:ord_subset/4,
    ordsets:ord_union/4, ordsets:ord_union4/7, ordsets:ord_union4/5,
    ordsets:ord_symdiff/3, ordsets:ord_symdiff/4, ordsets:ord_symdiff/6.

/*
:- public fastrw:fast_write/1, fastrw:fast_write/2,
    fastrw:fast_read/1, fastrw:fast_read/2, fastrw:init_fastrw/1,
    fastrw:deinit_fastrw/1.
*/

/*
:- public bdb:db_open_env/2, bdb:db_open_env/3, bdb:db_close_env/1, bdb:db_current_env/2,
    bdb:db_open/4, bdb:db_open/5, bdb:db_close/1, bdb:db_current/5, bdb:db_store/3, bdb:db_fetch/3, bdb:db_erase/2, bdb:db_erase/3,
    bdb:db_enumerate/3, bdb:db_findall/5, bdb:db_compress/2, bdb:db_compress/3, bdb:db_sync/1,
    bdb:db_make_iterator/2, bdb:db_make_iterator/3, bdb:db_iterator_next/3, bdb:db_iterator_done/1,
    bdb:db_current_iterator/3, bdb:db_export/2, bdb:db_export/3, bdb:db_import/2, bdb:db_import/3,
    bdb:c_get_fastrw_compatibility/1, bdb:c_set_fastrw_compatibility/1.
*/

/*
:- public alpino_zlib:zlib_compress/2, alpino_zlib:zlib_compress/3,
    alpino_zlib:zlib_uncompress/3.
*/

:- public alpino_unix:wait/2.


%% pre-compiled grammar
alpino_grammar:grammar_rule(_,_,_).
alpino_grammar:compile_predicate(_).
alpino_grammar:top_category(_).
alpino_grammar:robust_top_category(_).
alpino_grammar:no_memo(_).
alpino_grammar:weaken(_,_).
alpino_grammar:user_clause(_,_).
alpino_grammar:weaken_link_cat(_,_).

alpino_lex_types:r_ld_prep(_).
alpino_lex_types:no_r_ld_prep(_).
alpino_lex_types:lex(_,_,_,_).
alpino_lex_types:user_clause(_,_).

alpino_lc_in:parse_top_(_,_,_,_,_,_).
alpino_lc_in:parse_robust_top_(_,_,_,_,_,_).
alpino_lc_in:parse0_(_,_,_,_,_,_,_,_).
alpino_lc_in:top_category_(_).
alpino_lc_in:top_category_(_,_,_,_).
alpino_lc_in:robust_top_category_(_).
alpino_lc_in:robust_top_category_(_,_,_,_).
alpino_lc_in:gap_i_(_,_,_).
alpino_lc_in:gap_(_,_,_).
alpino_lc_in:lc_rule_i_(_,_,_,_,_).
alpino_lc_in:grammar_rule(_,_,_).
alpino_lc_in:grammar_rule_g(_,_,_).
alpino_lc_in:unknown_predicate_handler(_,_).
alpino_lc_in:generalized_pair(_,_).
alpino_lc_in:weaken_link_cat(_,_).
alpino_lc_in:parse_(_,_,_,_,_,_,_).

alpino_genrules:gap(_,_).
alpino_genrules:headed_grammar_rule(_,_,_,_).
alpino_genrules:grammar_rule_unpack(_,_,_,_).

alpino_disambiguation_weights:feature_weight(_,_).

alpino_penalties:syntactic_penalty(_,_,_,_).
% alpino_penalties:additional_weight(_,_).

result_to_gerlof(_,_,_).

alpino_startup_hook_end.

vraag_joost(_).

alpino_start_hook(_,_).
alpino_result_hook(_,_,_,_).
alpino_end_hook(_,_,_,_).

alpino_suite:sentence(_,_).
alpino_suite:my_sentence(_,_).

alpino_guides:check_predict(_,_).
alpino_guides:check_connect(_,_).

alpino_fluency_weights:feature_weight(_,_).

alpino_ngram_lm:fluency_model_initialize.

alpino_data:dt_prs(_,_).
alpino_data:precedes_embedded_subject_cat(_,_).
alpino_data:adjective_er_plural(_,_,_).
alpino_data:wh_relagr(_,_).
alpino_data:syntactic_penalty_cat_d(_,_).
alpino_data:tags(_36376,_36377).
alpino_data:sv1_mextra(_36376,_36377).
alpino_data:comparative_cat(_36376).
alpino_data:lexical(_36376,_36377,_36378,_36379,_36380,_36381,_36382,_).
alpino_data:dt_out(_36376,_36377).
alpino_data:vp_cat(_36376,_36377).
alpino_data:mexs(_36376,_36377).
alpino_data:clist(_36376,_36377,_36378).
alpino_data:dt_svp(_36376,_36377).
alpino_data:percolation_features(_36376,_36377,_36378,_36379,_36380,_36381,_36382,_36383,_36384).
alpino_data:dt_if_defined(_36376,_36377).
alpino_data:dt_cnj_crd(_36376,_36377,_36378).
alpino_data:context_embed_node(_36376).
alpino_data:robust_list_to_cat(_36376,_36377).
alpino_data:modifier(_36376,_36377).
alpino_data:sv1_extra(_36376,_36377).
alpino_data:mexs_cat_mods(_36376,_36377).
alpino_data:predicative(_36376,_36377).
alpino_data:postag(_36376,_36377).
alpino_data:label(_36376,_36377,_36378,_36379,_,_).
alpino_data:dt_features(_36376,_36377).
alpino_data:lix(_36376,_36377).
alpino_data:syntactic_penalty_cat(_36376,_36377).
alpino_data:deriv_tree_struct(_36376,_36377,_36378,_36379).
alpino_data:dt_daughters(_36376,_36377,_36378).
alpino_data:cat_to_result(_36376,_36377).
alpino_data:dt(_36376,_36377).
alpino_data:lexical_node(_36376,_36377,_36378,_36379,_36380).
alpino_data:punct(_36376).
alpino_data:case(_36376,_36377).
alpino_data:result(_36376,_36377,_36378).
alpino_data:top_cat(_36376).
alpino_data:dt_shared_head_parts(_36376,_36377).
alpino_data:inout(_36376).
alpino_data:slash(_36376,_36377).
alpino_data:precedes_subject_cat(_36376,_36377).
alpino_data:dt_hwrd_positions(_36376,_36377,_36378).
alpino_data:dt_fwrd_positions(_36376,_36377,_36378).
alpino_data:dt(_36376,_36377,_36378,_36379,_36380).
alpino_data:dt(_36376,_36377,_36378,_36379,_36380,_,_).
alpino_data:context_swap_node(_36376).
alpino_data:result_term(_36376,_36377,_36378,_36379,_36380,_36381).
alpino_data:result_term(_,_,_,_,_,_,_).
alpino_data:result_cdt(_,_).
alpino_data:det_agr(_,_).
alpino_data:n_agr(_,_).
alpino_data:puncttype(_,_).
alpino_data:vraag_puncttype(_).
alpino_data:slashed_prep(_).
alpino_data:nominative(_).
alpino_data:plural(_).
alpino_data:agr(_,_).
alpino_data:aform(_,_).
alpino_data:de(_).
alpino_data:het(_).
alpino_data:pl(_).
alpino_data:sg(_).
alpino_data:not_attr(_).
alpino_data:dt_num(_,_).
alpino_data:not_ynquestion(_).
alpino_data:not_whquestion(_).
alpino_data:not_declarative(_).
alpino_data:not_imparative(_).
alpino_data:not_topic_drop(_).
alpino_data:ynquestion(_).
alpino_data:whquestion(_).
alpino_data:declarative(_).
alpino_data:imparative(_).
alpino_data:topic_drop(_).
alpino_data:conj(_,_,_).
alpino_data:hstem(_,_).
alpino_data:prep(_,_).
alpino_data:max_dt(_).
alpino_data:np_agr(_,_).
alpino_data:subj_agr(_,_).
alpino_data:vproj_without_eps3(_).
alpino_data:ld_pp(_).
alpino_data:def(_).
alpino_data:indef(_).
alpino_data:vp(_).
alpino_data:not_dropped_fir(_).
alpino_data:not_dropped_thi(_).
alpino_data:not_dropped_sg(_).
alpino_data:not_dropped_pl(_).
alpino_data:dropped_fir(_).
alpino_data:dropped_thi(_).
alpino_data:dropped_sg(_).
alpino_data:dropped_pl(_).
alpino_data:dt_with_hwrd(_,_,_,_,_).
alpino_data:hwrd_with_pos(_,_).


alpino_lex:inv_spelling_variant(_,_).
alpino_lex:inv_spelling_variant21(_,_,_).
alpino_lex:inv_spelling_variant31(_,_,_,_).
alpino_lex:inv_abbreviation(_,_).

format_facts(_).

alpino_lex:noclp_assertz(_).
alpino_lexical_analysis:noclp_assertz(_).
alpino_lc:noclp_assertz(_).
alpino_unknowns:noclp_assertz(_).
alpino_util_tmp:noclp_assertz(_).
alpino_lc_in:noclp_assertz(_).
alpino_table_item:noclp_assertz(_).
alpino_paraphrase:noclp_assertz(_).

apply_adt_transformations(_,_).

alpino_user_transformation:user_transformation(_,_,_,_,_,_,_,_).

alpino_gen_suite:lf(_,_).

alpino_data:separate(_,_).

lu_form(_,_,_).

