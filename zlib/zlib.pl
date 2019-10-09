/*
 * A simple zlib module for Sicstus Prolog
 *
 * Copyright (c) 2009 Daniel de Kok
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 */

:- module(alpino_zlib,[ zlib_compress/2,
			zlib_compress/3,
			zlib_compress_term/3,
			zlib_compress_term/4,
			zlib_uncompress/3,
			zlib_uncompress_term/3 ]).

:- if(current_prolog_flag(dialect, swi)).
:- expects_dialect(sicstus).

foreign_resource(zlib,[ 'PL_record_external',
			'PL_recorded_external',
			'PL_erase_external',
			zlib_compress,
			zlib_compress_buf_list,
			zlib_compress_default,
			zlib_uncompress,
			zlib_uncompress_list_buf ]).

foreign('PL_record_external', c,
	'PL_record_external'(+term, -size_t, [-address(char)])).
foreign('PL_recorded_external', c,
	'PL_recorded_external'(+address('const char'), -term, [-int])).
foreign('PL_erase_external', c,
	'PL_erase_external'(+address(char), [-int])).

:- else.

:- use_module(library(fastrw),[ fast_buf_read/2,
				fast_buf_write/3 ]).
foreign_resource(zlib,[ zlib_compress,
			zlib_compress_buf_list,
			zlib_compress_default,
			zlib_free_buffer,
			zlib_uncompress,
			zlib_uncompress_list_buf ]).

foreign(zlib_free_buffer,c,zlib_free_buffer(+address)).

:- endif.


foreign(zlib_compress,c,zlib_compress(+term,+integer,[-term])).
foreign(zlib_compress_buf_list,c,zlib_compress_buf_list(+address(char),+integer,
						   +integer,[-term])).
foreign(zlib_compress_default,c,zlib_compress(+term,[-term])).
foreign(zlib_uncompress,c,zlib_uncompress(+term,+integer,[-term])).
foreign(zlib_uncompress_list_buf,c,zlib_uncompress_list_buf(+term,+integer,
							[-address])).

:- load_foreign_resource(zlib).

zlib_compress_term(Term,Compressed,Len) :-
    zlib_compress_term(Term,-1,Compressed,Len).

:- if(current_prolog_flag(dialect, swi)).

zlib_compress_term(Term,Level,Compressed,Len) :-
	'PL_record_external'(Term, Len, Addr),
	zlib_compress_buf_list(Addr,Len,Level,Compressed).

zlib_uncompress_term(Compressed,Len,Term) :-
	zlib_uncompress_list_buf(Compressed, Len, Addr),
	'PL_recorded_external'(Addr, Term0, 1),
	'PL_erase_external'(Addr, _),
	Term = Term0.

:- else.

copy_term(Term,Copy,Cons) :-
    call_residue(copy_term(Term,Copy),KeyCons),
    vals(KeyCons,Cons).

vals([],[]).
vals([_-H|T0],[H|T]) :-
    vals(T0,T).

zlib_compress_term(Term,Level,Compressed,Len) :-
    copy_term(Term,TermCopy,Cons),
    numbervars(TermCopy/Cons,0,_),
    fast_buf_write(TermCopy/Cons,Len,Addr),
    zlib_compress_buf_list(Addr,Len,Level,Compressed).

zlib_uncompress_term(Compressed,Len,Term) :-
    zlib_uncompress_list_buf(Compressed,Len,Addr),
    fast_buf_read(Term/Cons,Addr),
    zlib_free_buffer(Addr),
    call_constraints(Cons).

call_constraints([]).
call_constraints([Call|Rest]) :-
    call(Call),
    call_constraints(Rest).

:- endif.
