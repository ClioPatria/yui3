:- module(yui3_conf, []).

/** <module> YAHOO User Interface library version 3
*/


:- use_module(library(settings)).
:- use_module(library(http/http_path)).

:- setting(local, boolean, true,
	   'When set to true the local version of YUI is used').
:- setting(version, atom, '3.4.0', 'YUI version to use').
:- setting(combo, atom, 'http://yui.yahooapis.com/combo', 'URI of yui combo service').

:- multifile http:location/3.

user:file_search_path(yui3js, web('yui/x.x.0')).

http:location(yui3,	     yui3_base(build),	       [js(true)]).
http:location(yui3_examples, yui3_base(examples),      [js(true)]).

local_yui3 :-
	setting(local, true),
	absolute_file_name(yui3js(.), _, [ access(read),
					 file_type(directory),
					 file_errors(fail)
				       ]).

:- if(local_yui3).

http:location(yui3_base,    www('yui/x.y.0'),	       []).

:- use_module(library(http/http_dispatch)).

:- http_handler(yui3_base(.), serve_file, [prefix]).

serve_file(Request) :-
	memberchk(path_info(Path), Request),
	http_reply_file(yui3js(Path), [], Request).

:- else.

http:location(yui3_base, Base, []) :-
	setting(version, Version),
	format(atom(Base),'http://yui.yahooapis.com/~w', [Version]).

:- endif.
