:- module(conf_yui3, []).

/** <module> YAHOO User Interface library version 3
*/

:- use_module(library(settings)).
:- use_module(library(http/http_path)).

:- setting(local, boolean, false,
	   'When set to true the local version of YUI is used').

:- multifile http:location/3.

user:file_search_path(yui3js, web('yui/3.3.0')).

http:location(yui3,	    yui3_base(build),	       [js(true)]).
http:location(yui3_examples, yui3_base(examples),	       [js(true)]).

local_yui3 :-
	setting(local, true),
	absolute_file_name(yui3js(.), _, [ access(read),
					 file_type(directory),
					 file_errors(fail)
				       ]).

:- if(local_yui3).

http:location(yui3_base,    www('yui/3.3.0'),	       []).

:- use_module(library(http/http_dispatch)).

:- http_handler(yui3_base(.), serve_file, [prefix]).

serve_file(Request) :-
	memberchk(path_info(Path), Request),
	http_reply_file(yui3js(Path), [], Request).

:- else.

http:location(yui3_base, 'http://yui.yahooapis.com/3.3.0PR3/', []).

:- endif.
