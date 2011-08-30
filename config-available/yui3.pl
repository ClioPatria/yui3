:- module(yui3_conf, []).

/** <module> YAHOO User Interface library version 3.

Main functionality is to make it easy to:
* serve yui from a local git clone instead of using the YUI website
(handy when developing or demo-ing without an internet connection!)
* switch YUI versions.
*/

:- use_module(library(settings)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_dispatch)).

:- setting(local, boolean, false,
	   'When true the local version of YUI is used, when false (default) the normal YUI website is used.').
:- setting(local_path, atom, './yui3',
	   'Location of the local yui git (absolute dir name or relative to server directory)').
:- setting(remote_path, atom, 'http://yui.yahooapis.com/',
	   'Location of the local yui git (absolute dir name or relative to server directory)').
:- setting(version, atom, '3.4.0',
	   'remote YUI version to use (e.g. when local=false)').

:- multifile http:location/3.

user:file_search_path(yui3_base, LocalYui3) :- setting(local_path, LocalYui3).

:- http_handler(yui3_base(.), serve_local_yui3, [prefix]).

serve_local_yui3(Request) :-
	memberchk(path_info(Path), Request),
	http_reply_file(yui3_base(Path), [], Request).

http:location(yui3_base, root(local_yui3_version), []) :-
	setting(local, true),!.
http:location(yui3_base, Base, []) :-
	setting(version, Version),
	setting(remote_path, Path),
	format(atom(Base),'~w~w', [Path,Version]).

http:location(yui3,	     yui3_base(build),	       [js(true)]).
http:location(yui3_examples, yui3_base(examples),      [js(true)]).

:- listen(settings(changed(yui3_conf:_, _, _)),
	  http_path:clean_location_cache).


