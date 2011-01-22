:- module(yui3,
	  [ js_yui3//3,
	    js_yui3_event//5,
	    js_yui3_on//3,
	    js_function//2,
	    js_function_decl//3
	  ]).

:- use_module(library(http/js_write)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- meta_predicate
	js_yui3(+, +, :, -, +),
	js_yui3_on(+, +, :, -, +),
        js_function(+, :, -, +),
	js_function_decl(+, +, :, -, +).

/* additional javascript support */

%	js_yui3_event(+Id, +When, +EventType, +JSFunction, +Scope)
%
%	Emit javascript event handler.

js_yui3_event(Id, When, Event, Fn, Scope) -->
	html([Id, '.' ,When, '("', Event, '",', Fn, ',', Scope, ');\n']).

%	js_yui3_on(+Id, +EventType, +JSFunction)
%
%	Emit javascript event handler.
%	Same as js_yui3_event with When parameter = 'on'.

js_yui3_on(Id, Event, Fn) -->
	html([Id,'.on("',Event,'",', Fn, ');\n']).


%%	js_yui3(+Head, +Include, +Body)
%
%	Emit javascript YUI3 object.

js_yui3(Head, Include, Body) -->
	html_requires(yui3('yui/yui-min.js')),
 	html(['YUI(',
	      \js_args(Head),
	      ').use(',
	      \js_yui3_include(Include),
	      \js_function(['Y'], Body),
	      ');\n'
	     ]).

js_yui3_include([]) -->
	!.
js_yui3_include(List) -->
	js_args(List),
	html(', ').

%%	js_function(+Args, +Body)
%
%	Emit javascript function.

js_function(Args, Body) -->
 	html(['function(', \js_vars(Args), ') {\n']),
	html(Body),
	html('}').

%%	js_function_decl(+Id, +Args, +Body)
%
%	Emit javascript function declaration.

js_function_decl(Id, Args, Body) -->
	html(['var ', Id, ' = ']),
	js_function(Args, Body),
	html(';\n').

js_vars([]) -->
	[].
js_vars([H]) --> !,
	html(H).
js_vars([H|T]) -->
	html([H,',']),
	js_vars(T).
