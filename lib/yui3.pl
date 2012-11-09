:- module(yui3,
	  [ js_yui3//3,
	    js_yui3_event//5,
	    js_yui3_on//3,
	    js_yui3_delegate//5,
	    js_yui3_plug//3,
	    js_yui3_render//1,
	    js_yui3_render//2,
	    js_function//2,
	    js_function_decl//3,
	    js_global_function_decl//3,
	    js_yui3_decl//2,
	    js_yui3_select//1,
	    js_yui3_io//2
	  ]).

:- use_module(library(http/js_write)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- meta_predicate
	js_yui3(+, +, :, -, +),
	js_yui3_on(+, +, :, -, +),
	js_yui3_event(+, +, +, :, +, -, +),
	js_yui3_delegate(+, +, +, :, +, -, +),
        js_function(+, :, -, +),
	js_function_decl(+, +, :, -, +).

/* javascript support for YUI3 */

%%	js_yui3(+Head, +Include, +Body)
%
%	Emit javascript YUI3 object.

js_yui3(Head, Include, Body) -->
	html_requires(yui3('yui/yui-min.js')),
	html('var Y = YUI(\n'),
	js_args(Head),
	html(');\n'),
	html('Y.use('),
	js_yui3_include(Include),
	html('\n'),
	js_function(['Y'], Body),
	html(')').

js_yui3_include([]) -->
	!.
js_yui3_include(List) -->
	js_args(List),
	html(', ').


%%	js_yui3_event(+Id, +When, +EventType, +JSFunction, +Scope)
%
%	Emit javascript event handler.

js_yui3_event(Id, When, Event, Fn, Scope) -->
	html([\js_yui3_select(Id), '.' ,When, '("', Event, '",', Fn, ',', Scope, ');\n']).

%%	js_yui3_on(+Id, +EventType, +JSFunction)
%
%	Emit javascript event handler.
%	Same as js_yui3_event with When parameter = 'on'.

js_yui3_on(Id, Event, Fn) -->
	html([\js_yui3_select(Id),'.on("',Event,'",', Fn, ');\n']).

%	js_yui3_delegate(+Selecter, +Context, +EventType, +JSFunction,
%	+Args)
%
%	Emit javascript event handler.

js_yui3_delegate(Select, Context, Event, Fn, Args) -->
	html(['Y.delegate', '("', Event, '",', Fn, ',', \js_args([Select]),', "',Context,'",', Args, ');\n']).



%%	js_function(+Args, +Body)
%
%	Emit javascript function.

js_function(Args, Body) -->
	html(['function(', \js_vars(Args), ') {\n']),
	html([Body,'\n']),
	html('}').

js_vars([]) -->
	[].
js_vars([H]) --> !,
	html(H).
js_vars([H|T]) -->
	html([H,',']),
	js_vars(T).


%%	js_function_decl(+Id, +Args, +Body)
%
%	Emit javascript function declaration.

js_function_decl(Id, Args, Body) -->
	html(['var ', Id, ' = ']),
	js_function(Args, Body),
	html(';\n').
js_global_function_decl(Id, Args, Body) -->
	html([Id, ' = ']),
	js_function(Args, Body),
	html(';\n').

%%	js_yui3_decl(+Name, +Value)
%
%	Emit javascript variable declaration.

js_yui3_decl(Name, Value) -->
	html(['Y.', Name, ' = ']),
	js_arg(Value),
	html(';\n').

%%	js_yui3_plug(+Id, +Plugin, +Conf)
%
%	Emit javascript plugin.

js_yui3_plug(Id, Plugin, Conf) -->
	html([\js_yui3_select(Id), '.plug(', Plugin, ',']),
	js_args([Conf]),
	html(');\n').

%%	js_yui3_render(+Id, +El)
%
%	Emit javascript YUI3 render call.

js_yui3_render(Id) -->
	html([Id, '.render();\n']).
js_yui3_render(Id, Selector) -->
	html([Id, '.render(', \js_yui3_select(Selector), ');\n']).


%%	js_yui3_select(+YUI3_Selector)
%
%	Emit YUI3 selector.

js_yui3_select(Id) --> {atom(Id)}, !, html(Id).
js_yui3_select(#(Id)) --> !, html(['"#',Id,'"']).
js_yui3_select(class(Id)) --> !, html(['".',Id,'"']).
js_yui3_select(string(Id)) --> !, html(['"',Id,'"']).
js_yui3_select(one(Id)) --> !, html(['Y.one(',\js_yui3_select(Id),')']).
js_yui3_select(all(Id)) --> !, html(['Y.all(',\js_yui3_select(Id),')']).


%%	js_yui3_io(+Server, +Conf)
%
%	Emit YUI io object.

js_yui3_io(Server, Conf) -->
	html(['Y.io("',Server,'", ', \js_args([Conf]), ');']).
