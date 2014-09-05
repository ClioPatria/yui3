:- module(yui3_beta,
	  [ yui3//3,
	    yui3_config//0,
	    yui3_combo//2,
	    yui3_select//1,
	    yui3_new//3,
	    yui3_set//3,
	    yui3_get//3,
	    yui3_add_class//2,
	    yui3_render//1,
	    yui3_render//2,
	    yui3_plug//3,
	    yui3_on//4,
	    yui3_delegate//6,
	    yui3_io//2,
	    yui3_load//2,
	    js_function//2
	  ]).

:- use_module(library(http/js_write)).
:- use_module(library(http/json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).

:- style_check(-atom).

:- meta_predicate
	yui3(+, +, :, -, +),
	yui3_select(+, -, +),
	yui3_new(+,+,+,-,+),
	yui3_set(+,+,+,-,+),
	yui3_get(+,+,-,-,+),
	yui3_add_class(+,+,-,+),
	yui3_render(+,-,+),
	yui3_render(+,+,-,+),
	yui3_on(+, +, +, :, -, +),
	yui3_delegate(+, +, :, +, +, +, -, +),
	yui3_io(+, +, -, +),
	js_function(+, :, -, +).



%%	yui3(+Head, +Include, +Body)
%
%	Emit javascript YUI3 object.

yui3(Head, Include, Body) -->
	html_requires(yui3('yui/yui-min.js')),
	html(\yui3_config),
	html('YUI('),
	js_args(Head),
	html(').use('),
	yui3_include(Include),
	js_function(['Y'], Body),
	html(');').

yui3_config --> { setting(yui3_conf:local, false) }, !.
yui3_config -->
	{
	 http_absolute_location(gallery(.), LocalGalleryPath, [])
	},
	html(['
	YUI_config = {
           groups: {
            // set up for locally served gallery
            gallery: {
                combine: false,
                base: "', LocalGalleryPath, '",
                patterns: {
                    "gallery-": {},
                    "gallerycss-": { type: "css" }
                }
            }
          }
	}
	']).

yui3_combo(Type, Resources) -->
	{
	 setting(yui3_conf:local, false),!,
	 create_combo_string(Type, Resources, ComboString)
	},
	html_requires(ComboString).

yui3_combo(_, []) --> !.
yui3_combo(Type, [H|T]) -->
	{
	 Spec =.. [Type,H],
	 http_absolute_location(Spec, R, [])
	},
	html_requires(R),
	yui3_combo(Type, T).

create_combo_string(yui3, L,S) :-
	setting(yui3_conf:version, Version),
	setting(yui3_conf:remote_path, Path),
	atomic_list_concat([Version, '/build/'], Loc),
	atomic_list_concat(['&', Loc], Sep),
	atomic_list_concat(L, Sep, S0),
	atomic_list_concat([Path, 'combo?', Loc, S0], S).

create_combo_string(gallery, L, S) :-
	setting(yui3_conf:remote_path, Path),
	atomic_list_concat(L, '&', S0),
	atomic_list_concat([Path, 'combo?', S0], S).


yui3_include([]) -->
	!.
yui3_include(List) -->
	js_args(List),
	html(', ').

		 /*******************************
		 *	    atomic YUI3 writes	*
		 *******************************/

%%	yui3_new(+Node, +Object, +Options)
%
%	Emit javascript object initializer

:- if(current_predicate(is_dict/1)).

yui3_new(Node, Object, Options) -->
	{ is_dict(Options),
	  atom_json_dict(Atom, Options, [as(atom)])
	},
	html(['var ', Node, ' = new ', Object, '(', Atom]),
	html(');\n').

:-endif.

yui3_new(Node, Object, Options) -->
	html(['var ', Node, ' = new ', Object, '(']),
	js_expression(Options),
	html(');\n').


%%	yui3_set(+Selector, +Attr, +Value)
%
%	Emit javascript attribute setter.

yui3_set(Selector, Attr, Value) -->
	yui3_select(Selector),
	html(['.set("',Attr,'",']),
	js_arg(Value),
	html(');\n').

%%	yui3_get(+Selector, +Attr, -Value)
%
%	Emit javascript attribute getter.

yui3_get(Selector, Attr, symbol(Value)) -->
	{ Value = tmp1 },
	html(['var ', Value, ' = ']),
	yui3_select(Selector),
	html(['.get("',Attr,'");\n']).

%%	yui3_add_class(+Selector, +ClassName)
%
%	Emit javascript className setter.

yui3_add_class(Selector, ClassName) -->
	yui3_select(Selector),
	html(['.addClass("',ClassName,'");\n']).

%%	yui3_render(+Node, +Selector)
%
%	Emit javascript render call.

yui3_render(Node) -->
	html([Node, '.render();\n']).
yui3_render(Node, Selector) -->
	html([Node, '.render(']),
	yui3_select(Selector),
	html(');\n').

%%	yui3_on(+Selecter, +Event, +Vars, :Body)
%
%	Emit YUI3 event handler

yui3_on(Selector, Event, Vars, Body) -->
	yui3_select(Selector),
	html(['.on("',Event,'",']),
	js_function(Vars, Body),
	html(');\n').

%%	yui3_delegate(+Event, +Vars, :Body, +Selector, +Context, +Args)
%
%	Emit javascript event handler on multiple nodes.

yui3_delegate(Event, Vars, Body, Selector, Context, Args) -->
	html(['Y.delegate', '("', Event, '",']),
	js_function(Vars, Body),
	html(', '),
	yui3_select(Selector),
	html([', "',Context,'",', Args, ');\n']).

%%	yui3_plug(+Node, +Plugin, +Conf)
%
%	Emit javascript plugin.

:- if(current_predicate(is_dict/1)).

yui3_plug(Selector, Plugin, Conf) -->
	{ is_dict(Conf),
	  atom_json_dict(Json, Conf, [])
	},
	yui3_select(Selector),
	html(['.plug(', Plugin, ',']),
	html(\[Json]),
	html(');\n').

:- endif.

yui3_plug(Selector, Plugin, Conf) -->
	yui3_select(Selector),
	html(['.plug(', Plugin, ',']),
	js_arg(Conf),
	html(');\n').

%%	yui3_io(+Server, +Conf)
%
%	Emit YUI io object.

yui3_io(Server, Conf) -->
	html(['Y.io("',Server,'", ', \js_args([Conf]), ');']).


%%	yui3_load(+Node, +URL)
%
%	Fetch remote html and set content to Node

yui3_load(Node, URL) -->
	yui3_select(Node),
	html(['.load(',\js_arg(URL),');']).


%%	yui3_select(+YUI3_Selector)
%
%	Emit YUI3 selector.

yui3_select(#(Id)) --> html(['"#',Id,'"']).
yui3_select(id(Id)) --> html(['"#',Id,'"']).
yui3_select(class(Id)) --> html(['".',Id,'"']).
yui3_select(one(Id)) --> html(['Y.one(',\yui3_select(Id),')']).
yui3_select(all(Id)) --> html(['Y.all(',\yui3_select(Id),')']).
yui3_select(Id) --> html(Id).


		 /*******************************
		 *	 atomic JS writes	*
		 *******************************/

%%	js_function(+Args, +Body)
%
%	Emit javascript function.

js_function(Args, Body) -->
	html(['function(', \js_vars(Args), ') {\n']),
	html(Body),
	html('}').

js_vars([]) -->
	[].
js_vars([H]) --> !,
	html(H).
js_vars([H|T]) -->
	html([H,',']),
	js_vars(T).
