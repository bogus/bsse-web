%%%-------------------------------------------------------------------
%%% @author Burak OGUZ <burak@medratech.com>
%%% @copyright 2010, Burak OGUZ
%%% @doc Storage Manager for BSSE Web Application.
%%% @end
%%%-------------------------------------------------------------------
-module(storage_manager).
-author("burak@medratech.com").
-behaviour(gen_server).

%% API
-export([start_link/0, 
	stop/0,
	list_objects/1,
	save_object/1,
	delete_object/1,
	update_object/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-include("bsse_records.hrl").

-record(state, {id=0}).


start_link() -> 
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:call({local,?MODULE}, stop).

list_objects(RecordAtom) ->
	gen_server:call(?MODULE, {list_objects, RecordAtom}).

save_object(Object) ->
	gen_server:call(?MODULE, {save_object, Object}).

delete_object(Object) ->
	gen_server:call(?MODULE, {delete_object, Object}).

update_object(Object) ->
	gen_server:call(?MODULE, {update_object, Object}).

init([]) ->
	mnesia:create_schema([node()]),
	mnesia:start(),

	start_table({unique_ids, set}),
	start_tables(?TABLES),

	{ok, #state{}}.

handle_call({list_objects, RecordAtom}, _From, State) ->
	F = fun() ->
			Query = qlc:q([record_to_object(M) || 
					M <- mnesia:table(RecordAtom)]),
			qlc:e(Query)
		end,
	{atomic, Objects} = transaction(F),
	{reply, Objects, State};

handle_call({save_object, undefined}, _From, State) ->
    {reply, undefined, State};

handle_call({save_object, Object}, _From, State) ->
	R = object_to_record(Object),
	F = fun() ->
			mnesia:write(R)
		end,
	transaction(F),
	{reply, Object, State};

handle_call({delete_object, Object}, _From, State) ->
	R = object_to_record(Object),
	F = fun() ->
			mnesia:delete_object(R)
		end,
	transaction(F),
	{reply, Object, State};

handle_call({update_object, Object}, _From, State) ->
	R = object_to_record(Object),
	F = fun() ->
			mnesia:write(R)
		end,
	transaction(F),
	{reply, Object, State};

handle_call(stop, _From, State) ->
	{stop, normalStop, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Msg, State) -> 
	{noreply, State}.

terminate(_Reason, _State) ->
    	ok.

code_change(_OldVsn, State, _Extra) ->
    	{ok, State}.

%%%%%%%% internal functions

record_atom_to_object_name(RecAtom) ->
	[FirstChar|RecStr] = atom_to_list(RecAtom),
	record_atom_to_object_name(RecStr, [string:to_upper(FirstChar)]).

record_atom_to_object_name([$_|RecStr], ObjStr) ->
	[Char|RecStr1] = RecStr,
	record_atom_to_object_name(RecStr1, [string:to_upper(Char)|ObjStr]);
	
record_atom_to_object_name([Char|RecStr], ObjStr) ->
	record_atom_to_object_name(RecStr, [Char|ObjStr]);

record_atom_to_object_name([], ObjStr) ->
	list_to_binary(lists:reverse(ObjStr)).

object_name_to_record_atom(ObjName) ->
	[FirstChar|ObjStr] = binary_to_list(ObjName),
	object_name_to_record_atom(ObjStr, [string:to_lower(FirstChar)]).

object_name_to_record_atom([Char|ObjStr], RecStr) ->
	RecStr1 = case string:to_upper(Char) of
			Char -> [string:to_lower(Char), $_|RecStr];
			_LowerChar -> [Char|RecStr]
		end,
	object_name_to_record_atom(ObjStr, RecStr1);

object_name_to_record_atom([], RecStr) ->
	list_to_atom(lists:reverse(RecStr)).

get_field_value_from_prop_dict(Field, [Pair|PropDict]) ->
	case Pair of
		{Field, Value} -> Value;
		_ -> get_field_value_from_prop_dict(Field, PropDict)
	end.

%% if record has a field named id and that's equals to zero,
%% than this value will be replaced with a newly generated value
object_to_record(Object) ->
	{object, ObjName, PropDict} = Object,
	RecAtom = object_name_to_record_atom(ObjName),
	object_to_record([RecAtom], get_record_fields(RecAtom), PropDict).

object_to_record(TupleList, [Field|FieldList], PropDict) ->
	Prop = get_field_value_from_prop_dict(Field, PropDict),
	case {Field, Prop} of
		{id, 0} -> 
			RecAtom = lists:last(TupleList),
			UniqueId = get_unique_id(RecAtom),
			object_to_record([UniqueId|TupleList], FieldList, PropDict);
		_ ->
			object_to_record([Prop|TupleList], FieldList, PropDict)
	end;

object_to_record(TupleList, [], _PropDict) ->
	list_to_tuple(lists:reverse(TupleList)).

record_to_object(Record) ->
	[RecAtom|AttrList] = tuple_to_list(Record),
	FieldList = get_record_fields(Record),
	record_to_object(RecAtom, FieldList, AttrList, []).

record_to_object(RecAtom, [Field|FieldList], [Attr|AttrList], AttrDict) ->
	record_to_object(RecAtom, FieldList, AttrList, [{Field, Attr}|AttrDict] );

record_to_object(RecAtom, [], [], AttrDict) ->
	{object, record_atom_to_object_name(RecAtom), lists:reverse(AttrDict)}.
	

start_table(RecordAtom) when is_atom(RecordAtom) ->
	start_table({RecordAtom, ordered_set});

start_table({RecordAtom, TableType}) ->
	start_table({RecordAtom, TableType, fun() -> ok end});

start_table({RecordAtom, TableType, InitFun}) ->
	try
		mnesia:table_info(RecordAtom, type)
	catch
		exit: _ ->
			mnesia:create_table(RecordAtom,
					[{attributes, 
						get_record_fields(RecordAtom) },
						{type, TableType},
						{disc_copies, [node()]}]),

			transaction(InitFun)
	end.

start_tables([RecordAtom|[]]) ->
	start_table(RecordAtom);
	
start_tables([RecordAtom|RAList]) ->
	start_table(RecordAtom),
	start_tables(RAList).

get_unique_id(TableName) ->
	mnesia:dirty_update_counter(unique_ids, TableName, 1).
	
transaction(F) ->
	try {atomic, mnesia:activity(transaction, F)}
	catch
		exit:Reason ->
			{aborted, Reason}
	end.
