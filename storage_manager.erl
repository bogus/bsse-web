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
	update_object/2]).

-compile(export_all).

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

update_object(Object, RecordAtom) ->
        gen_server:call(?MODULE, {update_object, Object, RecordAtom}).

init([]) ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	try
		mnesia:table_info(user, type),
		mnesia:table_info(file_fingerprint, type),
		mnesia:table_info(regex, type),
		mnesia:table_info(ip_data, type),
		mnesia:table_info(node_data, type)
	catch
		exit: _->
			mnesia:create_table(user,
					[{attributes, 
						record_info(fields, user)},
						{type, set},
						{disc_copies, [node()]}]),
			mnesia:create_table(file_fingerprint,
                                        [{attributes,
                                                record_info(fields, file_fingerprint)},
                                                {type, bag},
                                                {disc_copies, [node()]}]),
			mnesia:create_table(regex,
                                        [{attributes,
                                                record_info(fields, regex)},
                                                {type, bag},
                                                {disc_copies, [node()]}]),
			mnesia:create_table(ip_data,
                                        [{attributes,
                                                record_info(fields, ip_data)},
                                                {type, bag},
                                                {disc_copies, [node()]}]),
			mnesia:create_table(node_data,
                                        [{attributes,
                                                record_info(fields, node_data)},
                                                {type, bag},
                                                {disc_copies, [node()]}]),
			mnesia:create_table(general_option,
                                        [{attributes,
                                                record_info(fields, general_option)},
                                                {type, bag},
                                                {disc_copies, [node()]}]),
			mnesia:transaction(fun()->mnesia:write({general_option,0,1,100,false,1,<<"update.medratech.com">>,false,<<"admin@corporation.com">>,<<"support@medratech.com">>, false, <<"username">>,<<"password">>,false,<<"remotelog.company.com">>}) end),
			mnesia:create_table(login_data,
                                        [{attributes,
                                                record_info(fields, login_data)},
                                                {type, bag},
                                                {disc_copies, [node()]}]),
			mnesia:create_table(service_log,
                                        [{attributes,
                                                record_info(fields, service_log)},
                                                {type, bag},
                                                {disc_copies, [node()]}])
	end,	
	{ok, #state{}}.

handle_call({list_objects, RecordAtom}, _From, State) ->
	F = fun() ->
			Query = qlc:q([record_to_object(M) || M <- mnesia:table(RecordAtom)]),
			qlc:e(Query)
		end,
	{atomic, Objects} = mnesia:transaction(F),
	erlang:display(Objects),
	{reply, Objects, State};

handle_call({save_object, undefined}, _From, State) ->
    {reply, undefined, State};

handle_call({save_object, Object}, _From, State) ->
	R = object_to_record(Object),
        F = fun() ->
			mnesia:write(R)
                end,
	mnesia:transaction(F),
	{reply, Object, State};

handle_call({delete_object, Object}, _From, State) ->
        R = object_to_record(Object),
        F = fun() ->
                        mnesia:delete_object(R)
                end,
        mnesia:transaction(F),
        {reply, Object, State};

handle_call({update_object, Object, RecordAtom}, _From, State) ->
        R = object_to_record(Object),
        F = fun() ->
			erlang:display(R)
                end,
        mnesia:transaction(F),
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

get_record_fields(Record) ->
	erlang:display(Record),
	if
		(Record == user) or (is_record(Record, user)) -> 
			record_info(fields, user);
		(Record == regex) or (is_record(Record, regex)) -> 
			record_info(fields, regex);
		(Record == ip_data) or (is_record(Record, ip_data)) -> 
			record_info(fields, ip_data);
		(Record == file_fingerprint) or (is_record(Record, file_fingerprint)) ->
                        record_info(fields, file_fingerprint);
		(Record == node_data) or (is_record(Record, node_data)) ->
                        record_info(fields, node_data);
		(Record == general_option) or (is_record(Record, general_option)) ->
                        record_info(fields, general_option);
		(Record == login_data) or (is_record(Record, login_data)) ->
                        record_info(fields, login_data);
		(Record == service_log) or (is_record(Record, service_log)) ->
                        record_info(fields, service_log)
	end.

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

object_to_record(Object) ->
	{object, ObjName, PropDict} = Object,
	RecAtom = object_name_to_record_atom(ObjName),
	object_to_record([RecAtom], get_record_fields(RecAtom), PropDict).

object_to_record(TupleList, [Field|FieldList], PropDict) ->
	Prop = get_field_value_from_prop_dict(Field, PropDict),
	object_to_record([Prop|TupleList], FieldList, PropDict);

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
	
	
