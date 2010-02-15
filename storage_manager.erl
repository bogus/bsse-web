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
	list_users/0]).

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

list_users() ->
        gen_server:call(?MODULE, list_users).

init([]) ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	try
		mnesia:table_info(user, type)
	catch
		exit: _->
			mnesia:create_table(user,
					[{attributes, 
						record_info(fields, user)},
						{type, bag},
						{disc_copies, [node()]}])
	end,	
	{ok, #state{}}.

handle_call(list_users, _From, State) ->
	F = fun() ->
			Query = qlc:q([{object, <<"User">>,
							[{username, M#user.username}, 
							{name, M#user.realname}, 
							{email, M#user.email}, 
							{password, M#user.password}]} 
							|| M <- mnesia:table(user)]),
			qlc:e(Query)
		end,
	{atomic, Users} = mnesia:transaction(F),
	{reply, Users, State};

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

