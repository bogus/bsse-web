-module(users).
-compile(export_all).
-include("bsse_records.hrl").

listObjects() ->
	storage_manager:list_objects(user).	

insertData(Object) ->
	erlang:display(Object).		

deleteData(Object) ->
	erlang:display(Object).		

updateData(Object) ->
	erlang:display(Object).

