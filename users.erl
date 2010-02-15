-module(users).
-compile(export_all).
-include("bsse_records.hrl").

getData() ->
	storage_manager:list_objects(user).	

insertData(Object) ->
	erlang:display(Object),
	storage_manager:save_object(Object).		

deleteData(Object) ->
	erlang:display(Object).		

updateData(Object) ->
	erlang:display(Object).

