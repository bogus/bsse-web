-module(node_management).
-compile(export_all).
-include("bsse_records.hrl").

getData() ->
	storage_manager:list_objects(node_data).	

insertData(Object) ->
	Object,
	ok.
	
deleteData(Object) ->
	Object,
	ok.

updateData(Object) ->
	storage_manager:update_object(Object).

