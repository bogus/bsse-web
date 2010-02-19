-module(service_logs).
-compile(export_all).
-include("bsse_records.hrl").

getData() ->
	storage_manager:list_objects(service_log).	

insertData(Object) ->
	storage_manager:save_object(Object).		

deleteData(Object) ->
	storage_manager:delete_object(Object).		

updateData(Object) ->
	storage_manager:update_object(Object, service_log).

