-ifndef(_BSSE_RECORDS_HRL).
-define(_BSSE_RECORDS_HRL, true).

-include_lib("stdlib/include/qlc.hrl").

-define(RECINFO(RecAtom), (Record == RecAtom) or (is_record(Record, RecAtom)) -> record_info(fields, RecAtom)).

-record(unique_ids, {type, id}).

-record(user, {id, username, name, email, password}).
-record(file_fingerprint, {id, filename, hash, description, date}).
-record(regex, {id, name, regexval, description}).
-record(mime_type_group_data, {id, name, crosscount}).
-record(mime_type_data, {id, fileext, mimetype}).
-record(mime_type_cross_data, {id, catid, fileext, mimetype}).
-record(domain_group_data, {id, name}).
-record(domain_cross_data, {id, catid, domain}).
-record(ip_data, {id, name, ipval, subnetval}).
-record(time_data, {id, name, starttime, endtime, dayofweek}).
-record(node_data, {id, name, status, load}).
-record(general_option, {id, dbbackupfreq, totalconn, checkupdate, updatecheckfreq, updateserver, sendfeedback, fbsenderemail, fbreceiveremail, enablelegallog, legallogusername, legallogpassword, enableremotelog, remotelogserver}).
-record(login_data, {date, username}).
-record(service_log, {date, log}).
-record(log_search_criteria, {ip, ip_cat_enabled, ip_category, protocol, startdate, enddate}).
-record(dashboard_user_log, {user, count}).
-record(dashboard_category_log, {category, count}).
-record(dashboard_protocol_log, {protocol, count}).
-record(dashboard_system_data, {load, memory, connection}).

-define(TABLES, [user,
		file_fingerprint,
		regex,
		mime_type_data,
		mime_type_cross_data,
		mime_type_group_data,
		domain_group_data,
		domain_cross_data,
		ip_data,
		time_data,
		node_data,
		{general_option, ordered_set, fun() ->
				mnesia:write(
					{general_option, 0, 1, 100, false, 1,
					<<"update.medratech.com">>, false,
					<<"admin@corporation.com">>,
					<<"support@medratech.com">>, false,
					<<"username">>, <<"password">>, false,
					<<"remotelog.company.com">>}
				)
			end},
		{login_data, bag},
		{service_log, bag},
		{log_search_criteria, bag},
		{dashboard_user_log, bag},
		{dashboard_category_log, bag},
		{dashboard_protocol_log, bag},
		{dashboard_system_data, bag, fun() -> mnesia:write({dashboard_system_data, 72,51,9468}) end}
	]).

get_record_fields(Record) ->
	if
		?RECINFO(unique_ids);
		?RECINFO(user);
		?RECINFO(regex);
		?RECINFO(mime_type_data);
		?RECINFO(mime_type_cross_data);
		?RECINFO(mime_type_group_data);
		?RECINFO(domain_cross_data);
		?RECINFO(domain_group_data);
		?RECINFO(ip_data);
		?RECINFO(time_data);
		?RECINFO(file_fingerprint);
		?RECINFO(node_data);
		?RECINFO(general_option);
		?RECINFO(login_data);
		?RECINFO(service_log);
		?RECINFO(dashboard_user_log);
		?RECINFO(dashboard_category_log);
		?RECINFO(dashboard_protocol_log);
		?RECINFO(dashboard_system_data);
		?RECINFO(log_search_criteria)
	end.

-endif.
