-ifndef(_BSSE_RECORDS_HRL).
-define(_BSSE_RECORDS_HRL, true).

-include_lib("stdlib/include/qlc.hrl").

-define(RECINFO(RecAtom), (Record == RecAtom) or (is_record(Record, RecAtom)) -> record_info(fields, RecAtom)).

-record(unique_ids, {type, id}).

-record(user, {id, username, name, email, password}).
-record(file_fingerprint, {id, filename, hash, description, date}).
-record(regex, {id, name, regexval, description}).
-record(ip_data, {id, name, ipval, subnetval}).
-record(node_data, {id, name, status, load}).
-record(general_option, {id, dbbackupfreq, totalconn, checkupdate, updatecheckfreq, updateserver, sendfeedback, fbsenderemail, fbreceiveremail, enablelegallog, legallogusername, legallogpassword, enableremotelog, remotelogserver}).
-record(login_data, {date, username}).
-record(service_log, {date, log}).

-define(TABLES, [user,
		file_fingerprint,
		regex,
		ip_data,
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
		{service_log, bag}
	]).

get_record_fields(Record) ->
	if
		?RECINFO(unique_ids);
		?RECINFO(user);
		?RECINFO(regex);
		?RECINFO(ip_data);
		?RECINFO(file_fingerprint);
		?RECINFO(node_data);
		?RECINFO(general_option);
		?RECINFO(login_data);
		?RECINFO(service_log)
	end.

-endif.
