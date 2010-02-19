-ifndef(_BSSE_RECORDS_HRL).
-define(_BSSE_RECORDS_HRL, true).

-include_lib("stdlib/include/qlc.hrl").

-record(user, {id, username, name, email, password}).
-record(file_fingerprint, {id, filename, hash, description, date}).
-record(regex, {id, name, regexval, description}).
-record(ip_data, {id, name, ipval, subnetval}).
-record(node_data, {id, name, status, load}).
-record(general_option, {id, dbbackupfreq, totalconn, checkupdate, updatecheckfreq, updateserver, sendfeedback, fbsenderemail, fbreceiveremail, enablelegallog, legallogusername, legallogpassword, enableremotelog, remotelogserver}).
-record(login_data, {date, username}).
-record(service_log, {date, log}).

-endif.
