-ifndef(_BSSE_RECORDS_HRL).
-define(_BSSE_RECORDS_HRL, true).

-include_lib("stdlib/include/qlc.hrl").

-record(user, {id, username, name, email, password}).
-record(file_fingerprint, {id, filename, hash, description, date}).
-record(regex, {id, name, regexval, description}).
-record(ip_data, {id, name, ipval, subnetval}).

-endif.
