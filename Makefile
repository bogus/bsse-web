ERL=erl
ERLC=erlc
ERLCFLAGS+=-W +debug_info
ERLS=users.erl file_fingerprinting.erl ip_management.erl service_logs.erl login_logs.erl\
regex_category.erl node_management.erl general_option_management.erl storage_manager.erl\
log_search.erl live_log.erl dashboard_protocol_service.erl dashboard_category_service.erl\
dashboard_user_service.erl dashboard_system_service.erl
 
BEAMS=$(ERLS:.erl=.beam)
HTTPDIR=/opt/erlang/lib/erlang/lib/http

.PHONY: clean
.SUFFIXES: .beam .erl 

all: $(BEAMS)

.erl.beam:
	$(ERLC) $(ERLCFLAGS) $<

clean:
	rm -f $(BEAMS) 

run:
	$(ERL) -pa $(HTTPDIR)/ebin $(HTTPDIR)/deps/*/ebin -s http -config bsse 
