dist_erl_DATA = osc_app.erl osc_sup.erl osc_lib.erl osc_server.erl
erldir = $(ERLANG_INSTALL_LIB_DIR_osc)/src
dist_hrl_DATA = osc.hrl
hrldir = $(ERLANG_INSTALL_LIB_DIR_osc)/include
beam_DATA = $(dist_erl_DATA:.erl=.beam) osc.app
beamdir = $(ERLANG_INSTALL_LIB_DIR_osc)/ebin
html_DATA = $(dist_erl_DATA:.erl=.html) erlang.png edoc-info stylesheet.css \
	index.html overview-summary.html modules-frame.html packages-frame.html
htmldir = $(ERLANG_INSTALL_LIB_DIR_osc)/doc
dist_noinst_DATA = overview.edoc.in
CLEANFILES = $(beam_DATA) $(html_DATA) overview.edoc
ERLCFLAGS += -W

$(beam_DATA): $(dist_hrl_DATA)

.erl.beam:
	@echo "Compiling $<..."
	@$(ERLC) $(ERLCFLAGS) $<

$(html_DATA): $(dist_erl_DATA) overview.edoc 
	@echo "Generating documentation..."
	@$(ERL) -noshell -run edoc_run application osc '"."' '[{dir,"."}]'

check:
	@echo "Running unit tests..."
	@$(ERL) -eval "eunit:test({application,osc},[verbose])" \
		-noshell -s init stop
