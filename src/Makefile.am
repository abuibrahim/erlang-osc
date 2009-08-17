dist_erl_DATA = osc_app.erl osc_sup.erl osc_lib.erl osc_server.erl
erldir = $(ERLANG_INSTALL_LIB_DIR_osc)/src
beam_DATA = $(dist_erl_DATA:.erl=.beam) osc.app
beamdir = $(ERLANG_INSTALL_LIB_DIR_osc)/ebin
boot_DATA = osc.boot osc.script
bootdir = $(ERLANG_ROOT_DIR)/bin
html_DATA = $(dist_erl_DATA:.erl=.html) erlang.png edoc-info stylesheet.css \
	index.html overview-summary.html modules-frame.html packages-frame.html
htmldir = $(ERLANG_INSTALL_LIB_DIR_osc)/doc
dist_noinst_DATA = overview.edoc.in
CLEANFILES = $(beam_DATA) $(boot_DATA) $(html_DATA) osc.rel overview.edoc
ERLCFLAGS += -W

$(beam_DATA): $(dist_hrl_DATA)
$(boot_DATA): $(beam_DATA)

.erl.beam:
	@echo "Compiling $<..."
	@$(ERLC) $(ERLCFLAGS) $<

.rel.boot:
	@echo "Generating boot script..."
	@$(ERLC) -I. +no_module_tests $<

.rel.script:
	@echo "Generating boot scripts..."
	@$(ERLC) -I. +no_module_tests $<

$(html_DATA): $(dist_erl_DATA) overview.edoc 
	@echo "Generating documentation..."
	@$(ERL) -noshell -run edoc_run application osc '"."' '[{dir,"."}]'

check:
	@echo "Running unit tests..."
	@$(ERL) -eval "eunit:test({application,osc},[verbose])" \
		-noshell -s init stop
