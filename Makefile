
##----------------------------------------------------------------------
## DISCLAIMER
##
## This file contains the rules to make an ocsigen package. The project
## is configured through the variables in the file Makefile.options.
##----------------------------------------------------------------------

include Makefile.options

##----------------------------------------------------------------------
##			      Internals

## Required binaries
ELIOMC            := eliomc -ppx
ELIOMOPT          := eliomopt -ppx
JS_OF_ELIOM       := js_of_eliom -ppx
ELIOMDEP          := eliomdep
OCAMLFIND         := ocamlfind

## Where to put intermediate object files.
## - ELIOM_{SERVER,CLIENT}_DIR must be distinct
## - ELIOM_CLIENT_DIR must not be the local dir.
## - ELIOM_SERVER_DIR could be ".", but you need to
##   remove it from the "clean" rules...
export ELIOM_SERVER_DIR := _server
export ELIOM_CLIENT_DIR := _client
export ELIOM_TYPE_DIR   := _server

ifeq ($(DEBUG),yes)
  GENERATE_DEBUG ?= -g
endif

ifeq ($(NATIVE),yes)
  OPT_RULE = opt
endif

##----------------------------------------------------------------------
## General

.PHONY: all byte opt doc
all: byte $(OPT_RULE)
byte:: $(LIBDIR)/${PKG_NAME}.server.cma $(LIBDIR)/${PKG_NAME}.client.cma
opt:: $(LIBDIR)/${PKG_NAME}.server.cmxs

##----------------------------------------------------------------------
## Aux

# Use `eliomdep -sort' only in OCaml>4
ifeq ($(shell ocamlc -version|cut -c1),4)
eliomdep=$(shell $(ELIOMDEP) $(1) -ppx -sort $(2) $(filter %.eliom %.ml,$(3))))
else
eliomdep=$(3)
endif
objs=$(patsubst %.ml,$(1)/%.$(2),$(patsubst %.eliom,$(1)/%.$(2),$(filter %.eliom %.ml,$(3))))
depsort=$(call objs,$(1),$(2),$(call eliomdep,$(3),$(4),$(5)))

$(LIBDIR):
	mkdir $(LIBDIR)

##----------------------------------------------------------------------
## Server side compilation

## make it more elegant ?
SERVER_DIRS     := $(shell echo $(foreach f, $(SERVER_FILES), $(dir $(f))) |  tr ' ' '\n' | sort -u | tr '\n' ' ')
SERVER_DEP_DIRS := ${addprefix -eliom-inc ,${SERVER_DIRS}}
SERVER_INC_DIRS := ${addprefix -I $(ELIOM_SERVER_DIR)/, ${SERVER_DIRS}}

SERVER_INC  := \
	${addprefix -package ,${SERVER_PACKAGES} ${SERVER_SYNTAX_PACKAGES}}

${ELIOM_TYPE_DIR}/%.type_mli: %.eliom
	${ELIOMC} -infer ${SERVER_INC} ${SERVER_INC_DIRS} $<

$(LIBDIR)/$(PKG_NAME).server.cma: $(call objs,$(ELIOM_SERVER_DIR),cmo,$(SERVER_FILES)) | $(LIBDIR)
	${ELIOMC} -a -o $@ $(GENERATE_DEBUG) \
          $(call depsort,$(ELIOM_SERVER_DIR),cmo,-server,$(SERVER_INC),$(SERVER_FILES))

$(LIBDIR)/$(PKG_NAME).server.cmxa: $(call objs,$(ELIOM_SERVER_DIR),cmx,$(SERVER_FILES)) | $(LIBDIR)
	${ELIOMOPT} -a -o $@ $(GENERATE_DEBUG) \
          $(call depsort,$(ELIOM_SERVER_DIR),cmx,-server,$(SERVER_INC),$(SERVER_FILES))

%.cmxs: %.cmxa
	$(ELIOMOPT) -shared -linkall -o $@ $(GENERATE_DEBUG) $<

${ELIOM_SERVER_DIR}/%.cmi: %.mli
	${ELIOMC} -c ${SERVER_INC} ${SERVER_INC_DIRS} $(GENERATE_DEBUG) $<

${ELIOM_SERVER_DIR}/%.cmi: %.eliomi
	${ELIOMC} -c ${SERVER_INC} ${SERVER_INC_DIRS} $(GENERATE_DEBUG) $<

${ELIOM_SERVER_DIR}/%.cmo: %.ml
	${ELIOMC} -c ${SERVER_INC} ${SERVER_INC_DIRS} $(GENERATE_DEBUG) $<
${ELIOM_SERVER_DIR}/%.cmo: %.eliom
	${ELIOMC} -c ${SERVER_INC} ${SERVER_INC_DIRS} $(GENERATE_DEBUG) $<

${ELIOM_SERVER_DIR}/%.cmx: %.ml
	${ELIOMOPT} -c ${SERVER_INC} ${SERVER_INC_DIRS} $(GENERATE_DEBUG) $<
${ELIOM_SERVER_DIR}/%.cmx: %.eliom
	${ELIOMOPT} -c ${SERVER_INC} ${SERVER_INC_DIRS} $(GENERATE_DEBUG) $<


##----------------------------------------------------------------------
## Client side compilation

## make it more elegant ?
CLIENT_DIRS     := $(shell echo $(foreach f, $(CLIENT_FILES), $(dir $(f))) |  tr ' ' '\n' | sort -u | tr '\n' ' ')
CLIENT_DEP_DIRS := ${addprefix -eliom-inc ,${CLIENT_DIRS}}
CLIENT_INC_DIRS := ${addprefix -I $(ELIOM_CLIENT_DIR)/,${CLIENT_DIRS}}

CLIENT_INC  := \
	${addprefix -package ,${CLIENT_PACKAGES} ${CLIENT_SYNTAX_PACKAGES}}

CLIENT_OBJS := $(filter %.eliom %.ml, $(CLIENT_FILES))
CLIENT_OBJS := $(patsubst %.eliom,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_OBJS})
CLIENT_OBJS := $(patsubst %.ml,${ELIOM_CLIENT_DIR}/%.cmo, ${CLIENT_OBJS})

$(LIBDIR)/$(PKG_NAME).client.cma: $(call objs,$(ELIOM_CLIENT_DIR),cmo,$(CLIENT_FILES)) | $(LIBDIR)
	${JS_OF_ELIOM} -a -o $@ $(GENERATE_DEBUG) \
          $(call depsort,$(ELIOM_CLIENT_DIR),cmo,-client,$(CLIENT_INC),$(CLIENT_FILES))

${ELIOM_CLIENT_DIR}/%.cmi: %.mli
	${JS_OF_ELIOM} -c ${CLIENT_INC} ${CLIENT_INC_DIRS} $(GENERATE_DEBUG) $<

${ELIOM_CLIENT_DIR}/%.cmo: %.eliom
	${JS_OF_ELIOM} -c ${CLIENT_INC} ${CLIENT_INC_DIRS} $(GENERATE_DEBUG) $<
${ELIOM_CLIENT_DIR}/%.cmo: %.ml
	${JS_OF_ELIOM} -c ${CLIENT_INC} ${CLIENT_INC_DIRS} $(GENERATE_DEBUG) $<

${ELIOM_CLIENT_DIR}/%.cmi: %.eliomi
	${JS_OF_ELIOM} -c ${CLIENT_INC} ${CLIENT_INC_DIRS} $(GENERATE_DEBUG) $<

##----------------------------------------------------------------------
## Installation

CLIENT_CMO=$(wildcard $(addsuffix /$(MODULE_PREFIX)*.cmo,$(addprefix $(ELIOM_CLIENT_DIR)/,$(CLIENT_DIRS))))
CLIENT_CMI=$(wildcard $(addsuffix /$(MODULE_PREFIX)*.cmi,$(addprefix $(ELIOM_CLIENT_DIR)/,$(CLIENT_DIRS))))
SERVER_CMI=$(wildcard $(addsuffix /$(MODULE_PREFIX)*.cmi,$(addprefix $(ELIOM_SERVER_DIR)/,$(SERVER_DIRS))))
SERVER_CMX=$(wildcard $(addsuffix /$(MODULE_PREFIX)*.cmx,$(addprefix $(ELIOM_SERVER_DIR)/,$(SERVER_DIRS))))

basename_for_each = $(shell echo $(foreach f,$(1),$(shell basename $(f))))
CLIENT_CMO_META=$(call basename_for_each, $(call depsort,$(ELIOM_CLIENT_DIR),cmo,-client,$(CLIENT_INC),$(CLIENT_FILES)))

META: META.in
	sed -e 's#@@PKG_NAME@@#$(PKG_NAME)#g' \
		-e 's#@@PKG_VERS@@#$(PKG_VERS)#g' \
		-e 's#@@PKG_DESC@@#$(PKG_DESC)#g' \
		-e 's#@@CLIENT_REQUIRES@@#$(CLIENT_PACKAGES)#g' \
		-e 's#@@CLIENT_ARCHIVES_BYTE@@#$(CLIENT_CMO_META)#g' \
		-e 's#@@SERVER_REQUIRES@@#$(SERVER_PACKAGES)#g' \
		-e 's#@@SERVER_ARCHIVES_BYTE@@#$(PKG_NAME).server.cma#g' \
		-e 's#@@SERVER_ARCHIVES_NATIVE@@#$(PKG_NAME).server.cmxa#g' \
		-e 's#@@SERVER_ARCHIVES_NATIVE_PLUGIN@@#$(PKG_NAME).server.cmxs#g' \
		$< > $@

install: all META
	$(OCAMLFIND) install $(PKG_NAME) META
	mkdir -p `$(OCAMLFIND) query $(PKG_NAME)`/client
	mkdir -p `$(OCAMLFIND) query $(PKG_NAME)`/server
	cp $(CLIENT_CMI) `$(OCAMLFIND) query $(PKG_NAME)`/client
	cp $(CLIENT_CMO) `$(OCAMLFIND) query $(PKG_NAME)`/client
	cp $(SERVER_CMI) `$(OCAMLFIND) query $(PKG_NAME)`/server
	cp $(SERVER_CMX) `$(OCAMLFIND) query $(PKG_NAME)`/server
	cp $(LIBDIR)/$(PKG_NAME).server.cm* `$(OCAMLFIND) query $(PKG_NAME)`/server

uninstall:
	rm -rf `$(OCAMLFIND) query $(PKG_NAME)`/client
	rm -rf `$(OCAMLFIND) query $(PKG_NAME)`/server
	$(OCAMLFIND) remove $(PKG_NAME)

reinstall: uninstall install

##----------------------------------------------------------------------
## Dependencies

ifneq ($(MAKECMDGOALS),distclean)
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),depend)
    include .depend
endif
endif
endif

DEPSDIR := _deps

.depend: $(patsubst %,$(DEPSDIR)/%.server,$(SERVER_FILES)) $(patsubst %,$(DEPSDIR)/%.client,$(CLIENT_FILES))
	cat $^ > $@

$(DEPSDIR)/%.server: % | $(DEPSDIR)
	$(ELIOMDEP) -server -ppx $(SERVER_INC) $(SERVER_DEP_DIRS) $< > $@

$(DEPSDIR)/%.client: % | $(DEPSDIR)
	$(ELIOMDEP) -client -ppx $(CLIENT_INC) $(CLIENT_DEP_DIRS) $< > $@

$(DEPSDIR):
	mkdir -p $@
	mkdir -p $(addprefix $@/, ${CLIENT_DIRS})
	mkdir -p $(addprefix $@/, ${SERVER_DIRS})

##----------------------------------------------------------------------
## Documentation

COMMON_OPTIONS := -colorize-code -stars -sort

eliomdoc_wiki = ODOC_WIKI_SUBPROJECT="$(1)" eliomdoc -$(1) -ppx -intro doc/indexdoc.$(1) $(COMMON_OPTIONS) -i $(shell ocamlfind query wikidoc) -g odoc_wiki.cma -d doc/$(1)/wiki $(2)
eliomdoc_html = ODOC_WIKI_SUBPROJECT="$(1)" eliomdoc -$(1) -ppx -intro doc/indexdoc.$(1) $(COMMON_OPTIONS) -html -d doc/$(1)/html $(2)

doc: doc-clean
	mkdir -p doc/client/html
	mkdir -p doc/client/wiki
	mkdir -p doc/server/html
	mkdir -p doc/server/wiki
	$(call eliomdoc_html,client, $(CLIENT_INC) $(CLIENT_INC_DIRS) $(CLIENT_FILES_DOC))
	$(call eliomdoc_wiki,client, $(CLIENT_INC) $(CLIENT_INC_DIRS) $(CLIENT_FILES_DOC))
	$(call eliomdoc_html,server, $(SERVER_INC) $(SERVER_INC_DIRS) $(SERVER_FILES_DOC))
	$(call eliomdoc_wiki,server, $(SERVER_INC) $(SERVER_INC_DIRS) $(SERVER_FILES_DOC))

doc-clean:
	rm -rf doc/client
	rm -rf doc/server

##----------------------------------------------------------------------
## Clean up

clean:
	-rm -f .depend
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f *.type_mli
	-rm -f META
	-rm -rf ${ELIOM_CLIENT_DIR} ${ELIOM_SERVER_DIR} ${LIBDIR} ${DEPSDIR}

distclean: clean
	-rm -rf $(DEPSDIR) .depend
