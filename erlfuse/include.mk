EBIN_DIR = $(TOP)/ebin
ERLC = erlc
ERLC_FLAGS += +debug_info -Wall -I $(TOP)/include
#ERLC_FLAGS += +native

BIN_DIR = $(TOP)/bin

EBIN_TARGET = $(addprefix $(EBIN_DIR)/, $(EBIN))
BIN_TARGET = $(addprefix $(BIN_DIR)/, $(BIN))
ESRC = $(EBIN:.beam=.erl)

ifneq ($(SUBDIRS),)
TARGETS += subdirs
endif
TARGETS += $(EBIN_TARGET)
TARGETS += $(BIN_TARGET)
TARGETS += $(TARGET)

all: $(TARGETS)

.PHONY: clean
clean: subdirs $(CLEAN)
	rm -f $(EBIN_TARGET) $(BIN_TARGET) $(OBJS)

.PHONY: subdirs
subdirs:
	@for s in $(SUBDIRS) ; do \
		make -C $$s $(MAKECMDGOALS) ; \
	done

$(EBIN_DIR)/%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

$(BIN_TARGET): $(OBJS)
	$(CC) -o $@ $+ $(LDFLAGS)

depends:
	@grep '^-include' $(ESRC) | \
	sed 's/^\(.*\).erl:-include("\(.*\)").$$/\$$\(EBIN_DIR\)\/\1.beam: \2/'

