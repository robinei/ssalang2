CC = gcc
EXEC_NAME = compiler

SRC = src
BUILD = build
BIN = bin

LDFLAGS = -export-dynamic
CFLAGS = -g
LIBS = 

SRC_EXT = c


rwildcard = $(foreach d, $(wildcard $1*), $(call rwildcard,$d/,$2) \
						$(filter $(subst *,%,$2), $d))

SOURCES = $(call rwildcard, $(SRC), *.$(SRC_EXT))
OBJECTS = $(SOURCES:$(SRC)/%.$(SRC_EXT)=$(BUILD)/%.o)
DEPEND = $(OBJECTS:.o=.d)

.PHONY: build
build: createdirs gentests
	@$(MAKE) all --no-print-directory

.PHONY: createdirs
createdirs:
	@mkdir -p $(dir $(OBJECTS))
	@mkdir -p $(BIN)

all: $(BIN)/$(EXEC_NAME)

$(BIN)/$(EXEC_NAME): $(OBJECTS)
	@echo "Linking $@..."
	@$(CC) $(CFLAGS) $(OBJECTS) $(LDFLAGS) -o $@ $(LIBS)

$(BUILD)/%.o: $(SRC)/%.$(SRC_EXT)
	@echo "[CC] Compiling $< -> $@"
	@$(CC) -c $(CFLAGS) -MP -MMD $< -o $@

-include $(DEPEND)

.PHONY: clean
clean:
	@rm -rf $(BIN) $(BUILD)

.PHONY: gentests
gentests:
	@./test.py --generate

.PHONY: test
test: build
	@./test.py

.PHONY: test
itest: build
	@./test.py --interactive
