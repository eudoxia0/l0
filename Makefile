SML := sml
SMLFLAGS := -Cprint.depth=10
MLTON := mlton

BIN := l0

CM_FILE := l0.cm
MLB_FILE := l0.mlb

VENDOR_DIR := vendor
PARSIMONY := $(VENDOR_DIR)/parsimony
PARSIMONY_URL := https://github.com/eudoxia0/parsimony.git

SRC := src/*.sig src/*.sml

all: compile

$(VENDOR_DIR):
	mkdir -p $(VENDOR_DIR)

$(PARSIMONY): $(VENDOR_DIR)
	git clone $(PARSIMONY_URL) $(PARSIMONY)

compile: $(SRC) $(PARSIMONY)
	$(SML) $(SMLFLAGS) -m $(CM_FILE)

$(BIN): $(SRC) $(PARSIMONY)
	$(MLTON) $(MLB_FILE)

.PHONY: test
test: $(BIN)
	./l0 examples/hello.lisp hello.c
	./l0 examples/fib.lisp fib.c
	clang hello.c -o hello
	clang fib.c -o fib
	./hello
	./fib
	rm hello.c hello fib.c fib

clean:
	rm -rf $(VENDOR_DIR)
	rm $(BIN)
