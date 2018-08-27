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
	./l0 examples/hello.lisp hello.cpp
	./l0 examples/fib.lisp fib.cpp
	clang hello.cpp -o hello
	clang fib.cpp -o fib
	./hello
	./fib
	rm hello.cpp hello fib.cpp fib

clean:
	rm -rf $(VENDOR_DIR)
	rm $(BIN)
