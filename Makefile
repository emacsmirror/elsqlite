# Makefile for ELSQLite

EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -L . -L tests
EL_FILES = $(filter-out %-autoloads.el, $(wildcard elsqlite*.el))

.PHONY: test compile clean check-emacs checkdoc help

help:
	@echo "ELSQLite development tasks:"
	@echo "  make test         - Run test suite"
	@echo "  make compile      - Byte-compile all .el files"
	@echo "  make checkdoc     - Check docstring formatting"
	@echo "  make clean        - Remove compiled files"
	@echo "  make check-emacs  - Verify Emacs version and SQLite support"
	@echo "  make check        - Run compilation + tests"

check-emacs:
	@echo "Checking Emacs configuration..."
	@$(EMACS) --version | head -n1
	@$(BATCH) --eval "(message \"SQLite support: %s\" (sqlite-available-p))"

test: check-emacs
	@echo "Running test suite..."
	@$(BATCH) -l tests/elsqlite-test.el -f ert-run-tests-batch-and-exit

compile:
	@echo "Byte-compiling..."
	@$(BATCH) -f batch-byte-compile $(EL_FILES)
	@echo "Cleaning compiled files..."
	@rm -f *.elc tests/*.elc

checkdoc:
	@echo "Running checkdoc..."
	@$(BATCH) --eval "(or (fboundp 'checkdoc-file) (load \"checkdoc\"))" \
	          --eval "(dolist (file command-line-args-left) (checkdoc-file file))" \
	          $(EL_FILES)

clean:
	@echo "Cleaning compiled files..."
	@rm -f *.elc tests/*.elc

check: compile test
	@echo "All checks passed!"
