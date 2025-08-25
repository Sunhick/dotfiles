# Shell Package Makefile
SHELL := /bin/bash

# Available shells
SHELLS := bash zsh

.PHONY: all install uninstall restow list dryrun $(SHELLS) $(addprefix uninstall-,$(SHELLS)) $(addprefix restow-,$(SHELLS)) $(addprefix dryrun-,$(SHELLS))

# Default target
all: install

# Install all shells
install: $(SHELLS)

# Uninstall all shells
uninstall: $(addprefix uninstall-,$(SHELLS))

# Restow all shells
restow: $(addprefix restow-,$(SHELLS))

# Dry run all shells
dryrun: $(addprefix dryrun-,$(SHELLS))

# Individual shell targets
bash:
	@cd bash && stow --target=$(HOME) --restow . && echo "  LINK: bash"

zsh:
	@cd zsh && stow --target=$(HOME) --restow . && echo "  LINK: zsh"

# Uninstall targets
uninstall-bash:
	@cd bash && stow --target=$(HOME) --delete . && echo "  UNLINK: bash"

uninstall-zsh:
	@cd zsh && stow --target=$(HOME) --delete . && echo "  UNLINK: zsh"

# Restow targets
restow-bash:
	@cd bash && stow --target=$(HOME) --restow . && echo "  RESTOW: bash"

restow-zsh:
	@cd zsh && stow --target=$(HOME) --restow . && echo "  RESTOW: zsh"

# Dry run targets
dryrun-bash:
	@if [ -d bash ]; then \
		if cd bash && stow --target=$(HOME) --simulate . >/dev/null 2>&1; then \
			echo "  WOULD LINK: bash"; \
		else \
			echo "  WOULD LINK: bash (conflicts)"; \
		fi; \
	else \
		echo "  SKIP: bash (not found)"; \
	fi

dryrun-zsh:
	@if [ -d zsh ]; then \
		if cd zsh && stow --target=$(HOME) --simulate . >/dev/null 2>&1; then \
			echo "  WOULD LINK: zsh"; \
		else \
			echo "  WOULD LINK: zsh (conflicts)"; \
		fi; \
	else \
		echo "  SKIP: zsh (not found)"; \
	fi

# List available shells
list:
	@echo "Available shells:"
	@echo "  bash - Bash shell configuration"
	@echo "  zsh  - Zsh shell configuration"
