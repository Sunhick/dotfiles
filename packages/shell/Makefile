# Shell Package Makefile
SHELL := /bin/bash

# Available shells
SHELLS := bash zsh

.PHONY: all install uninstall restow list $(SHELLS) $(addprefix uninstall-,$(SHELLS)) $(addprefix restow-,$(SHELLS))

# Default target
all: install

# Install all shells
install: $(SHELLS)

# Uninstall all shells
uninstall: $(addprefix uninstall-,$(SHELLS))

# Restow all shells
restow: $(addprefix restow-,$(SHELLS))

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

# List available shells
list:
	@echo "Available shells:"
	@echo "  bash - Bash shell configuration"
	@echo "  zsh  - Zsh shell configuration"
