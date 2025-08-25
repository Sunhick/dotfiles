# Editors Package Makefile
SHELL := /bin/bash

# Available editors
EDITORS := emacs nano vscode

.PHONY: all install uninstall restow list $(EDITORS) $(addprefix uninstall-,$(EDITORS)) $(addprefix restow-,$(EDITORS))

# Default target
all: install

# Install all editors
install: $(EDITORS)

# Uninstall all editors
uninstall: $(addprefix uninstall-,$(EDITORS))

# Restow all editors
restow: $(addprefix restow-,$(EDITORS))

# Individual editor targets
emacs:
	@cd emacs && stow --target=$(HOME) --restow . && echo "  LINK: emacs"

nano:
	@cd nano && stow --target=$(HOME) --restow . && echo "  LINK: nano"

vscode:
	@cd vscode && stow --target=$(HOME) --restow . && echo "  LINK: vscode"

# Uninstall targets
uninstall-emacs:
	@cd emacs && stow --target=$(HOME) --delete . && echo "  UNLINK: emacs"

uninstall-nano:
	@cd nano && stow --target=$(HOME) --delete . && echo "  UNLINK: nano"

uninstall-vscode:
	@cd vscode && stow --target=$(HOME) --delete . && echo "  UNLINK: vscode"

# Restow targets
restow-emacs:
	@cd emacs && stow --target=$(HOME) --restow . && echo "  RESTOW: emacs"

restow-nano:
	@cd nano && stow --target=$(HOME) --restow . && echo "  RESTOW: nano"

restow-vscode:
	@cd vscode && stow --target=$(HOME) --restow . && echo "  RESTOW: vscode"

# List available editors
list:
	@echo "Available editors:"
	@echo "  emacs  - Emacs editor configuration"
	@echo "  nano   - Nano editor configuration"
	@echo "  vscode - Visual Studio Code configuration"
