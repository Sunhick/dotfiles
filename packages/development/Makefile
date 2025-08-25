# Development Package Makefile
SHELL := /bin/bash

# Available development tools
DEV_TOOLS := git

.PHONY: all install uninstall restow list $(DEV_TOOLS) $(addprefix uninstall-,$(DEV_TOOLS)) $(addprefix restow-,$(DEV_TOOLS))

# Default target
all: install

# Install all development tools
install: $(DEV_TOOLS)

# Uninstall all development tools
uninstall: $(addprefix uninstall-,$(DEV_TOOLS))

# Restow all development tools
restow: $(addprefix restow-,$(DEV_TOOLS))

# Individual development tool targets
git:
	@cd git && stow --target=$(HOME) --restow . && echo "  LINK: git"

# Uninstall targets
uninstall-git:
	@cd git && stow --target=$(HOME) --delete . && echo "  UNLINK: git"

# Restow targets
restow-git:
	@cd git && stow --target=$(HOME) --restow . && echo "  RESTOW: git"

# List available development tools
list:
	@echo "Available development tools:"
	@echo "  git - Git version control configuration"
