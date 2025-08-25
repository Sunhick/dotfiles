# Desktop Package Makefile
SHELL := /bin/bash

# Available desktop tools
DESKTOP_TOOLS := i3 terminal

.PHONY: all install uninstall restow list $(DESKTOP_TOOLS) $(addprefix uninstall-,$(DESKTOP_TOOLS)) $(addprefix restow-,$(DESKTOP_TOOLS))

# Default target
all: install

# Install all desktop tools
install: $(DESKTOP_TOOLS)

# Uninstall all desktop tools
uninstall: $(addprefix uninstall-,$(DESKTOP_TOOLS))

# Restow all desktop tools
restow: $(addprefix restow-,$(DESKTOP_TOOLS))

# Individual desktop tool targets
i3:
	@cd i3 && stow --target=$(HOME) --restow . && echo "  LINK: i3"

terminal:
	@cd terminal && stow --target=$(HOME) --restow . && echo "  LINK: terminal"

# Uninstall targets
uninstall-i3:
	@cd i3 && stow --target=$(HOME) --delete . && echo "  UNLINK: i3"

uninstall-terminal:
	@cd terminal && stow --target=$(HOME) --delete . && echo "  UNLINK: terminal"

# Restow targets
restow-i3:
	@cd i3 && stow --target=$(HOME) --restow . && echo "  RESTOW: i3"

restow-terminal:
	@cd terminal && stow --target=$(HOME) --restow . && echo "  RESTOW: terminal"

# List available desktop tools
list:
	@echo "Available desktop tools:"
	@echo "  i3       - i3 window manager configuration"
	@echo "  terminal - Terminal emulator configuration"
