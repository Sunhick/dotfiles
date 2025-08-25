# Tools Package Makefile
SHELL := /bin/bash

# Available tools
TOOLS := curlrc gnupg htop inputrc ripgrep tmux

.PHONY: all install uninstall restow list $(TOOLS) $(addprefix uninstall-,$(TOOLS)) $(addprefix restow-,$(TOOLS))

# Default target
all: install

# Install all tools
install: $(TOOLS)

# Uninstall all tools
uninstall: $(addprefix uninstall-,$(TOOLS))

# Restow all tools
restow: $(addprefix restow-,$(TOOLS))

# Individual tool targets
curlrc:
	@cd curlrc && stow --target=$(HOME) --restow . && echo "  LINK: curlrc"

gnupg:
	@cd gnupg && stow --target=$(HOME) --restow . && echo "  LINK: gnupg"

htop:
	@cd htop && stow --target=$(HOME) --restow . && echo "  LINK: htop"

inputrc:
	@cd inputrc && stow --target=$(HOME) --restow . && echo "  LINK: inputrc"

ripgrep:
	@cd ripgrep && stow --target=$(HOME) --restow . && echo "  LINK: ripgrep"

tmux:
	@cd tmux && stow --target=$(HOME) --restow . && echo "  LINK: tmux"

# Uninstall targets
uninstall-curlrc:
	@cd curlrc && stow --target=$(HOME) --delete . && echo "  UNLINK: curlrc"

uninstall-gnupg:
	@cd gnupg && stow --target=$(HOME) --delete . && echo "  UNLINK: gnupg"

uninstall-htop:
	@cd htop && stow --target=$(HOME) --delete . && echo "  UNLINK: htop"

uninstall-inputrc:
	@cd inputrc && stow --target=$(HOME) --delete . && echo "  UNLINK: inputrc"

uninstall-ripgrep:
	@cd ripgrep && stow --target=$(HOME) --delete . && echo "  UNLINK: ripgrep"

uninstall-tmux:
	@cd tmux && stow --target=$(HOME) --delete . && echo "  UNLINK: tmux"

# Restow targets
restow-curlrc:
	@cd curlrc && stow --target=$(HOME) --restow . && echo "  RESTOW: curlrc"

restow-gnupg:
	@cd gnupg && stow --target=$(HOME) --restow . && echo "  RESTOW: gnupg"

restow-htop:
	@cd htop && stow --target=$(HOME) --restow . && echo "  RESTOW: htop"

restow-inputrc:
	@cd inputrc && stow --target=$(HOME) --restow . && echo "  RESTOW: inputrc"

restow-ripgrep:
	@cd ripgrep && stow --target=$(HOME) --restow . && echo "  RESTOW: ripgrep"

restow-tmux:
	@cd tmux && stow --target=$(HOME) --restow . && echo "  RESTOW: tmux"

# List available tools
list:
	@echo "Available tools:"
	@echo "  curlrc    - curl configuration"
	@echo "  gnupg     - GnuPG configuration"
	@echo "  htop      - htop configuration"
	@echo "  inputrc   - readline configuration"
	@echo "  ripgrep   - ripgrep search configuration"
	@echo "  tmux      - tmux terminal multiplexer"
