# Tools Package Makefile
SHELL := /bin/bash

# Available tools
TOOLS := curlrc editorconfig fzf gnupg htop inputrc ripgrep tmux

.PHONY: all install uninstall restow list dryrun $(TOOLS) $(addprefix uninstall-,$(TOOLS)) $(addprefix restow-,$(TOOLS)) $(addprefix dryrun-,$(TOOLS))

# Default target
all: install

# Install all tools
install: $(TOOLS)

# Uninstall all tools
uninstall: $(addprefix uninstall-,$(TOOLS))

# Restow all tools
restow: $(addprefix restow-,$(TOOLS))

# Dry run all tools
dryrun: $(addprefix dryrun-,$(TOOLS))

# Individual tool targets
curlrc:
	@cd curlrc && stow --target=$(HOME) --restow . && echo "  LINK: curlrc"

editorconfig:
	@cd editorconfig && stow --target=$(HOME) --restow . && echo "  LINK: editorconfig"

fzf:
	@cd fzf && stow --target=$(HOME) --restow . && echo "  LINK: fzf"

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

uninstall-editorconfig:
	@cd editorconfig && stow --target=$(HOME) --delete . && echo "  UNLINK: editorconfig"

uninstall-fzf:
	@cd fzf && stow --target=$(HOME) --delete . && echo "  UNLINK: fzf"

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

restow-editorconfig:
	@cd editorconfig && stow --target=$(HOME) --restow . && echo "  RESTOW: editorconfig"

restow-fzf:
	@cd fzf && stow --target=$(HOME) --restow . && echo "  RESTOW: fzf"

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

# Dry run targets
dryrun-curlrc:
	@if [ -d curlrc ]; then \
		if cd curlrc && stow --target=$(HOME) --simulate . >/dev/null 2>&1; then \
			echo "  WOULD LINK: curlrc"; \
		else \
			echo "  WOULD LINK: curlrc (conflicts)"; \
		fi; \
	else \
		echo "  SKIP: curlrc (not found)"; \
	fi

dryrun-editorconfig:
	@if [ -d editorconfig ]; then \
		if cd editorconfig && stow --target=$(HOME) --simulate . >/dev/null 2>&1; then \
			echo "  WOULD LINK: editorconfig"; \
		else \
			echo "  WOULD LINK: editorconfig (conflicts)"; \
		fi; \
	else \
		echo "  SKIP: editorconfig (not found)"; \
	fi

dryrun-fzf:
	@if [ -d fzf ]; then \
		if cd fzf && stow --target=$(HOME) --simulate . >/dev/null 2>&1; then \
			echo "  WOULD LINK: fzf"; \
		else \
			echo "  WOULD LINK: fzf (conflicts)"; \
		fi; \
	else \
		echo "  SKIP: fzf (not found)"; \
	fi

dryrun-gnupg:
	@if [ -d gnupg ]; then \
		if cd gnupg && stow --target=$(HOME) --simulate . >/dev/null 2>&1; then \
			echo "  WOULD LINK: gnupg"; \
		else \
			echo "  WOULD LINK: gnupg (conflicts)"; \
		fi; \
	else \
		echo "  SKIP: gnupg (not found)"; \
	fi

dryrun-htop:
	@if [ -d htop ]; then \
		if cd htop && stow --target=$(HOME) --simulate . >/dev/null 2>&1; then \
			echo "  WOULD LINK: htop"; \
		else \
			echo "  WOULD LINK: htop (conflicts)"; \
		fi; \
	else \
		echo "  SKIP: htop (not found)"; \
	fi

dryrun-inputrc:
	@if [ -d inputrc ]; then \
		if cd inputrc && stow --target=$(HOME) --simulate . >/dev/null 2>&1; then \
			echo "  WOULD LINK: inputrc"; \
		else \
			echo "  WOULD LINK: inputrc (conflicts)"; \
		fi; \
	else \
		echo "  SKIP: inputrc (not found)"; \
	fi

dryrun-ripgrep:
	@if [ -d ripgrep ]; then \
		if cd ripgrep && stow --target=$(HOME) --simulate . >/dev/null 2>&1; then \
			echo "  WOULD LINK: ripgrep"; \
		else \
			echo "  WOULD LINK: ripgrep (conflicts)"; \
		fi; \
	else \
		echo "  SKIP: ripgrep (not found)"; \
	fi

dryrun-tmux:
	@if [ -d tmux ]; then \
		if cd tmux && stow --target=$(HOME) --simulate . >/dev/null 2>&1; then \
			echo "  WOULD LINK: tmux"; \
		else \
			echo "  WOULD LINK: tmux (conflicts)"; \
		fi; \
	else \
		echo "  SKIP: tmux (not found)"; \
	fi

# List available tools
list:
	@echo "Available tools:"
	@echo "  curlrc       - curl configuration"
	@echo "  editorconfig - EditorConfig for consistent coding styles"
	@echo "  fzf          - fuzzy finder"
	@echo "  gnupg        - GnuPG configuration"
	@echo "  htop         - htop configuration"
	@echo "  inputrc      - readline configuration"
	@echo "  ripgrep      - ripgrep search configuration"
	@echo "  tmux         - tmux terminal multiplexer"
