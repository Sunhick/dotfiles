# Tools Package Makefile
SHELL := /bin/bash
include ../../make/log.mk

PACKAGES := curlrc editorconfig fzf gnupg homebrew htop inputrc ripgrep tmux

.PHONY: all install uninstall restow list dryrun $(PACKAGES) \
	$(addprefix uninstall-,$(PACKAGES)) \
	$(addprefix restow-,$(PACKAGES)) \
	$(addprefix dryrun-,$(PACKAGES))

all: install
install: $(PACKAGES)
uninstall: $(addprefix uninstall-,$(PACKAGES))
restow: $(addprefix restow-,$(PACKAGES))
dryrun: $(addprefix dryrun-,$(PACKAGES))

$(PACKAGES):
	@cd $@ && stow --target=$(HOME) --restow . && $(call log,ok,LINK $@)

$(addprefix uninstall-,$(PACKAGES)):
	@cd $(subst uninstall-,,$@) && stow --target=$(HOME) --delete . && $(call log,ok,UNLINK $(subst uninstall-,,$@))

$(addprefix restow-,$(PACKAGES)):
	@cd $(subst restow-,,$@) && stow --target=$(HOME) --restow . && $(call log,ok,RESTOW $(subst restow-,,$@))

$(addprefix dryrun-,$(PACKAGES)):
	@pkg=$(subst dryrun-,,$@); \
	if [ -d "$$pkg" ]; then \
		if (cd "$$pkg" && stow --target=$(HOME) --simulate . >/dev/null 2>&1); then \
			$(call log,ok,WOULD LINK $$pkg); \
		else \
			$(call log,warn,WOULD LINK $$pkg (conflicts)); \
		fi; \
	else \
		$(call log,warn,SKIP $$pkg (not found)); \
	fi

list:
	@echo "Available tools:"
	@echo "  curlrc       - curl configuration"
	@echo "  editorconfig - EditorConfig for consistent coding styles"
	@echo "  fzf          - fuzzy finder"
	@echo "  gnupg        - GnuPG configuration"
	@echo "  homebrew     - Homebrew Brewfile for package management"
	@echo "  htop         - htop configuration"
	@echo "  inputrc      - readline configuration"
	@echo "  ripgrep      - ripgrep search configuration"
	@echo "  tmux         - tmux terminal multiplexer"
