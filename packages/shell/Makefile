# Shell Package Makefile
SHELL := /bin/bash
include ../../make/log.mk

UNIX_SHELLS := bash zsh
ALL_SHELLS := bash zsh powershell

.PHONY: all install uninstall restow list dryrun $(ALL_SHELLS) \
	$(addprefix uninstall-,$(ALL_SHELLS)) \
	$(addprefix restow-,$(ALL_SHELLS)) \
	$(addprefix dryrun-,$(ALL_SHELLS))

all: install
install: $(UNIX_SHELLS)
uninstall: $(addprefix uninstall-,$(UNIX_SHELLS))
restow: $(addprefix restow-,$(UNIX_SHELLS))
dryrun: $(addprefix dryrun-,$(UNIX_SHELLS))

bash zsh:
	@cd $@ && stow --target=$(HOME) --restow . && $(call log,ok,LINK $@)

powershell:
	@$(call log,warn,SKIP powershell (Windows-only))

uninstall-bash uninstall-zsh:
	@cd $(subst uninstall-,,$@) && stow --target=$(HOME) --delete . && $(call log,ok,UNLINK $(subst uninstall-,,$@))

uninstall-powershell:
	@$(call log,warn,SKIP powershell (Windows-only))

restow-bash restow-zsh:
	@cd $(subst restow-,,$@) && stow --target=$(HOME) --restow . && $(call log,ok,RESTOW $(subst restow-,,$@))

restow-powershell:
	@$(call log,warn,SKIP powershell (Windows-only))

dryrun-bash dryrun-zsh:
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

dryrun-powershell:
	@$(call log,warn,SKIP powershell (Windows-only))

list:
	@echo "Available shells:"
	@echo "  bash       - Bash shell configuration"
	@echo "  zsh        - Zsh shell configuration"
	@echo "  powershell - PowerShell configuration (Windows)"
