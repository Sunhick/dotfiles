# Shell Package Makefile
SHELL := /bin/bash

# Available shells (Unix-only for default install)
UNIX_SHELLS := bash zsh
ALL_SHELLS := bash zsh powershell

.PHONY: all install uninstall restow list dryrun $(ALL_SHELLS) $(addprefix uninstall-,$(ALL_SHELLS)) $(addprefix restow-,$(ALL_SHELLS)) $(addprefix dryrun-,$(ALL_SHELLS))

# Default target
all: install

# Install Unix shells only (PowerShell excluded)
install: $(UNIX_SHELLS)

# Uninstall Unix shells only
uninstall: $(addprefix uninstall-,$(UNIX_SHELLS))

# Restow Unix shells only
restow: $(addprefix restow-,$(UNIX_SHELLS))

# Dry run Unix shells only
dryrun: $(addprefix dryrun-,$(UNIX_SHELLS))

# Individual shell targets
bash:
	@cd bash && stow --target=$(HOME) --restow . && echo "  LINK: bash"

zsh:
	@cd zsh && stow --target=$(HOME) --restow . && echo "  LINK: zsh"

powershell:
	@echo "  SKIP: powershell (Windows-only, use setup-powershell-profile.ps1)"

# Uninstall targets
uninstall-bash:
	@cd bash && stow --target=$(HOME) --delete . && echo "  UNLINK: bash"

uninstall-zsh:
	@cd zsh && stow --target=$(HOME) --delete . && echo "  UNLINK: zsh"

uninstall-powershell:
	@echo "  SKIP: powershell (Windows-only, manual uninstall required)"

# Restow targets
restow-bash:
	@cd bash && stow --target=$(HOME) --restow . && echo "  RESTOW: bash"

restow-zsh:
	@cd zsh && stow --target=$(HOME) --restow . && echo "  RESTOW: zsh"

restow-powershell:
	@echo "  SKIP: powershell (Windows-only, use setup-powershell-profile.ps1)"

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

dryrun-powershell:
	@echo "  SKIP: powershell (Windows-only, use setup-powershell-profile.ps1)"

# List available shells
list:
	@echo "Available shells:"
	@echo "  bash       - Bash shell configuration"
	@echo "  zsh        - Zsh shell configuration"
	@echo "  powershell - PowerShell configuration (Windows)"
