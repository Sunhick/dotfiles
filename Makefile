# Dotfiles Management Makefile
# Delegates to individual package Makefiles

SHELL := /bin/bash
include make/log.mk

# Package categories
PACKAGE_DIRS := shell editors development desktop tools

# Backup directory with timestamp
BACKUP_DIR := $(HOME)/.dotfiles-backup/$(shell date +%Y%m%d_%H%M%S)
LATEST_BACKUP := $(HOME)/.dotfiles-backup/latest

.PHONY: help install uninstall restow list backup restore list-backups \
	clean-backups clean-all-backups check-stow install-safe dryrun doctor \
	$(PACKAGE_DIRS) $(addprefix uninstall-,$(PACKAGE_DIRS)) \
	$(addprefix restow-,$(PACKAGE_DIRS)) $(addprefix dryrun-,$(PACKAGE_DIRS))

help:
	@echo "Dotfiles Management System"
	@echo "=========================="
	@echo ""
	@echo "Main targets:"
	@echo "  install       - Install all package categories"
	@echo "  install-safe  - Backup existing configs then install"
	@echo "  dryrun        - Preview what would be installed (dry run)"
	@echo "  uninstall     - Uninstall all package categories"
	@echo "  restow        - Restow all package categories"
	@echo "  list          - List all available packages"
	@echo "  doctor        - Verify tools, symlinks, and config health"
	@echo ""
	@echo "Category targets: $(PACKAGE_DIRS)"
	@echo ""
	@echo "Backup targets: backup, restore, list-backups, clean-backups"

check-stow:
	@which stow > /dev/null || ($(call log,fail,GNU Stow is not installed) && exit 1)

install: check-stow $(PACKAGE_DIRS)
install-safe: backup install
uninstall: $(addprefix uninstall-,$(PACKAGE_DIRS))
restow: $(addprefix restow-,$(PACKAGE_DIRS))
dryrun: check-stow $(addprefix dryrun-,$(PACKAGE_DIRS))

list:
	@for dir in $(PACKAGE_DIRS); do \
		$(call log,info,=== $$dir ===); \
		cd packages/$$dir && $(MAKE) --no-print-directory list; \
	done

# Package category targets
shell editors development desktop tools: check-stow
	@$(call log,info,Installing $@...)
	@cd packages/$@ && $(MAKE) --no-print-directory install

uninstall-shell uninstall-editors uninstall-development uninstall-desktop uninstall-tools:
	@$(call log,info,Uninstalling $(subst uninstall-,,$@)...)
	@cd packages/$(subst uninstall-,,$@) && $(MAKE) --no-print-directory uninstall

restow-shell restow-editors restow-development restow-desktop restow-tools: check-stow
	@$(call log,info,Restowing $(subst restow-,,$@)...)
	@cd packages/$(subst restow-,,$@) && $(MAKE) --no-print-directory restow

dryrun-shell dryrun-editors dryrun-development dryrun-desktop dryrun-tools: check-stow
	@$(call log,info,Previewing $(subst dryrun-,,$@)...)
	@cd packages/$(subst dryrun-,,$@) && $(MAKE) --no-print-directory dryrun

# Backup existing configuration files
backup:
	@$(call log,info,Creating backup at $(BACKUP_DIR)...)
	@mkdir -p "$(BACKUP_DIR)"
	@for config in bash zsh git emacs nano terminal tmux htop ripgrep fd curl gnupg Code; do \
		if [ -d "$(HOME)/.config/$$config" ]; then \
			cp -r "$(HOME)/.config/$$config" "$(BACKUP_DIR)/"; \
			$(call log,ok,BACKUP .config/$$config/); \
		fi; \
	done
	@for file in .bashrc .zshrc .zshenv .gitconfig .inputrc; do \
		if [ -f "$(HOME)/$$file" ]; then \
			cp "$(HOME)/$$file" "$(BACKUP_DIR)/"; \
			$(call log,ok,BACKUP $$file); \
		fi; \
	done
	@for dir in .emacs.d .gnupg; do \
		if [ -d "$(HOME)/$$dir" ]; then \
			cp -r "$(HOME)/$$dir" "$(BACKUP_DIR)/"; \
			$(call log,ok,BACKUP $$dir/); \
		fi; \
	done
	@rm -f "$(LATEST_BACKUP)"
	@ln -sf "$(BACKUP_DIR)" "$(LATEST_BACKUP)"
	@$(call log,ok,Backup completed at $(BACKUP_DIR))

# Restore from latest backup
restore:
	@if [ ! -d "$(LATEST_BACKUP)" ]; then \
		$(call log,fail,No backup found at $(LATEST_BACKUP)); \
		exit 1; \
	fi
	@$(call log,warn,This will overwrite current configurations)
	@read -p "Continue? (y/N): " confirm && [ "$$confirm" = "y" ] || exit 1
	@for file in .bashrc .zshrc .zshenv .gitconfig .inputrc; do \
		if [ -f "$(LATEST_BACKUP)/$$file" ]; then \
			cp "$(LATEST_BACKUP)/$$file" "$(HOME)/"; \
			$(call log,ok,RESTORE $$file); \
		fi; \
	done
	@for config in bash zsh git emacs nano terminal tmux htop ripgrep fd curl gnupg Code; do \
		if [ -d "$(LATEST_BACKUP)/$$config" ]; then \
			cp -r "$(LATEST_BACKUP)/$$config" "$(HOME)/.config/"; \
			$(call log,ok,RESTORE .config/$$config/); \
		fi; \
	done
	@for dir in .emacs.d .gnupg; do \
		if [ -d "$(LATEST_BACKUP)/$$dir" ]; then \
			cp -r "$(LATEST_BACKUP)/$$dir" "$(HOME)/"; \
			$(call log,ok,RESTORE $$dir/); \
		fi; \
	done
	@$(call log,ok,Restore completed from $(LATEST_BACKUP))

list-backups:
	@if [ -d "$(HOME)/.dotfiles-backup" ]; then \
		ls -la "$(HOME)/.dotfiles-backup/" | grep "^d" | awk '{print "  " $$9 " (" $$6 " " $$7 " " $$8 ")"}' | grep -v "^  \." || echo "  No backups found"; \
	else \
		$(call log,warn,No backup directory found); \
	fi

clean-backups:
	@$(call log,info,Cleaning old backups - keeping last 5...)
	@if [ -d "$(HOME)/.dotfiles-backup" ]; then \
		cd "$(HOME)/.dotfiles-backup" && \
		ls -t | grep -E '^[0-9]{8}_[0-9]{6}$$' | tail -n +6 | xargs -r rm -rf; \
		$(call log,ok,Old backups cleaned); \
	else \
		$(call log,warn,No backup directory found); \
	fi

clean-all-backups:
	@$(call log,warn,This will delete ALL backups)
	@read -p "Are you sure? (y/N): " confirm && [ "$$confirm" = "y" ] || exit 1
	@rm -rf "$(HOME)/.dotfiles-backup"
	@$(call log,ok,All backups deleted)

# ── Health check ────────────────────────────────────────────────────
XDG_CONFIG := $(or $(XDG_CONFIG_HOME),$(HOME)/.config)
XDG_DATA   := $(or $(XDG_DATA_HOME),$(HOME)/.local/share)
XDG_CACHE  := $(or $(XDG_CACHE_HOME),$(HOME)/.cache)
XDG_STATE  := $(or $(XDG_STATE_HOME),$(HOME)/.local/state)

doctor:
	@PASS=0; WARN=0; FAIL=0; \
	pass()  { PASS=$$((PASS + 1)); printf "  $(_GREEN)OK$(_NC)   %s\n" "$$1"; }; \
	warn()  { WARN=$$((WARN + 1)); printf "  $(_YELLOW)WARN$(_NC) %s\n" "$$1"; }; \
	fail()  { FAIL=$$((FAIL + 1)); printf "  $(_RED)FAIL$(_NC) %s\n" "$$1"; }; \
	header(){ printf "\n$(_CYAN)%s$(_NC)\n" "$$1"; }; \
	\
	header "Required tools"; \
	for cmd in git stow bash; do \
		if command -v "$$cmd" >/dev/null 2>&1; then \
			pass "$$cmd ($$(command -v $$cmd))"; \
		else \
			fail "$$cmd not found"; \
		fi; \
	done; \
	\
	header "Optional tools"; \
	for cmd in emacs fzf bat eza fd rg tmux gpg curl jq htop zoxide; do \
		if command -v "$$cmd" >/dev/null 2>&1; then \
			pass "$$cmd"; \
		else \
			warn "$$cmd not installed"; \
		fi; \
	done; \
	\
	header "XDG directories"; \
	for dir in "$(XDG_CONFIG)" "$(XDG_DATA)" "$(XDG_CACHE)" "$(XDG_STATE)"; do \
		if [ -d "$$dir" ]; then \
			pass "$$dir"; \
		else \
			warn "$$dir does not exist"; \
		fi; \
	done; \
	\
	header "Symlinks"; \
	BROKEN=0; \
	if [ -d "$(XDG_CONFIG)" ]; then \
		for link in $$(find "$(XDG_CONFIG)" -type l ! -exec test -e {} \; -print 2>/dev/null); do \
			fail "broken: $$link -> $$(readlink "$$link")"; \
			BROKEN=$$((BROKEN + 1)); \
		done; \
	fi; \
	if [ "$$BROKEN" -eq 0 ]; then \
		pass "no broken symlinks in $(XDG_CONFIG)"; \
	fi; \
	for cfg in bash git ripgrep tmux readline; do \
		if [ -d "$(XDG_CONFIG)/$$cfg" ] || [ -f "$(XDG_CONFIG)/$$cfg" ]; then \
			pass "$$cfg config present"; \
		else \
			warn "$$cfg config not found at $(XDG_CONFIG)/$$cfg"; \
		fi; \
	done; \
	\
	header "Config validation"; \
	if command -v git >/dev/null 2>&1; then \
		if git config --list >/dev/null 2>&1; then \
			pass "git config parses OK"; \
		else \
			fail "git config has errors (run: git config --list)"; \
		fi; \
	fi; \
	if [ -f "$(XDG_CONFIG)/bash/bashrc" ]; then \
		if bash -n "$(XDG_CONFIG)/bash/bashrc" 2>/dev/null; then \
			pass "bash config syntax OK"; \
		else \
			fail "bash config has syntax errors"; \
		fi; \
	fi; \
	if [ -f "$(XDG_CONFIG)/ripgrep/config" ]; then \
		pass "ripgrep config found"; \
	elif [ -n "$${RIPGREP_CONFIG_PATH:-}" ] && [ -f "$$RIPGREP_CONFIG_PATH" ]; then \
		pass "ripgrep config found at $$RIPGREP_CONFIG_PATH"; \
	else \
		warn "ripgrep config not found"; \
	fi; \
	\
	header "Shell environment"; \
	if [ -n "$${EDITOR:-}" ]; then \
		if command -v "$$EDITOR" >/dev/null 2>&1; then \
			pass "EDITOR=$$EDITOR"; \
		else \
			warn "EDITOR=$$EDITOR (not found in PATH)"; \
		fi; \
	else \
		warn "EDITOR not set"; \
	fi; \
	\
	header "GPG"; \
	if command -v gpg >/dev/null 2>&1; then \
		pass "gpg found at $$(command -v gpg)"; \
		GIT_GPG="$$(git config --get gpg.program 2>/dev/null || true)"; \
		if [ -n "$$GIT_GPG" ]; then \
			if [ -x "$$GIT_GPG" ]; then \
				pass "git gpg.program=$$GIT_GPG (exists)"; \
			else \
				fail "git gpg.program=$$GIT_GPG (not found)"; \
			fi; \
		else \
			pass "git gpg.program uses default"; \
		fi; \
	else \
		warn "gpg not installed (commit signing unavailable)"; \
	fi; \
	\
	header "Summary"; \
	printf "  $(_GREEN)$$PASS passed$(_NC), $(_YELLOW)$$WARN warnings$(_NC), $(_RED)$$FAIL errors$(_NC)\n"; \
	if [ "$$FAIL" -gt 0 ]; then \
		printf "\n  $(_RED)Some checks failed. Review the errors above.$(_NC)\n"; \
		exit 1; \
	elif [ "$$WARN" -gt 0 ]; then \
		printf "\n  $(_YELLOW)All good but some optional tools are missing.$(_NC)\n"; \
	else \
		printf "\n  $(_GREEN)Everything looks great!$(_NC)\n"; \
	fi
