# Dotfiles Management Makefile

SHELL := /bin/bash
include make/log.mk

# All packages (flat structure)
ALL_PKGS := bash emacs nano vscode git tmux fzf ripgrep inputrc \
            curlrc editorconfig gnupg htop i3 terminal

# Groups for convenience installs
SHELL_PKGS   := bash
EDITOR_PKGS  := emacs nano vscode
DEV_PKGS     := git
DESKTOP_PKGS := i3 terminal
TOOL_PKGS    := tmux fzf ripgrep inputrc curlrc editorconfig gnupg htop

# Stow flags: --no-folding ensures file-level symlinks (not directory-level)
STOW_FLAGS := --no-folding --target=$(HOME)

# Backup
BACKUP_DIR    := $(HOME)/.dotfiles-backup/$(shell date +%Y%m%d_%H%M%S)
LATEST_BACKUP := $(HOME)/.dotfiles-backup/latest

.PHONY: help install adopt safe-install sync plan ls doctor \
	check-stow backup restore list-backups prune-backups \
	shell editors development desktop tools prune \
	$(ALL_PKGS) $(addprefix rm-,$(ALL_PKGS))

help:
	@echo "Dotfiles Management"
	@echo "==================="
	@echo ""
	@echo "Targets:"
	@echo "  install       - Install all packages"
	@echo "  adopt         - Install all (adopt existing files)"
	@echo "  safe-install  - Backup then install"
	@echo "  sync          - Re-link all packages"
	@echo "  plan          - Preview changes (dry run)"
	@echo "  ls            - List available packages"
	@echo "  doctor        - Health check"
	@echo "  prune         - Remove broken symlinks"
	@echo ""
	@echo "Groups:"
	@echo "  shell         - $(SHELL_PKGS)"
	@echo "  editors       - $(EDITOR_PKGS)"
	@echo "  development   - $(DEV_PKGS)"
	@echo "  desktop       - $(DESKTOP_PKGS)"
	@echo "  tools         - $(TOOL_PKGS)"
	@echo ""
	@echo "Individual: make <package>  (e.g. make git, make tmux)"
	@echo "Remove:     make rm-<package>"

check-stow:
	@which stow > /dev/null || ($(call log,fail,GNU Stow is not installed) && exit 1)

# ── Core targets ──────────────────────────────────────────────────
install: check-stow $(ALL_PKGS)
safe-install: backup install

adopt: check-stow
	@for pkg in $(ALL_PKGS); do \
		if [ -d "$$pkg" ]; then \
			(cd $$pkg && stow $(STOW_FLAGS) --adopt . 2>/dev/null; \
			 stow $(STOW_FLAGS) --restow . && printf "  $(_GREEN)OK$(_NC)   %s\n" "LINK $$pkg (adopted)"); \
		fi; \
	done

sync: check-stow $(addsuffix .restow,$(ALL_PKGS))

# ── Group targets ─────────────────────────────────────────────────
shell: check-stow $(SHELL_PKGS)
editors: check-stow $(EDITOR_PKGS)
development: check-stow $(DEV_PKGS)
desktop: check-stow $(DESKTOP_PKGS)
tools: check-stow $(TOOL_PKGS)

# ── Per-package targets ───────────────────────────────────────────
$(ALL_PKGS): check-stow
	@cd $@ && stow $(STOW_FLAGS) --restow . && $(call log,ok,LINK $@)

$(addprefix rm-,$(ALL_PKGS)):
	@cd $(subst rm-,,$@) && stow $(STOW_FLAGS) --delete . && $(call log,ok,UNLINK $(subst rm-,,$@))

$(addsuffix .restow,$(ALL_PKGS)): check-stow
	@cd $(subst .restow,,$@) && stow $(STOW_FLAGS) --restow . && $(call log,ok,SYNC $(subst .restow,,$@))

# ── Plan (dry run) ────────────────────────────────────────────────
plan: check-stow
	@for pkg in $(ALL_PKGS); do \
		if [ -d "$$pkg" ]; then \
			if (cd "$$pkg" && stow $(STOW_FLAGS) --simulate . >/dev/null 2>&1); then \
				printf "  $(_GREEN)OK$(_NC)   %s\n" "WOULD LINK $$pkg"; \
			else \
				printf "  $(_YELLOW)WARN$(_NC) %s\n" "WOULD LINK $$pkg (conflicts)"; \
			fi; \
		fi; \
	done

ls:
	@$(call log,info,Shell:)
	@for p in $(SHELL_PKGS); do echo "  $$p"; done
	@$(call log,info,Editors:)
	@for p in $(EDITOR_PKGS); do echo "  $$p"; done
	@$(call log,info,Development:)
	@for p in $(DEV_PKGS); do echo "  $$p"; done
	@$(call log,info,Desktop:)
	@for p in $(DESKTOP_PKGS); do echo "  $$p"; done
	@$(call log,info,Tools:)
	@for p in $(TOOL_PKGS); do echo "  $$p"; done

prune:
	@$(call log,info,Removing broken symlinks...)
	@find $(HOME)/.config -type l -exec test ! -e {} \; -print -delete 2>/dev/null || true
	@$(call log,ok,Prune complete)

# ── Backup / Restore ──────────────────────────────────────────────
backup:
	@$(call log,info,Creating backup at $(BACKUP_DIR)...)
	@mkdir -p "$(BACKUP_DIR)"
	@for config in bash git emacs nano terminal tmux htop ripgrep fd curl gnupg Code; do \
		if [ -d "$(HOME)/.config/$$config" ]; then \
			cp -r "$(HOME)/.config/$$config" "$(BACKUP_DIR)/"; \
			printf "  $(_GREEN)OK$(_NC)   %s\n" "BACKUP .config/$$config/"; \
		fi; \
	done
	@for file in .bashrc .gitconfig .inputrc; do \
		if [ -f "$(HOME)/$$file" ]; then \
			cp "$(HOME)/$$file" "$(BACKUP_DIR)/"; \
			printf "  $(_GREEN)OK$(_NC)   %s\n" "BACKUP $$file"; \
		fi; \
	done
	@rm -f "$(LATEST_BACKUP)"
	@ln -sf "$(BACKUP_DIR)" "$(LATEST_BACKUP)"
	@$(call log,ok,Backup completed at $(BACKUP_DIR))

restore:
	@if [ ! -d "$(LATEST_BACKUP)" ]; then \
		printf "  $(_RED)FAIL$(_NC) %s\n" "No backup found at $(LATEST_BACKUP)"; \
		exit 1; \
	fi
	@$(call log,warn,This will overwrite current configurations)
	@read -p "Continue? (y/N): " confirm && [ "$$confirm" = "y" ] || exit 1
	@for file in .bashrc .gitconfig .inputrc; do \
		if [ -f "$(LATEST_BACKUP)/$$file" ]; then \
			cp "$(LATEST_BACKUP)/$$file" "$(HOME)/"; \
			printf "  $(_GREEN)OK$(_NC)   %s\n" "RESTORE $$file"; \
		fi; \
	done
	@for config in bash git emacs nano terminal tmux htop ripgrep fd curl gnupg Code; do \
		if [ -d "$(LATEST_BACKUP)/$$config" ]; then \
			cp -r "$(LATEST_BACKUP)/$$config" "$(HOME)/.config/"; \
			printf "  $(_GREEN)OK$(_NC)   %s\n" "RESTORE .config/$$config/"; \
		fi; \
	done
	@$(call log,ok,Restore completed from $(LATEST_BACKUP))

list-backups:
	@if [ -d "$(HOME)/.dotfiles-backup" ]; then \
		ls -la "$(HOME)/.dotfiles-backup/" | grep "^d" | awk '{print "  " $$9 " (" $$6 " " $$7 " " $$8 ")"}' | grep -v "^  \." || echo "  No backups found"; \
	else \
		$(call log,warn,No backup directory found); \
	fi

prune-backups:
	@$(call log,info,Cleaning old backups - keeping last 5...)
	@if [ -d "$(HOME)/.dotfiles-backup" ]; then \
		cd "$(HOME)/.dotfiles-backup" && \
		ls -t | grep -E '^[0-9]{8}_[0-9]{6}$$' | tail -n +6 | xargs -r rm -rf; \
		$(call log,ok,Old backups pruned); \
	fi

# ── Health check ──────────────────────────────────────────────────
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
	for cmd in emacs fzf bat eza fd rg tmux gpg curl jq htop; do \
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
