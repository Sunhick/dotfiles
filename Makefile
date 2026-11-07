# Dotfiles Management Makefile

SHELL := /bin/bash
include make/log.mk

UNAME_S := $(shell uname -s)

# Packages by group (flat structure). Add a new package to one group only.
SHELL_PKGS   := bash
EDITOR_PKGS  := emacs nano vscode
DEV_PKGS     := git
DESKTOP_PKGS := i3 terminal
TOOL_PKGS    := tmux fzf ripgrep inputrc blesh curlrc editorconfig gnupg htop

# Every package (each gets `make <pkg>` / `make rm-<pkg>` targets)
ALL_PKGS := $(SHELL_PKGS) $(EDITOR_PKGS) $(DEV_PKGS) $(DESKTOP_PKGS) $(TOOL_PKGS)

# Packages that only make sense on Linux; skipped by install/sync/adopt/plan
# and the desktop group on macOS (still installable with `make i3`).
LINUX_ONLY_PKGS := i3
ifeq ($(UNAME_S),Darwin)
OS_PKGS := $(filter-out $(LINUX_ONLY_PKGS),$(ALL_PKGS))
else
OS_PKGS := $(ALL_PKGS)
endif

# Stow flags: --no-folding ensures file-level symlinks (not directory-level)
STOW_FLAGS := --no-folding --target=$(HOME)

# XDG base directories
XDG_CONFIG := $(or $(XDG_CONFIG_HOME),$(HOME)/.config)
XDG_DATA   := $(or $(XDG_DATA_HOME),$(HOME)/.local/share)
XDG_CACHE  := $(or $(XDG_CACHE_HOME),$(HOME)/.cache)
XDG_STATE  := $(or $(XDG_STATE_HOME),$(HOME)/.local/state)

# tmux plugin manager (tmux-modern.conf loads it from here)
TPM_DIR := $(XDG_DATA)/tmux/plugins/tpm

# Backup
BACKUP_DIR    := $(HOME)/.dotfiles-backup/$(shell date +%Y%m%d_%H%M%S)
LATEST_BACKUP := $(HOME)/.dotfiles-backup/latest

.PHONY: help install adopt safe-install sync plan ls doctor \
	check-stow backup restore list-backups prune-backups \
	shell editors development desktop tools prune deps tpm bootstrap \
	$(ALL_PKGS) $(addprefix rm-,$(ALL_PKGS)) $(addsuffix .restow,$(ALL_PKGS))

help:
	@echo "Dotfiles Management"
	@echo "==================="
	@echo ""
	@echo "Targets:"
	@echo "  bootstrap     - New machine: deps, install, tpm"
	@echo "  deps          - Install packages (Brewfile / apt list)"
	@echo "  install       - Install all packages"
	@echo "  adopt         - Install all (adopt existing files)"
	@echo "  safe-install  - Backup then install"
	@echo "  sync          - Re-link all packages"
	@echo "  plan          - Preview changes (dry run)"
	@echo "  ls            - List available packages"
	@echo "  doctor        - Health check"
	@echo "  prune         - Remove broken symlinks"
	@echo "  tpm           - Install the tmux plugin manager"
	@echo ""
	@echo "Groups:"
	@echo "  shell         - $(SHELL_PKGS)"
	@echo "  editors       - $(EDITOR_PKGS)"
	@echo "  development   - $(DEV_PKGS)"
	@echo "  desktop       - $(filter $(OS_PKGS),$(DESKTOP_PKGS))"
	@echo "  tools         - $(TOOL_PKGS)"
	@echo ""
	@echo "Individual: make <package>  (e.g. make git, make tmux)"
	@echo "Remove:     make rm-<package>"

check-stow:
	@which stow > /dev/null || ($(call log,fail,GNU Stow is not installed) && exit 1)

# ── Core targets ──────────────────────────────────────────────────
install: check-stow $(OS_PKGS)
safe-install: backup install

# New machine: install tools first (that brings stow), then link and add TPM.
# Sub-makes keep the order even under `make -j`.
bootstrap:
	@$(MAKE) --no-print-directory deps
	@$(MAKE) --no-print-directory install
	@$(MAKE) --no-print-directory tpm

deps:
	@case "$(UNAME_S)" in \
		Darwin) \
			if ! command -v brew >/dev/null 2>&1; then \
				$(call log,fail,Homebrew not installed: see https://brew.sh); exit 1; \
			fi; \
			$(call log,info,Installing from homebrew/Brewfile...); \
			brew bundle --file=homebrew/Brewfile ;; \
		Linux) \
			if command -v apt-get >/dev/null 2>&1; then \
				bash apt/apt-packages; \
			else \
				$(call log,warn,No apt-get: install the tools in apt/package-list.txt by hand); \
			fi ;; \
		*) $(call log,warn,Unsupported OS $(UNAME_S): install tools by hand) ;; \
	esac

tpm:
	@if [ -x "$(TPM_DIR)/tpm" ]; then \
		$(call log,ok,TPM already installed at $(TPM_DIR)); \
	else \
		git clone --depth 1 https://github.com/tmux-plugins/tpm "$(TPM_DIR)" && \
		$(call log,ok,TPM installed at $(TPM_DIR)) && \
		echo "  In tmux, press prefix + I to install the plugins"; \
	fi

# --adopt moves existing files from $HOME into the repo, replacing tracked
# content. Require a clean tree so the result can be reviewed and reverted.
adopt: check-stow
	@if ! git diff --quiet HEAD -- .; then \
		$(call log,fail,Uncommitted changes: commit or stash before adopt); \
		exit 1; \
	fi
	@for pkg in $(OS_PKGS); do \
		if [ -d "$$pkg" ]; then \
			(cd $$pkg && stow $(STOW_FLAGS) --adopt . && \
			 stow $(STOW_FLAGS) --restow . && printf "  $(_GREEN)OK$(_NC)   %s\n" "LINK $$pkg (adopted)"); \
		fi; \
	done
	@if ! git diff --quiet HEAD -- .; then \
		$(call log,warn,Adopted files changed the repo. Review before committing:); \
		git -P diff --stat HEAD -- .; \
		echo "  Discard with: git checkout -- <file>"; \
	fi

sync: check-stow $(addsuffix .restow,$(OS_PKGS))

# ── Group targets ─────────────────────────────────────────────────
shell: check-stow $(SHELL_PKGS)
editors: check-stow $(EDITOR_PKGS)
development: check-stow $(DEV_PKGS)
desktop: check-stow $(filter $(OS_PKGS),$(DESKTOP_PKGS))
tools: check-stow $(TOOL_PKGS)

# ── Per-package targets ───────────────────────────────────────────
$(filter-out vscode,$(ALL_PKGS)): check-stow
	@cd $@ && stow $(STOW_FLAGS) --restow . && $(call log,ok,LINK $@)

# VS Code needs OS-specific symlink (macOS uses ~/Library, Linux uses ~/.config)
vscode: check-stow
	@cd vscode && stow $(STOW_FLAGS) --restow . && $(call log,ok,LINK vscode)
	@case "$$(uname -s)" in \
		Darwin) \
			mkdir -p "$(HOME)/Library/Application Support/Code/User"; \
			for f in settings.json keybindings.json; do \
				src="$(HOME)/.config/Code/User/$$f"; \
				dst="$(HOME)/Library/Application Support/Code/User/$$f"; \
				if [ -f "$$src" ] && [ ! -L "$$dst" ]; then \
					ln -sf "$$src" "$$dst"; \
					printf "  $(_GREEN)OK$(_NC)   %s\n" "SYMLINK $$f -> Library/Application Support/Code/User/"; \
				fi; \
			done ;; \
	esac

$(addprefix rm-,$(ALL_PKGS)):
	@cd $(subst rm-,,$@) && stow $(STOW_FLAGS) --delete . && $(call log,ok,UNLINK $(subst rm-,,$@))

$(addsuffix .restow,$(ALL_PKGS)): check-stow
	@cd $(subst .restow,,$@) && stow $(STOW_FLAGS) --restow . && $(call log,ok,SYNC $(subst .restow,,$@))

# ── Plan (dry run) ────────────────────────────────────────────────
plan: check-stow
	@for pkg in $(OS_PKGS); do \
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
	@for p in $(DESKTOP_PKGS); do \
		case " $(LINUX_ONLY_PKGS) " in *" $$p "*) echo "  $$p (Linux only)" ;; *) echo "  $$p" ;; esac; \
	done
	@$(call log,info,Tools:)
	@for p in $(TOOL_PKGS); do echo "  $$p"; done

# Only broken links under ~/.config that point into this repo.
prune:
	@$(call log,info,Removing broken symlinks into this repo...)
	@find "$(HOME)/.config" -type l ! -exec test -e {} \; -print 2>/dev/null | \
	while IFS= read -r l; do \
		case "$$(readlink "$$l")" in \
			*"$(patsubst $(HOME)/%,%,$(CURDIR))/"*) rm -f "$$l" && echo "  removed $$l" ;; \
		esac; \
	done
	@$(call log,ok,Prune complete)

# ── Backup / Restore ──────────────────────────────────────────────
# Paths (relative to $HOME) that the packages manage. Backups copy what the
# links point to (cp -L), so a backup holds real files, not links into the repo.
BACKUP_PATHS := .bashrc .bash_profile .gitconfig .editorconfig .gnupg/gpg.conf \
	$(addprefix .config/,bash git emacs nano terminal tmux htop ripgrep fzf \
	                     readline blesh curl Code)

backup:
	@$(call log,info,Creating backup at $(BACKUP_DIR)...)
	@mkdir -p "$(BACKUP_DIR)"
	@for p in $(BACKUP_PATHS); do \
		if [ -e "$(HOME)/$$p" ]; then \
			mkdir -p "$(BACKUP_DIR)/$$(dirname "$$p")"; \
			if cp -RL "$(HOME)/$$p" "$(BACKUP_DIR)/$$p" 2>/dev/null; then \
				printf "  $(_GREEN)OK$(_NC)   %s\n" "BACKUP $$p"; \
			else \
				printf "  $(_YELLOW)WARN$(_NC) %s\n" "BACKUP $$p (partial: broken links skipped)"; \
			fi; \
		fi; \
	done
	@touch "$(BACKUP_DIR)/.layout-home"
	@rm -f "$(LATEST_BACKUP)"
	@ln -sf "$(BACKUP_DIR)" "$(LATEST_BACKUP)"
	@$(call log,ok,Backup completed at $(BACKUP_DIR))

# Restores each backed-up file to its $HOME path. A stow link there is removed
# first, so the copy never writes through it into the repo. Run `make install`
# afterwards to link the packages again.
restore:
	@if [ ! -f "$(LATEST_BACKUP)/.layout-home" ]; then \
		printf "  $(_RED)FAIL$(_NC) %s\n" "No backup in the current layout at $(LATEST_BACKUP)"; \
		exit 1; \
	fi
	@$(call log,warn,This will overwrite current configurations)
	@read -p "Continue? (y/N): " confirm && [ "$$confirm" = "y" ] || exit 1
	@cd "$(LATEST_BACKUP)/" && find . -type f ! -name .layout-home -print0 | \
	while IFS= read -r -d '' f; do \
		f="$${f#./}"; dst="$(HOME)/$$f"; \
		mkdir -p "$$(dirname "$$dst")"; \
		case "$$(cd "$$(dirname "$$dst")" && pwd -P)/" in \
			"$(CURDIR)"/*) printf "  $(_YELLOW)WARN$(_NC) %s\n" "SKIP $$f (directory is inside the repo)"; continue ;; \
		esac; \
		[ -L "$$dst" ] && rm -f "$$dst"; \
		cp -p "$$f" "$$dst" && printf "  $(_GREEN)OK$(_NC)   %s\n" "RESTORE $$f"; \
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
	for cmd in emacs fzf bat eza fd rg tmux gpg curl jq htop zoxide mise shellcheck; do \
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
		if command -v "$${EDITOR%% *}" >/dev/null 2>&1; then \
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
			if [ -x "$$GIT_GPG" ] || command -v "$$GIT_GPG" >/dev/null 2>&1; then \
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
