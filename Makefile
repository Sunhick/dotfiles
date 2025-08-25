# Dotfiles Management Makefile
# Delegates to individual package Makefiles

SHELL := /bin/bash

# Package categories
PACKAGE_DIRS := shell editors development desktop tools

# Backup directory with timestamp
BACKUP_DIR := $(HOME)/.dotfiles-backup/$(shell date +%Y%m%d_%H%M%S)
LATEST_BACKUP := $(HOME)/.dotfiles-backup/latest

.PHONY: help install uninstall restow list backup restore list-backups clean-backups clean-all-backups check-stow install-safe dryrun $(PACKAGE_DIRS) $(addprefix uninstall-,$(PACKAGE_DIRS)) $(addprefix restow-,$(PACKAGE_DIRS)) $(addprefix dryrun-,$(PACKAGE_DIRS))

# Default target
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
	@echo ""
	@echo "Package categories:"
	@echo "  shell         - Shell configurations (bash, zsh)"
	@echo "  editors       - Editor configurations (emacs, nano, vscode)"
	@echo "  development   - Development tools (git)"
	@echo "  desktop       - Desktop environment (i3, terminal)"
	@echo "  tools         - Command-line tools (fd, ripgrep, tmux, etc.)"
	@echo ""
	@echo "Category targets:"
	@echo "  $(PACKAGE_DIRS)"
	@echo "  $(addprefix uninstall-,$(PACKAGE_DIRS))"
	@echo "  $(addprefix restow-,$(PACKAGE_DIRS))"
	@echo "  $(addprefix dryrun-,$(PACKAGE_DIRS))"
	@echo ""
	@echo "Backup targets:"
	@echo "  backup        - Backup existing configuration files"
	@echo "  restore       - Restore from latest backup"
	@echo "  list-backups  - List available backups"
	@echo "  clean-backups - Remove old backups (keep last 5)"
	@echo ""
	@echo "Usage examples:"
	@echo "  make dryrun          # Preview all changes"
	@echo "  make dryrun-tools    # Preview tools installation"
	@echo "  make tools           # Install all tools"
	@echo "  make uninstall-shell # Uninstall shell configs"
	@echo "  make restow-tools    # Restow all tools"
	@echo "  cd packages/tools && make ripgrep  # Install specific tool"

# Check if stow is installed
check-stow:
	@which stow > /dev/null || (echo "GNU Stow is not installed. Please install it first." && exit 1)

# Install all packages
install: check-stow $(PACKAGE_DIRS)

# Safe install - backup first, then install
install-safe: backup install

# Uninstall all packages
uninstall: $(addprefix uninstall-,$(PACKAGE_DIRS))

# Restow all packages
restow: $(addprefix restow-,$(PACKAGE_DIRS))

# Dry run - preview what would be installed
dryrun: check-stow $(addprefix dryrun-,$(PACKAGE_DIRS))

# List all available packages
list:
	@echo "Available package categories:"
	@for dir in $(PACKAGE_DIRS); do \
		echo ""; \
		echo "=== $$dir ==="; \
		cd packages/$$dir && $(MAKE) list; \
	done

# Package category targets
shell: check-stow
	@cd packages/shell && $(MAKE) install

editors: check-stow
	@cd packages/editors && $(MAKE) install

development: check-stow
	@cd packages/development && $(MAKE) install

desktop: check-stow
	@cd packages/desktop && $(MAKE) install

tools: check-stow
	@cd packages/tools && $(MAKE) install

# Uninstall category targets
uninstall-shell:
	@cd packages/shell && $(MAKE) uninstall

uninstall-editors:
	@cd packages/editors && $(MAKE) uninstall

uninstall-development:
	@cd packages/development && $(MAKE) uninstall

uninstall-desktop:
	@cd packages/desktop && $(MAKE) uninstall

uninstall-tools:
	@cd packages/tools && $(MAKE) uninstall

# Restow category targets
restow-shell:
	@cd packages/shell && $(MAKE) restow

restow-editors:
	@cd packages/editors && $(MAKE) restow

restow-development:
	@cd packages/development && $(MAKE) restow

restow-desktop:
	@cd packages/desktop && $(MAKE) restow

restow-tools:
	@cd packages/tools && $(MAKE) restow

# Dry run category targets
dryrun-shell: check-stow
	@cd packages/shell && $(MAKE) dryrun

dryrun-editors: check-stow
	@cd packages/editors && $(MAKE) dryrun 2>/dev/null || \
		(for pkg in packages/editors/*/; do \
			if [ -d "$$pkg" ]; then \
				pkg_name=$$(basename "$$pkg"); \
				echo "  WOULD LINK: $$pkg_name"; \
			fi; \
		done)

dryrun-development: check-stow
	@cd packages/development && $(MAKE) dryrun 2>/dev/null || \
		(for pkg in packages/development/*/; do \
			if [ -d "$$pkg" ]; then \
				pkg_name=$$(basename "$$pkg"); \
				echo "  WOULD LINK: $$pkg_name"; \
			fi; \
		done)

dryrun-desktop: check-stow
	@cd packages/desktop && $(MAKE) dryrun 2>/dev/null || \
		(for pkg in packages/desktop/*/; do \
			if [ -d "$$pkg" ]; then \
				pkg_name=$$(basename "$$pkg"); \
				echo "  WOULD LINK: $$pkg_name"; \
			fi; \
		done)

dryrun-tools: check-stow
	@cd packages/tools && $(MAKE) dryrun

# Backup existing configuration files
backup:
	@echo "Creating backup at $(BACKUP_DIR)..."
	@mkdir -p "$(BACKUP_DIR)"
	@echo "Backing up existing configuration files..."
	@# Backup common config directories
	@for config in bash zsh git emacs nano terminal tmux htop ripgrep fd curl gnupg Code; do \
		if [ -d "$(HOME)/.config/$$config" ]; then \
			cp -r "$(HOME)/.config/$$config" "$(BACKUP_DIR)/"; \
			echo "  BACKUP: .config/$$config/"; \
		fi; \
	done
	@# Backup common dotfiles
	@for file in .bashrc .zshrc .zshenv .gitconfig .inputrc; do \
		if [ -f "$(HOME)/$$file" ]; then \
			cp "$(HOME)/$$file" "$(BACKUP_DIR)/"; \
			echo "  BACKUP: $$file"; \
		fi; \
	done
	@# Backup legacy directories
	@for dir in .emacs.d .gnupg; do \
		if [ -d "$(HOME)/$$dir" ]; then \
			cp -r "$(HOME)/$$dir" "$(BACKUP_DIR)/"; \
			echo "  BACKUP: $$dir/"; \
		fi; \
	done
	@# Create symlink to latest backup
	@rm -f "$(LATEST_BACKUP)"
	@ln -sf "$(BACKUP_DIR)" "$(LATEST_BACKUP)"
	@echo "Backup completed at: $(BACKUP_DIR)"
	@echo "Latest backup symlink: $(LATEST_BACKUP)"

# Restore from latest backup
restore:
	@if [ ! -d "$(LATEST_BACKUP)" ]; then \
		echo "No backup found at $(LATEST_BACKUP)"; \
		echo "Available backups:"; \
		ls -la $(HOME)/.dotfiles-backup/ 2>/dev/null || echo "No backups available"; \
		exit 1; \
	fi
	@echo "Restoring from backup: $(LATEST_BACKUP)"
	@echo "WARNING: This will overwrite current configurations!"
	@read -p "Continue? (y/N): " confirm && [ "$$confirm" = "y" ] || exit 1
	@# Restore common dotfiles
	@for file in .bashrc .zshrc .zshenv .gitconfig .inputrc; do \
		if [ -f "$(LATEST_BACKUP)/$$file" ]; then \
			cp "$(LATEST_BACKUP)/$$file" "$(HOME)/"; \
			echo "  RESTORE: $$file"; \
		fi; \
	done
	@# Restore config directories
	@for config in bash zsh git emacs nano terminal tmux htop ripgrep fd curl gnupg Code; do \
		if [ -d "$(LATEST_BACKUP)/$$config" ]; then \
			cp -r "$(LATEST_BACKUP)/$$config" "$(HOME)/.config/"; \
			echo "  RESTORE: .config/$$config/"; \
		fi; \
	done
	@# Restore legacy directories
	@for dir in .emacs.d .gnupg; do \
		if [ -d "$(LATEST_BACKUP)/$$dir" ]; then \
			cp -r "$(LATEST_BACKUP)/$$dir" "$(HOME)/"; \
			echo "  RESTORE: $$dir/"; \
		fi; \
	done
	@echo "Restore completed from: $(LATEST_BACKUP)"

# List available backups
list-backups:
	@echo "Available backups:"
	@if [ -d "$(HOME)/.dotfiles-backup" ]; then \
		ls -la "$(HOME)/.dotfiles-backup/" | grep "^d" | awk '{print "  " $$9 " (" $$6 " " $$7 " " $$8 ")"}' | grep -v "^  \." || echo "  No backups found"; \
		if [ -L "$(LATEST_BACKUP)" ]; then \
			echo ""; \
			echo "Latest backup points to: $$(readlink $(LATEST_BACKUP))"; \
		fi; \
	else \
		echo "  No backup directory found"; \
	fi

# Clean old backups (keep last 5)
clean-backups:
	@echo "Cleaning old backups (keeping last 5)..."
	@if [ -d "$(HOME)/.dotfiles-backup" ]; then \
		cd "$(HOME)/.dotfiles-backup" && \
		ls -t | grep -E '^[0-9]{8}_[0-9]{6}$$' | tail -n +6 | xargs -r rm -rf && \
		echo "Old backups cleaned"; \
	else \
		echo "No backup directory found"; \
	fi

# Clean all backups (dangerous!)
clean-all-backups:
	@echo "WARNING: This will delete ALL backups!"
	@read -p "Are you sure? (y/N): " confirm && [ "$$confirm" = "y" ] || exit 1
	@rm -rf "$(HOME)/.dotfiles-backup"
	@echo "All backups deleted"
