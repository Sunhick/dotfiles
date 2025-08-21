# Advanced Dotfiles Makefile for Stow Operations
# Usage: make <target>

.PHONY: help install uninstall restow status clean validate backup test

# Default target
.DEFAULT_GOAL := help

# Configuration
STOW_DIR := $(CURDIR)
TARGET_DIR := $(HOME)
BACKUP_DIR := $(HOME)/.dotfiles-backup

# Discover all packages (directories that aren't special)
PACKAGES := $(shell find . -maxdepth 1 -type d -not -name '.*' -not -name 'docs' -not -name 'scripts' | sed 's|./||' | sort)

# Core packages that should be installed first
CORE_PACKAGES := stow bash git
# Optional packages
OPTIONAL_PACKAGES := $(filter-out $(CORE_PACKAGES),$(PACKAGES))

help: ## Show this help message
	@echo "Dotfiles Stow Management"
	@echo "======================="
	@echo "Available packages: $(PACKAGES)"
	@echo ""
	@echo "Targets:"
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  %-15s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

install: install-core install-optional ## Install all packages

install-core: ## Install core packages first
	@echo "Installing core packages: $(CORE_PACKAGES)"
	@for pkg in $(CORE_PACKAGES); do \
		if [ -d "$$pkg" ]; then \
			echo "Stowing $$pkg..."; \
			stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) $$pkg || exit 1; \
		fi; \
	done

install-optional: ## Install optional packages
	@echo "Installing optional packages: $(OPTIONAL_PACKAGES)"
	@for pkg in $(OPTIONAL_PACKAGES); do \
		if [ -d "$$pkg" ]; then \
			echo "Stowing $$pkg..."; \
			stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) $$pkg || true; \
		fi; \
	done

install-%: ## Install specific package (e.g., make install-bash)
	@pkg=$*; \
	if [ -d "$$pkg" ]; then \
		echo "Stowing $$pkg..."; \
		stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) $$pkg; \
	else \
		echo "Package $$pkg not found"; \
		exit 1; \
	fi

uninstall: ## Uninstall all packages
	@echo "Uninstalling all packages: $(PACKAGES)"
	@for pkg in $(PACKAGES); do \
		if [ -d "$$pkg" ]; then \
			echo "Unstowing $$pkg..."; \
			stow --delete --dir=$(STOW_DIR) --target=$(TARGET_DIR) $$pkg || true; \
		fi; \
	done

uninstall-%: ## Uninstall specific package (e.g., make uninstall-bash)
	@pkg=$*; \
	if [ -d "$$pkg" ]; then \
		echo "Unstowing $$pkg..."; \
		stow --delete --dir=$(STOW_DIR) --target=$(TARGET_DIR) $$pkg; \
	else \
		echo "Package $$pkg not found"; \
		exit 1; \
	fi

restow: ## Restow all packages (useful after updates)
	@echo "Restowing all packages: $(PACKAGES)"
	@for pkg in $(PACKAGES); do \
		if [ -d "$$pkg" ]; then \
			echo "Restowing $$pkg..."; \
			stow --restow --dir=$(STOW_DIR) --target=$(TARGET_DIR) $$pkg || true; \
		fi; \
	done

restow-%: ## Restow specific package (e.g., make restow-bash)
	@pkg=$*; \
	if [ -d "$$pkg" ]; then \
		echo "Restowing $$pkg..."; \
		stow --restow --dir=$(STOW_DIR) --target=$(TARGET_DIR) $$pkg; \
	else \
		echo "Package $$pkg not found"; \
		exit 1; \
	fi

status: ## Show status of all packages
	@echo "Package Status:"
	@echo "==============="
	@for pkg in $(PACKAGES); do \
		if [ -d "$$pkg" ]; then \
			echo -n "$$pkg: "; \
			if stow --simulate --dir=$(STOW_DIR) --target=$(TARGET_DIR) $$pkg >/dev/null 2>&1; then \
				echo "✓ Ready to stow"; \
			else \
				echo "⚠ Conflicts or already stowed"; \
			fi; \
		fi; \
	done

validate: ## Validate package structure
	@echo "Validating packages..."
	@for pkg in $(PACKAGES); do \
		if [ -d "$$pkg" ]; then \
			echo -n "Validating $$pkg: "; \
			if find "$$pkg" -name ".DS_Store" -o -name "Thumbs.db" | grep -q .; then \
				echo "⚠ Contains system files"; \
			elif find "$$pkg" -type l ! -exec test -e {} \; | grep -q .; then \
				echo "⚠ Contains broken symlinks"; \
			else \
				echo "✓ Valid"; \
			fi; \
		fi; \
	done

test: ## Test stow operations (dry run)
	@echo "Testing stow operations (dry run)..."
	@for pkg in $(PACKAGES); do \
		if [ -d "$$pkg" ]; then \
			echo "Testing $$pkg:"; \
			stow --simulate --verbose=2 --dir=$(STOW_DIR) --target=$(TARGET_DIR) $$pkg || true; \
			echo ""; \
		fi; \
	done

clean: ## Remove broken symlinks from target directory
	@echo "Cleaning broken symlinks in $(TARGET_DIR)..."
	@find $(TARGET_DIR) -maxdepth 3 -type l ! -exec test -e {} \; -print -delete 2>/dev/null || true
	@echo "Cleanup completed"

backup: ## Create backup of existing dotfiles
	@backup_dir="$(BACKUP_DIR)/backup-$$(date +%Y%m%d-%H%M%S)"; \
	echo "Creating backup at $$backup_dir..."; \
	mkdir -p "$$backup_dir"; \
	for pkg in $(PACKAGES); do \
		if [ -d "$$pkg" ]; then \
			find "$$pkg" -type f | while read -r file; do \
				rel_path="$${file#$$pkg/}"; \
				target_file="$(TARGET_DIR)/$$rel_path"; \
				if [ -e "$$target_file" ] && [ ! -L "$$target_file" ]; then \
					backup_file="$$backup_dir/$$rel_path"; \
					mkdir -p "$$(dirname "$$backup_file")"; \
					cp -p "$$target_file" "$$backup_file"; \
					echo "Backed up: $$target_file"; \
				fi; \
			done; \
		fi; \
	done; \
	echo "Backup completed at $$backup_dir"

list: ## List all available packages
	@echo "Available packages:"
	@for pkg in $(PACKAGES); do \
		echo "  $$pkg"; \
	done

# Platform-specific targets
install-macos: ## Install macOS-specific packages
	@echo "Installing macOS-specific packages..."
	@for pkg in iterm2 vscode; do \
		if [ -d "$$pkg" ]; then \
			echo "Stowing $$pkg..."; \
			stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) $$pkg || true; \
		fi; \
	done

install-linux: ## Install Linux-specific packages
	@echo "Installing Linux-specific packages..."
	@for pkg in i3 htop; do \
		if [ -d "$$pkg" ]; then \
			echo "Stowing $$pkg..."; \
			stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) $$pkg || true; \
		fi; \
	done

# Development targets
dev-install: backup install ## Development install with backup
	@echo "Development installation completed"

dev-test: validate test ## Run validation and tests
	@echo "Development tests completed"
