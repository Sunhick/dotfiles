# Dotfiles Management with Stow
# Main interface for package installation and management

.PHONY: help install uninstall status clean list deps check-stow test
.DEFAULT_GOAL := help

# Configuration
DOTFILES_ROOT := $(CURDIR)
TARGET_DIR := $(HOME)
BACKUP_DIR := $(HOME)/.dotfiles-backup

# Package categories
CATEGORIES := shell editors tools desktop development

help: ## Show help for dotfiles management
	@echo "Dotfiles Management"
	@echo "==================="
	@echo ""
	@echo "Main Commands:"
	@echo "  make install         - Install all packages"
	@echo "  make uninstall       - Remove all packages"
	@echo "  make status          - Show package status"
	@echo "  make test            - Test installation (dry run)"
	@echo "  make clean           - Remove broken symlinks"
	@echo "  make list            - List all packages"
	@echo "  make deps            - Check dependencies"
	@echo ""
	@echo "Category Commands:"
	@echo "  make install-shell   - Install shell packages"
	@echo "  make install-editors - Install editor packages"
	@echo "  make install-tools   - Install tool packages"
	@echo "  make install-desktop - Install desktop packages"
	@echo "  make install-dev     - Install development packages"
	@echo ""
	@echo "Individual Package Commands:"
	@echo "  make install-bash    - Install specific package"
	@echo "  make uninstall-bash  - Remove specific package"
	@echo ""
	@echo "Utility Commands:"
	@echo "  make backup          - Create backup of existing files"

check-stow: ## Check if stow is installed
	@command -v stow >/dev/null 2>&1 || { \
		echo "ERROR: stow is not installed. Please install it first:"; \
		echo "  macOS: brew install stow"; \
		echo "  Ubuntu/Debian: sudo apt install stow"; \
		echo "  CentOS/RHEL: sudo yum install stow"; \
		exit 1; \
	}

deps: check-stow ## Check all dependencies
	@echo "Checking dependencies..."
	@echo "✓ stow: $(shell command -v stow)"
	@for category in $(CATEGORIES); do \
		if [ -d "packages/$$category" ]; then \
			echo "Checking $$category dependencies:"; \
			$(MAKE) -C packages/$$category deps 2>/dev/null || true; \
		fi; \
	done

install: check-stow install-mgmt install-categories ## Install all packages
	@echo "✓ All packages installed successfully!"
	@echo "Run 'make status' to verify installation"

install-mgmt: ## Install management configuration (stow)
	@echo "Installing management configuration..."
	@if [ -d "management/stow" ]; then \
		stow --dir=management --target=$(TARGET_DIR) --verbose stow || { \
			echo "WARNING: Adopting existing stow configuration..."; \
			stow --dir=management --target=$(TARGET_DIR) --adopt --verbose stow; \
		}; \
	fi

install-categories: $(addprefix install-,$(CATEGORIES)) ## Install all categories

install-shell: ## Install shell packages
	@$(MAKE) -C packages/shell install

install-editors: ## Install editor packages
	@$(MAKE) -C packages/editors install

install-tools: ## Install tool packages
	@$(MAKE) -C packages/tools install

install-desktop: ## Install desktop packages
	@$(MAKE) -C packages/desktop install

install-dev: ## Install development packages
	@$(MAKE) -C packages/development install

uninstall: ## Remove all packages
	@echo "Uninstalling all packages..."
	@for category in $(CATEGORIES); do \
		if [ -d "packages/$$category" ]; then \
			echo "Removing $$category packages..."; \
			$(MAKE) -C packages/$$category uninstall || true; \
		fi; \
	done
	@if [ -d "management/stow" ]; then \
		echo "Removing management configuration..."; \
		stow --dir=management --target=$(TARGET_DIR) --delete --verbose stow || true; \
	fi
	@echo "✓ All packages uninstalled"

status: ## Show installation status of all packages
	@echo "Package Installation Status"
	@echo "==========================="
	@for category in $(CATEGORIES); do \
		if [ -d "packages/$$category" ]; then \
			echo ""; \
			$(MAKE) -C packages/$$category status; \
		fi; \
	done

list: ## List all available packages
	@echo "Available Packages by Category"
	@echo "=============================="
	@for category in $(CATEGORIES); do \
		if [ -d "packages/$$category" ]; then \
			echo ""; \
			$(MAKE) -C packages/$$category list; \
		fi; \
	done

test: check-stow ## Test installation (dry run)
	@echo "Testing package installation (dry run)..."
	@for category in $(CATEGORIES); do \
		if [ -d "packages/$$category" ]; then \
			echo ""; \
			echo "Testing $$category packages:"; \
			$(MAKE) -C packages/$$category test; \
		fi; \
	done

clean: ## Remove broken symlinks from home directory
	@echo "Cleaning broken symlinks..."
	@for category in $(CATEGORIES); do \
		if [ -d "packages/$$category" ]; then \
			$(MAKE) -C packages/$$category clean || true; \
		fi; \
	done
	@echo "✓ Cleanup completed"

backup: ## Create backup of existing dotfiles
	@backup_dir="$(BACKUP_DIR)/backup-$$(date +%Y%m%d-%H%M%S)"; \
	echo "Creating backup at $$backup_dir..."; \
	mkdir -p "$$backup_dir"; \
	for category in $(CATEGORIES); do \
		if [ -d "packages/$$category" ]; then \
			for pkg in packages/$$category/*/; do \
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
		fi; \
	done; \
	echo "✓ Backup completed at $$backup_dir"

# Individual package installation targets
install-%: check-stow ## Install specific package (e.g., make install-bash)
	@pkg=$*; \
	found=false; \
	for category in $(CATEGORIES); do \
		if [ -d "packages/$$category/$$pkg" ]; then \
			echo "Installing $$pkg from $$category category..."; \
			$(MAKE) -C packages/$$category install-$$pkg; \
			found=true; \
			break; \
		fi; \
	done; \
	if [ "$$found" = false ]; then \
		echo "ERROR: Package $$pkg not found in any category"; \
		echo "Available packages:"; \
		$(MAKE) list; \
		exit 1; \
	fi

uninstall-%: check-stow ## Uninstall specific package (e.g., make uninstall-bash)
	@pkg=$*; \
	found=false; \
	for category in $(CATEGORIES); do \
		if [ -d "packages/$$category/$$pkg" ]; then \
			echo "Removing $$pkg from $$category category..."; \
			$(MAKE) -C packages/$$category uninstall-$$pkg; \
			found=true; \
			break; \
		fi; \
	done; \
	if [ "$$found" = false ]; then \
		echo "ERROR: Package $$pkg not found in any category"; \
		echo "Available packages:"; \
		$(MAKE) list; \
		exit 1; \
	fi
