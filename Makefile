# Reorganized Dotfiles Management with Stow
# Main interface for package installation and management

.PHONY: help install uninstall status clean list deps check-stow
.DEFAULT_GOAL := help

# Configuration
DOTFILES_ROOT := $(CURDIR)
TARGET_DIR := $(HOME)
BACKUP_DIR := $(HOME)/.dotfiles-backup

# Package discovery
SHELL_PACKAGES := $(shell find packages/shell -maxdepth 1 -type d -mindepth 1 2>/dev/null | xargs -I {} basename {} | sort)
EDITOR_PACKAGES := $(shell find packages/editors -maxdepth 1 -type d -mindepth 1 2>/dev/null | xargs -I {} basename {} | sort)
TOOL_PACKAGES := $(shell find packages/tools -maxdepth 1 -type d -mindepth 1 2>/dev/null | xargs -I {} basename {} | sort)
DESKTOP_PACKAGES := $(shell find packages/desktop -maxdepth 1 -type d -mindepth 1 2>/dev/null | xargs -I {} basename {} | sort)
DEV_PACKAGES := $(shell find packages/development -maxdepth 1 -type d -mindepth 1 2>/dev/null | xargs -I {} basename {} | sort)

# Core packages (installed first)
CORE_PACKAGES := bash git

help: ## Show help for reorganized dotfiles
	@echo "Reorganized Dotfiles Management"
	@echo "==============================="
	@echo ""
	@echo "Package Structure:"
	@echo "  Shell:      $(SHELL_PACKAGES)"
	@echo "  Editors:    $(EDITOR_PACKAGES)"
	@echo "  Tools:      $(TOOL_PACKAGES)"
	@echo "  Desktop:    $(DESKTOP_PACKAGES)"
	@echo "  Development: $(DEV_PACKAGES)"
	@echo ""
	@echo "Main Commands:"
	@echo "  make install         - Install all packages"
	@echo "  make uninstall       - Remove all packages"
	@echo "  make status          - Show package status"
	@echo "  make deps            - Check dependencies"
	@echo ""
	@echo "Category Commands:"
	@echo "  make install-shell   - Install shell packages"
	@echo "  make install-editors - Install editor packages"
	@echo "  make install-tools   - Install tool packages"
	@echo "  make install-desktop - Install desktop packages"
	@echo "  make install-dev     - Install development packages"
	@echo ""
	@echo "Utility Commands:"
	@echo "  make clean          - Remove broken symlinks"
	@echo "  make backup         - Create backup of existing files"
	@echo "  make list           - List all packages"
	@echo "  make test           - Test installation (dry run)"

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
	@echo "OK: stow: $(shell command -v stow)"
	@for category in packages/*/; do \
		for pkg in "$$category"*/; do \
			if [ -f "$$pkg/.stow-dependencies" ]; then \
				echo "Checking $$pkg dependencies:";\
				while IFS= read -r dep; do \
					[[ "$$dep" =~ ^#.*$$ ]] && continue; \
					[ -z "$$dep" ] && continue; \
					if command -v "$$dep" >/dev/null 2>&1; then \
						echo "  OK: $$dep"; \
					else \
						echo "  MISSING: $$dep"; \
					fi; \
				done < "$$pkg/.stow-dependencies"; \
			fi; \
		done; \
	done

install: check-stow install-mgmt install-core install-categories ## Install all packages
	@echo "All packages installed successfully!"
	@echo "Run 'make status' to verify installation"

install-mgmt: ## Install management configuration (stow)
	@echo "Installing management configuration..."
	@if [ -d "management/stow" ]; then \
		stow --dir=management --target=$(TARGET_DIR) --verbose stow || { \
			echo "WARNING: Adopting existing stow configuration..."; \
			stow --dir=management --target=$(TARGET_DIR) --adopt --verbose stow; \
		}; \
	fi

install-core: ## Install core packages first
	@echo "Installing core packages: $(CORE_PACKAGES)"
	@for pkg in $(CORE_PACKAGES); do \
		if [ -d "packages/shell/$$pkg" ]; then \
			echo "Installing $$pkg..."; \
			stow --dir=packages/shell --target=$(TARGET_DIR) --verbose $$pkg || { \
				echo "WARNING: Adopting existing $$pkg configuration..."; \
				stow --dir=packages/shell --target=$(TARGET_DIR) --adopt --verbose $$pkg; \
			}; \
		fi; \
	done

install-categories: install-shell install-editors install-tools install-desktop install-dev ## Install all categories

install-shell: ## Install shell packages
	@echo "Installing shell packages: $(SHELL_PACKAGES)"
	@for pkg in $(SHELL_PACKAGES); do \
		echo "Installing $$pkg..."; \
		stow --dir=packages/shell --target=$(TARGET_DIR) --verbose $$pkg || true; \
	done

install-editors: ## Install editor packages
	@echo "Installing editor packages: $(EDITOR_PACKAGES)"
	@for pkg in $(EDITOR_PACKAGES); do \
		echo "Installing $$pkg..."; \
		stow --dir=packages/editors --target=$(TARGET_DIR) --verbose $$pkg || true; \
	done

install-tools: ## Install tool packages
	@echo "Installing tool packages: $(TOOL_PACKAGES)"
	@for pkg in $(TOOL_PACKAGES); do \
		echo "Installing $$pkg..."; \
		stow --dir=packages/tools --target=$(TARGET_DIR) --verbose $$pkg || true; \
	done

install-desktop: ## Install desktop packages
	@echo "Installing desktop packages: $(DESKTOP_PACKAGES)"
	@for pkg in $(DESKTOP_PACKAGES); do \
		echo "Installing $$pkg..."; \
		stow --dir=packages/desktop --target=$(TARGET_DIR) --verbose $$pkg || true; \
	done

install-dev: ## Install development packages
	@echo "Installing development packages: $(DEV_PACKAGES)"
	@for pkg in $(DEV_PACKAGES); do \
		echo "Installing $$pkg..."; \
		stow --dir=packages/development --target=$(TARGET_DIR) --verbose $$pkg || true; \
	done

uninstall: ## Remove all packages
	@echo "Uninstalling all packages..."
	@for pkg in $(SHELL_PACKAGES); do \
		echo "Removing $$pkg..."; \
		stow --dir=packages/shell --target=$(TARGET_DIR) --delete --verbose $$pkg || true; \
	done
	@for pkg in $(EDITOR_PACKAGES); do \
		echo "Removing $$pkg..."; \
		stow --dir=packages/editors --target=$(TARGET_DIR) --delete --verbose $$pkg || true; \
	done
	@for pkg in $(TOOL_PACKAGES); do \
		echo "Removing $$pkg..."; \
		stow --dir=packages/tools --target=$(TARGET_DIR) --delete --verbose $$pkg || true; \
	done
	@for pkg in $(DESKTOP_PACKAGES); do \
		echo "Removing $$pkg..."; \
		stow --dir=packages/desktop --target=$(TARGET_DIR) --delete --verbose $$pkg || true; \
	done
	@for pkg in $(DEV_PACKAGES); do \
		echo "Removing $$pkg..."; \
		stow --dir=packages/development --target=$(TARGET_DIR) --delete --verbose $$pkg || true; \
	done
	@if [ -d "management/stow" ]; then \
		echo "Removing management configuration..."; \
		stow --dir=management --target=$(TARGET_DIR) --delete --verbose stow || true; \
	fi
	@echo "All packages uninstalled"

status: ## Show installation status of all packages
	@echo "Package Installation Status"
	@echo "==========================="
	@echo ""
	@echo "SHELL PACKAGES:"
	@for pkg in $(SHELL_PACKAGES); do \
		if stow --dir=packages/shell --target=$(TARGET_DIR) --simulate $$pkg >/dev/null 2>&1; then \
			echo "  READY: $$pkg (ready to install)"; \
		else \
			echo "  INSTALLED: $$pkg (installed/conflicts)"; \
		fi; \
	done
	@echo ""
	@echo "EDITOR PACKAGES:"
	@for pkg in $(EDITOR_PACKAGES); do \
		if stow --dir=packages/editors --target=$(TARGET_DIR) --simulate $$pkg >/dev/null 2>&1; then \
			echo "  READY: $$pkg (ready to install)"; \
		else \
			echo "  INSTALLED: $$pkg (installed/conflicts)"; \
		fi; \
	done
	@echo ""
	@echo "TOOL PACKAGES:"
	@for pkg in $(TOOL_PACKAGES); do \
		if stow --dir=packages/tools --target=$(TARGET_DIR) --simulate $$pkg >/dev/null 2>&1; then \
			echo "  READY: $$pkg (ready to install)"; \
		else \
			echo "  INSTALLED: $$pkg (installed/conflicts)"; \
		fi; \
	done

list: ## List all available packages
	@echo "Available Packages by Category"
	@echo "=============================="
	@echo ""
	@echo "Shell ($(words $(SHELL_PACKAGES))):"
	@for pkg in $(SHELL_PACKAGES); do echo "  • $$pkg"; done
	@echo ""
	@echo "Editors ($(words $(EDITOR_PACKAGES))):"
	@for pkg in $(EDITOR_PACKAGES); do echo "  • $$pkg"; done
	@echo ""
	@echo "Tools ($(words $(TOOL_PACKAGES))):"
	@for pkg in $(TOOL_PACKAGES); do echo "  • $$pkg"; done
	@echo ""
	@echo "Desktop ($(words $(DESKTOP_PACKAGES))):"
	@for pkg in $(DESKTOP_PACKAGES); do echo "  • $$pkg"; done
	@echo ""
	@echo "Development ($(words $(DEV_PACKAGES))):"
	@for pkg in $(DEV_PACKAGES); do echo "  • $$pkg"; done

test: check-stow ## Test installation (dry run)
	@echo "Testing package installation (dry run)..."
	@echo ""
	@for category in shell editors tools desktop development; do \
		echo "Testing $$category packages:"; \
		for pkg in packages/$$category/*/; do \
			if [ -d "$$pkg" ]; then \
				pkg_name=$$(basename "$$pkg"); \
				echo -n "  $$pkg_name: "; \
				if stow --dir=packages/$$category --target=$(TARGET_DIR) --simulate --verbose $$pkg_name >/dev/null 2>&1; then \
					echo "OK"; \
				else \
					echo "CONFLICTS"; \
				fi; \
			fi; \
		done; \
		echo ""; \
	done

clean: ## Remove broken symlinks from home directory
	@echo "Cleaning broken symlinks in $(TARGET_DIR)..."
	@find $(TARGET_DIR) -maxdepth 3 -type l ! -exec test -e {} \; -print -delete 2>/dev/null || true
	@echo "Cleanup completed"

backup: ## Create backup of existing dotfiles
	@backup_dir="$(BACKUP_DIR)/backup-$$(date +%Y%m%d-%H%M%S)"; \
	echo "Creating backup at $$backup_dir..."; \
	mkdir -p "$$backup_dir"; \
	for category in packages/*/; do \
		for pkg in "$$category"*/; do \
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
	done; \
	echo "Backup completed at $$backup_dir"

# Individual package installation targets
install-%: check-stow ## Install specific package (e.g., make install-bash)
	@pkg=$*; \
	found=false; \
	for category in shell editors tools desktop development; do \
		if [ -d "packages/$$category/$$pkg" ]; then \
			echo "Installing $$pkg from $$category category...";\
			stow --dir=packages/$$category --target=$(TARGET_DIR) --verbose $$pkg || { \
				echo "WARNING: Adopting existing $$pkg configuration..."; \
				stow --dir=packages/$$category --target=$(TARGET_DIR) --adopt --verbose $$pkg; \
			}; \
			found=true; \
			break; \
		fi; \
	done; \
	if [ "$$found" = false ]; then \
		echo "ERROR: Package $$pkg not found in any category"; \
		exit 1; \
	fi

uninstall-%: check-stow ## Uninstall specific package (e.g., make uninstall-bash)
	@pkg=$*; \
	found=false; \
	for category in shell editors tools desktop development; do \
		if [ -d "packages/$$category/$$pkg" ]; then \
			echo "Removing $$pkg from $$category category..."; \
			stow --dir=packages/$$category --target=$(TARGET_DIR) --delete --verbose $$pkg; \
			found=true; \
			break; \
		fi; \
	done; \
	if [ "$$found" = false ]; then \
		echo "ERROR: Package $$pkg not found in any category"; \
		exit 1; \
	fi
