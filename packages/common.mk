# Common Makefile for dotfiles package categories
# This file contains shared functionality for all package categories
# Individual category Makefiles should set CATEGORY and include this file

# Validate that CATEGORY is set
ifndef CATEGORY
$(error CATEGORY must be set before including common.mk)
endif

# Common variables
CATEGORY_DIR := $(CURDIR)
STOW_DIR := $(CATEGORY_DIR)
TARGET_DIR := $(HOME)

# Discover packages in this category
PACKAGES := $(shell find . -maxdepth 1 -type d -mindepth 1 | sed 's|./||' | sort)

# Capitalize category name for display
CATEGORY_DISPLAY := $(shell echo "$(CATEGORY)" | awk '{print toupper(substr($$0,1,1)) tolower(substr($$0,2))}')

.PHONY: help install uninstall status test clean list

.DEFAULT_GOAL := help

help: ## Show help for packages in this category
	@echo "$(CATEGORY_DISPLAY) Packages Management"
	@echo "$(shell printf '=%.0s' $$(seq 1 $$(echo "$(CATEGORY_DISPLAY) Packages Management" | wc -c)))"
	@echo "Available packages: $(PACKAGES)"
	@echo ""
	@echo "Commands:"
	@echo "  make install    - Install all $(CATEGORY) packages"
	@echo "  make uninstall  - Remove all $(CATEGORY) packages"
	@echo "  make status     - Show status of all packages"
	@echo "  make test       - Test installation (dry run)"
	@echo "  make clean      - Clean broken symlinks"
	@echo "  make list       - List available packages"

install: ## Install all packages in this category
	@echo "Installing $(CATEGORY) packages: $(PACKAGES)"
	@for pkg in $(PACKAGES); do \
		echo "Installing $$pkg..."; \
		stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) --verbose $$pkg || { \
			echo "WARNING: Adopting existing $$pkg configuration..."; \
			stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) --adopt --verbose $$pkg; \
		}; \
	done

uninstall: ## Remove all packages in this category
	@echo "Removing $(CATEGORY) packages: $(PACKAGES)"
	@for pkg in $(PACKAGES); do \
		echo "Removing $$pkg..."; \
		stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) --delete --verbose $$pkg || true; \
	done

status: ## Show status of all packages in this category
	@echo "$(CATEGORY_DISPLAY) Packages Status"
	@echo "$(shell printf '=%.0s' $$(seq 1 $$(echo "$(CATEGORY_DISPLAY) Packages Status" | wc -c)))"
	@for pkg in $(PACKAGES); do \
		echo -n "$$pkg: "; \
		if stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) --simulate $$pkg >/dev/null 2>&1; then \
			echo "READY Ready to install"; \
		else \
			echo "OK Installed/conflicts"; \
		fi; \
	done

test: ## Test installation of all packages in this category
	@echo "Testing $(CATEGORY) packages installation..."
	@for pkg in $(PACKAGES); do \
		echo "Testing $$pkg:"; \
		stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) --simulate --verbose $$pkg || true; \
		echo ""; \
	done

clean: ## Clean broken symlinks for packages in this category
	@echo "Cleaning $(CATEGORY) package symlinks..."
	@for pkg in $(PACKAGES); do \
		find $(TARGET_DIR) -type l -lname "$(CATEGORY_DIR)/$$pkg/*" ! -exec test -e {} \; -delete 2>/dev/null || true; \
	done

list: ## List all packages in this category
	@echo "Available $(CATEGORY) packages:"
	@for pkg in $(PACKAGES); do \
		echo "  • $$pkg"; \
		if [ -f "$$pkg/.stow-dependencies" ]; then \
			echo "    Dependencies: $$(grep -v '^#' $$pkg/.stow-dependencies 2>/dev/null | tr '\n' ' ' || echo 'none')"; \
		fi; \
	done

# Individual package targets
install-%: ## Install specific package (e.g., make install-bash)
	@pkg=$*; \
	if [ -d "$$pkg" ]; then \
		echo "Installing $(CATEGORY) package: $$pkg"; \
		stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) --verbose $$pkg || { \
			echo "WARNING: Adopting existing $$pkg configuration..."; \
			stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) --adopt --verbose $$pkg; \
		}; \
	else \
		echo "ERROR: Package $$pkg not found in $(CATEGORY) category"; \
		exit 1; \
	fi

uninstall-%: ## Remove specific package (e.g., make uninstall-bash)
	@pkg=$*; \
	if [ -d "$$pkg" ]; then \
		echo "Removing $(CATEGORY) package: $$pkg"; \
		stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) --delete --verbose $$pkg; \
	else \
		echo "ERROR: Package $$pkg not found in $(CATEGORY) category"; \
		exit 1; \
	fi
