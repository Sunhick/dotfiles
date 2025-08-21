# Stow management package Makefile
PACKAGE_NAME := stow
PACKAGE_DIR := $(CURDIR)
STOW_DIR := $(shell dirname $(PACKAGE_DIR))
TARGET_DIR := $(HOME)

.PHONY: install uninstall status test clean

install: ## Install stow management configuration
	@echo "Installing $(PACKAGE_NAME) management..."
	@stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) --verbose $(PACKAGE_NAME) || { \
		echo "WARNING: Adopting existing $(PACKAGE_NAME) configuration..."; \
		stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) --adopt --verbose $(PACKAGE_NAME); \
	}

uninstall: ## Uninstall stow management configuration
	@echo "Removing $(PACKAGE_NAME) management..."
	@stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) --delete --verbose $(PACKAGE_NAME)

status: ## Show stow management status
	@echo "$(PACKAGE_NAME) management status:"
	@if stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) --simulate $(PACKAGE_NAME) >/dev/null 2>&1; then \
		echo "  READY Ready to install"; \
	else \
		echo "  OK Installed/conflicts"; \
	fi

test: ## Test stow management installation
	@echo "Testing $(PACKAGE_NAME) management..."
	@stow --dir=$(STOW_DIR) --target=$(TARGET_DIR) --simulate --verbose $(PACKAGE_NAME)

clean: ## Clean stow management symlinks
	@echo "Cleaning $(PACKAGE_NAME) management symlinks..."
	@find $(TARGET_DIR) -type l -lname "$(PACKAGE_DIR)/*" ! -exec test -e {} \; -delete 2>/dev/null || true
