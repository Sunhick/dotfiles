# Dotfiles Management Makefile
# Uses GNU Stow to create symlinks for configuration files

# Default target directory
TARGET_DIR := $(HOME)

# Package directories
PACKAGES_DIR := packages

# Define package categories and their packages
SHELL_PACKAGES := bash zsh
EDITOR_PACKAGES := emacs nano vscode
DEV_PACKAGES := git
DESKTOP_PACKAGES := i3 iterm2
TOOL_PACKAGES := htop tmux gnupg
MGMT_PACKAGES := stow

# All packages
ALL_PACKAGES := $(SHELL_PACKAGES) $(EDITOR_PACKAGES) $(DEV_PACKAGES) $(DESKTOP_PACKAGES) $(TOOL_PACKAGES) $(MGMT_PACKAGES)

# Stow command with verbose output
STOW := stow -v --target="$(TARGET_DIR)"

.PHONY: help install force-install uninstall restow clean check-stow $(ALL_PACKAGES) uninstall-bash uninstall-zsh uninstall-emacs uninstall-nano uninstall-git

# Default target
help:
	@echo "XDG-Compliant Dotfiles Management"
	@echo "=================================="
	@echo ""
	@echo "Available targets:"
	@echo "  install       - Install all packages (skip conflicts)"
	@echo "  force-install - Install all packages (overwrite existing files)"
	@echo "  uninstall     - Uninstall all packages"
	@echo "  restow        - Restow all packages (uninstall + install)"
	@echo "  clean         - Remove broken symlinks"
	@echo "  check-stow    - Check if GNU Stow is installed"
	@echo ""
	@echo "Individual packages:"
	@echo "  $(ALL_PACKAGES)"
	@echo ""
	@echo "Individual uninstall targets:"
	@echo "  uninstall-bash uninstall-zsh uninstall-emacs uninstall-nano uninstall-vscode uninstall-git"
	@echo ""
	@echo "Package categories:"
	@echo "  shell       - $(SHELL_PACKAGES)"
	@echo "  editors     - $(EDITOR_PACKAGES)"
	@echo "  development - $(DEV_PACKAGES)"
	@echo "  desktop     - $(DESKTOP_PACKAGES)"
	@echo "  tools       - $(TOOL_PACKAGES)"
	@echo "  management  - $(MGMT_PACKAGES)"

# Check if stow is installed
check-stow:
	@which stow > /dev/null || (echo "GNU Stow is not installed. Please install it first." && exit 1)

# Install all packages
install: check-stow
	@echo "Installing all dotfiles packages..."
	@cd $(PACKAGES_DIR) && for category in shell editors development desktop tools management; do \
		if [ -d "$$category" ]; then \
			cd "$$category" && \
			for package in */; do \
				if [ -d "$$package" ]; then \
					echo "Installing $$category/$$package..."; \
					$(STOW) "$$package" || echo "Warning: $$category/$$package had conflicts (use 'make force-install' to override)"; \
				fi; \
			done && \
			cd ..; \
		fi; \
	done
	@echo "Installation complete!"

# Force install all packages (overwrites existing files)
force-install: check-stow
	@echo "Force installing all dotfiles packages (this will overwrite existing files)..."
	@cd $(PACKAGES_DIR) && for category in shell editors development desktop tools management; do \
		if [ -d "$$category" ]; then \
			cd "$$category" && \
			for package in */; do \
				if [ -d "$$package" ]; then \
					echo "Force installing $$category/$$package..."; \
					$(STOW) --adopt "$$package"; \
				fi; \
			done && \
			cd ..; \
		fi; \
	done
	@echo "All packages force installed successfully!"

# Uninstall all packages
uninstall: check-stow
	@echo "Uninstalling all dotfiles packages..."
	@echo "Removing XDG configuration directories..."
	@if [ -L "$(HOME)/.config/bash" ]; then \
		echo "Removing ~/.config/bash symlink"; \
		rm "$(HOME)/.config/bash"; \
	fi
	@if [ -L "$(HOME)/.config/emacs" ]; then \
		echo "Removing ~/.config/emacs symlink"; \
		rm "$(HOME)/.config/emacs"; \
	fi
	@if [ -L "$(HOME)/.config/git" ]; then \
		echo "Removing ~/.config/git symlink"; \
		rm "$(HOME)/.config/git"; \
	fi
	@echo "Removing legacy dotfiles..."
	@if [ -L "$(HOME)/.bashrc" ]; then \
		echo "Removing ~/.bashrc symlink"; \
		rm "$(HOME)/.bashrc"; \
	fi
	@if [ -L "$(HOME)/.emacs.d" ]; then \
		echo "Removing ~/.emacs.d symlink"; \
		rm "$(HOME)/.emacs.d"; \
	fi
	@if [ -L "$(HOME)/.gitconfig" ]; then \
		echo "Removing ~/.gitconfig symlink"; \
		rm "$(HOME)/.gitconfig"; \
	fi
	@echo "Removing stow-managed packages..."
	@cd $(PACKAGES_DIR) && for category in shell editors development desktop tools management; do \
		if [ -d "$$category" ]; then \
			cd "$$category" && \
			for package in */; do \
				if [ -d "$$package" ]; then \
					echo "Uninstalling $$category/$$package..."; \
					$(STOW) -D "$$package" 2>/dev/null || true; \
				fi; \
			done && \
			cd ..; \
		fi; \
	done
	@echo "All packages uninstalled successfully!"

# Restow (uninstall + install)
restow: check-stow
	@echo "Restowing all packages..."
	@cd $(PACKAGES_DIR) && for category in shell editors development desktop tools management; do \
		if [ -d "$$category" ]; then \
			cd "$$category" && \
			for package in */; do \
				if [ -d "$$package" ]; then \
					echo "Restowing $$category/$$package..."; \
					$(STOW) -R "$$package"; \
				fi; \
			done && \
			cd ..; \
		fi; \
	done
	@echo "All packages restowed successfully!"

# Individual package targets
bash: check-stow
	@echo "Installing bash configuration..."
	@mkdir -p "$(HOME)/.config/bash"
	@cd $(PACKAGES_DIR)/shell && $(STOW) bash

uninstall-bash:
	@echo "Uninstalling bash configuration..."
	@if [ -L "$(HOME)/.config/bash" ]; then \
		rm "$(HOME)/.config/bash"; \
		echo "Removed ~/.config/bash symlink"; \
	fi
	@if [ -L "$(HOME)/.bashrc" ]; then \
		rm "$(HOME)/.bashrc"; \
		echo "Removed ~/.bashrc symlink"; \
	fi
	@cd $(PACKAGES_DIR)/shell && $(STOW) -D bash 2>/dev/null || true

zsh: check-stow
	@echo "Installing zsh configuration..."
	@mkdir -p "$(HOME)/.config/zsh"
	@cd $(PACKAGES_DIR)/shell && $(STOW) zsh

uninstall-zsh:
	@echo "Uninstalling zsh configuration..."
	@if [ -L "$(HOME)/.config/zsh" ]; then \
		rm "$(HOME)/.config/zsh"; \
		echo "Removed ~/.config/zsh symlink"; \
	fi
	@cd $(PACKAGES_DIR)/shell && $(STOW) -D zsh 2>/dev/null || true

emacs: check-stow
	@echo "Installing emacs configuration..."
	@mkdir -p "$(HOME)/.config"
	@cd $(PACKAGES_DIR)/editors && $(STOW) emacs

uninstall-emacs:
	@echo "Uninstalling emacs configuration..."
	@if [ -L "$(HOME)/.config/emacs" ]; then \
		rm "$(HOME)/.config/emacs"; \
		echo "Removed ~/.config/emacs symlink"; \
	fi
	@if [ -L "$(HOME)/.emacs.d" ]; then \
		rm "$(HOME)/.emacs.d"; \
		echo "Removed ~/.emacs.d symlink"; \
	fi
	@cd $(PACKAGES_DIR)/editors && $(STOW) -D emacs 2>/dev/null || true

nano: check-stow
	@echo "Installing nano configuration..."
	@cd $(PACKAGES_DIR)/editors && $(STOW) nano

uninstall-nano:
	@echo "Uninstalling nano configuration..."
	@cd $(PACKAGES_DIR)/editors && $(STOW) -D nano 2>/dev/null || true

vscode: check-stow
	@echo "Installing VSCode configuration..."
	@cd $(PACKAGES_DIR)/editors && $(STOW) vscode

uninstall-vscode:
	@echo "Uninstalling VSCode configuration..."
	@cd $(PACKAGES_DIR)/editors && $(STOW) -D vscode 2>/dev/null || true

git: check-stow
	@echo "Installing git configuration..."
	@mkdir -p "$(HOME)/.config/git"
	@cd $(PACKAGES_DIR)/development && $(STOW) git

uninstall-git:
	@echo "Uninstalling git configuration..."
	@if [ -L "$(HOME)/.config/git" ]; then \
		rm "$(HOME)/.config/git"; \
		echo "Removed ~/.config/git symlink"; \
	fi
	@if [ -L "$(HOME)/.gitconfig" ]; then \
		rm "$(HOME)/.gitconfig"; \
		echo "Removed ~/.gitconfig symlink"; \
	fi
	@cd $(PACKAGES_DIR)/development && $(STOW) -D git 2>/dev/null || true

i3: check-stow
	@echo "Installing i3 configuration..."
	@cd $(PACKAGES_DIR)/desktop && $(STOW) i3

iterm2: check-stow
	@echo "Installing iterm2 configuration..."
	@cd $(PACKAGES_DIR)/desktop && $(STOW) iterm2

htop: check-stow
	@echo "Installing htop configuration..."
	@cd $(PACKAGES_DIR)/tools && $(STOW) htop

tmux: check-stow
	@echo "Installing tmux configuration..."
	@cd $(PACKAGES_DIR)/tools && $(STOW) tmux

gnupg: check-stow
	@echo "Installing gnupg configuration..."
	@cd $(PACKAGES_DIR)/tools && $(STOW) gnupg

stow: check-stow
	@echo "Installing stow configuration..."
	@cd $(PACKAGES_DIR)/../management && $(STOW) stow

# Category targets
shell: bash zsh
editors: emacs nano vscode
development: git
desktop: i3 iterm2
tools: htop tmux gnupg
management: stow

# Clean broken symlinks
clean:
	@echo "Cleaning broken symlinks in $(TARGET_DIR)..."
	@find $(TARGET_DIR) -type l -exec test ! -e {} \; -print -delete 2>/dev/null || true
	@echo "Cleanup complete!"
