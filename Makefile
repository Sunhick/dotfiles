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
DESKTOP_PACKAGES := i3 terminal
TOOL_PACKAGES := htop tmux gnupg curlrc
MGMT_PACKAGES := stow

# All packages
ALL_PACKAGES := $(SHELL_PACKAGES) $(EDITOR_PACKAGES) $(DEV_PACKAGES) $(DESKTOP_PACKAGES) $(TOOL_PACKAGES) $(MGMT_PACKAGES)

# Stow command with minimal output
STOW := stow --target="$(TARGET_DIR)"
STOW_DRY_RUN := stow --target="$(TARGET_DIR)" --simulate

# Backup directory with timestamp
BACKUP_DIR := $(HOME)/.dotfiles-backup/$(shell date +%Y%m%d_%H%M%S)
LATEST_BACKUP := $(HOME)/.dotfiles-backup/latest

.PHONY: help install force-install uninstall restow clean-backups clean-all-backups check-stow backup restore list-backups install-safe dry-run $(ALL_PACKAGES) uninstall-bash uninstall-zsh uninstall-emacs uninstall-nano uninstall-git

# Default target
help:
	@echo "XDG-Compliant Dotfiles Management"
	@echo "=================================="
	@echo ""
	@echo "Available targets:"
	@echo "  install       - Install all packages (skip conflicts)"
	@echo "  install-safe  - Backup existing configs then install"
	@echo "  force-install - Install all packages (overwrite existing files)"
	@echo "  uninstall     - Uninstall all packages"
	@echo "  restow        - Restow all packages (uninstall + install)"
	@echo "  check-stow    - Check if GNU Stow is installed"
	@echo "  dry-run       - Preview install operations (no changes made)"
	@echo ""
	@echo "Backup targets:"
	@echo "  backup        - Backup existing configuration files"
	@echo "  restore       - Restore from latest backup"
	@echo "  list-backups  - List available backups"
	@echo ""
	@echo "Cleanup targets:"
	@echo "  clean-backups - Remove old backups (keep last 5)"
	@echo "  clean-all-backups - Remove ALL backups (dangerous!)"
	@echo ""
	@echo "Individual packages:"
	@echo "  $(ALL_PACKAGES)"
	@echo ""
	@echo "Individual uninstall targets:"
	@echo "  uninstall-bash uninstall-zsh uninstall-emacs uninstall-nano uninstall-vscode uninstall-git uninstall-curlrc"
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

# Backup existing configuration files
backup:
	@echo "Creating backup at $(BACKUP_DIR)..."
	@mkdir -p "$(BACKUP_DIR)"
	@echo "Backing up existing configuration files..."
	@# Backup shell configs
	@if [ -f "$(HOME)/.bashrc" ]; then cp "$(HOME)/.bashrc" "$(BACKUP_DIR)/"; echo "  BACKUP: .bashrc"; fi
	@if [ -f "$(HOME)/.zshrc" ]; then cp "$(HOME)/.zshrc" "$(BACKUP_DIR)/"; echo "  BACKUP: .zshrc"; fi
	@if [ -f "$(HOME)/.zshenv" ]; then cp "$(HOME)/.zshenv" "$(BACKUP_DIR)/"; echo "  BACKUP: .zshenv"; fi
	@# Backup XDG config directories
	@if [ -d "$(HOME)/.config/bash" ]; then cp -r "$(HOME)/.config/bash" "$(BACKUP_DIR)/"; echo "  BACKUP: .config/bash/"; fi
	@if [ -d "$(HOME)/.config/zsh" ]; then cp -r "$(HOME)/.config/zsh" "$(BACKUP_DIR)/"; echo "  BACKUP: .config/zsh/"; fi
	@if [ -d "$(HOME)/.config/git" ]; then cp -r "$(HOME)/.config/git" "$(BACKUP_DIR)/"; echo "  BACKUP: .config/git/"; fi
	@if [ -d "$(HOME)/.config/emacs" ]; then cp -r "$(HOME)/.config/emacs" "$(BACKUP_DIR)/"; echo "  BACKUP: .config/emacs/"; fi
	@if [ -d "$(HOME)/.config/terminal" ]; then cp -r "$(HOME)/.config/terminal" "$(BACKUP_DIR)/"; echo "  BACKUP: .config/terminal/"; fi
	@if [ -d "$(HOME)/.config/tmux" ]; then cp -r "$(HOME)/.config/tmux" "$(BACKUP_DIR)/"; echo "  BACKUP: .config/tmux/"; fi
	@if [ -d "$(HOME)/.config/htop" ]; then cp -r "$(HOME)/.config/htop" "$(BACKUP_DIR)/"; echo "  BACKUP: .config/htop/"; fi
	@if [ -d "$(HOME)/.config/Code" ]; then cp -r "$(HOME)/.config/Code" "$(BACKUP_DIR)/"; echo "  BACKUP: .config/Code/"; fi
	@# Backup legacy configs
	@if [ -f "$(HOME)/.gitconfig" ]; then cp "$(HOME)/.gitconfig" "$(BACKUP_DIR)/"; echo "  BACKUP: .gitconfig"; fi
	@if [ -d "$(HOME)/.emacs.d" ]; then cp -r "$(HOME)/.emacs.d" "$(BACKUP_DIR)/"; echo "  BACKUP: .emacs.d/"; fi
	@if [ -d "$(HOME)/.gnupg" ]; then cp -r "$(HOME)/.gnupg" "$(BACKUP_DIR)/"; echo "  BACKUP: .gnupg/"; fi
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
	@# Restore files
	@if [ -f "$(LATEST_BACKUP)/.bashrc" ]; then cp "$(LATEST_BACKUP)/.bashrc" "$(HOME)/"; echo "  RESTORE: .bashrc"; fi
	@if [ -f "$(LATEST_BACKUP)/.zshrc" ]; then cp "$(LATEST_BACKUP)/.zshrc" "$(HOME)/"; echo "  RESTORE: .zshrc"; fi
	@if [ -f "$(LATEST_BACKUP)/.zshenv" ]; then cp "$(LATEST_BACKUP)/.zshenv" "$(HOME)/"; echo "  RESTORE: .zshenv"; fi
	@if [ -f "$(LATEST_BACKUP)/.gitconfig" ]; then cp "$(LATEST_BACKUP)/.gitconfig" "$(HOME)/"; echo "  RESTORE: .gitconfig"; fi
	@# Restore directories
	@if [ -d "$(LATEST_BACKUP)/bash" ]; then cp -r "$(LATEST_BACKUP)/bash" "$(HOME)/.config/"; echo "  RESTORE: .config/bash/"; fi
	@if [ -d "$(LATEST_BACKUP)/zsh" ]; then cp -r "$(LATEST_BACKUP)/zsh" "$(HOME)/.config/"; echo "  RESTORE: .config/zsh/"; fi
	@if [ -d "$(LATEST_BACKUP)/git" ]; then cp -r "$(LATEST_BACKUP)/git" "$(HOME)/.config/"; echo "  RESTORE: .config/git/"; fi
	@if [ -d "$(LATEST_BACKUP)/emacs" ]; then cp -r "$(LATEST_BACKUP)/emacs" "$(HOME)/.config/"; echo "  RESTORE: .config/emacs/"; fi
	@if [ -d "$(LATEST_BACKUP)/terminal" ]; then cp -r "$(LATEST_BACKUP)/terminal" "$(HOME)/.config/"; echo "  RESTORE: .config/terminal/"; fi
	@if [ -d "$(LATEST_BACKUP)/.emacs.d" ]; then cp -r "$(LATEST_BACKUP)/.emacs.d" "$(HOME)/"; echo "  RESTORE: .emacs.d/"; fi
	@if [ -d "$(LATEST_BACKUP)/.gnupg" ]; then cp -r "$(LATEST_BACKUP)/.gnupg" "$(HOME)/"; echo "  RESTORE: .gnupg/"; fi
	@if [ -d "$(LATEST_BACKUP)/Code" ]; then cp -r "$(LATEST_BACKUP)/Code" "$(HOME)/.config/"; echo "  RESTORE: .config/Code/"; fi
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

# Preview install operations without making changes
dry-run: check-stow
	@cd $(PACKAGES_DIR) && for category in shell editors development desktop tools management; do \
		if [ -d "$$category" ]; then \
			cd "$$category" && \
			for package in */; do \
				if [ -d "$$package" ]; then \
					package_name=$${package%/}; \
					if $(STOW_DRY_RUN) "$$package" 2>/dev/null; then \
						echo "  WOULD LINK: $$package_name"; \
					else \
						echo "  WOULD SKIP: $$package_name (conflicts)"; \
					fi; \
				fi; \
			done && \
			cd ..; \
		fi; \
	done

# Safe install - backup first, then install
install-safe: backup install

# Install all packages
install: check-stow

	@cd $(PACKAGES_DIR) && for category in shell editors development desktop tools management; do \
		if [ -d "$$category" ]; then \
			cd "$$category" && \
			for package in */; do \
				if [ -d "$$package" ]; then \
					package_name=$${package%/}; \
					if $(STOW) "$$package" 2>/dev/null; then \
						echo "  LINK: $$package_name"; \
					else \
						echo "  SKIP: $$package_name (conflicts)"; \
					fi; \
				fi; \
			done && \
			cd ..; \
		fi; \
	done


# Force install all packages (overwrites existing files)
force-install: check-stow

	@cd $(PACKAGES_DIR) && for category in shell editors development desktop tools management; do \
		if [ -d "$$category" ]; then \
			cd "$$category" && \
			for package in */; do \
				if [ -d "$$package" ]; then \
					package_name=$${package%/}; \
					$(STOW) --adopt "$$package" 2>/dev/null; \
					echo "  LINK: $$package_name (forced)"; \
				fi; \
			done && \
			cd ..; \
		fi; \
	done
	@echo "All packages force installed successfully!"

# Uninstall all packages
uninstall: check-stow
	@if [ -L "$(HOME)/.config/bash" ]; then \
		echo "  UNLINK: .config/bash"; \
		rm "$(HOME)/.config/bash"; \
	fi
	@if [ -L "$(HOME)/.config/emacs" ]; then \
		echo "  UNLINK: .config/emacs"; \
		rm "$(HOME)/.config/emacs"; \
	fi
	@if [ -L "$(HOME)/.config/git" ]; then \
		echo "  UNLINK: .config/git"; \
		rm "$(HOME)/.config/git"; \
	fi
	@if [ -L "$(HOME)/.bashrc" ]; then \
		echo "  UNLINK: .bashrc"; \
		rm "$(HOME)/.bashrc"; \
	fi
	@if [ -L "$(HOME)/.emacs.d" ]; then \
		echo "  UNLINK: .emacs.d"; \
		rm "$(HOME)/.emacs.d"; \
	fi
	@if [ -L "$(HOME)/.gitconfig" ]; then \
		echo "  UNLINK: .gitconfig"; \
		rm "$(HOME)/.gitconfig"; \
	fi
	@if [ -L "$(HOME)/.config/Code" ]; then \
		echo "  UNLINK: .config/Code"; \
		rm "$(HOME)/.config/Code"; \
	fi
	@if [ -L "$(HOME)/.config/.curlrc" ]; then \
		echo "  UNLINK: .config/.curlrc"; \
		rm "$(HOME)/.config/.curlrc"; \
	fi
	@cd $(PACKAGES_DIR) && for category in shell editors development desktop tools management; do \
		if [ -d "$$category" ]; then \
			cd "$$category" && \
			for package in */; do \
				if [ -d "$$package" ]; then \
					package_name=$${package%/}; \
					$(STOW) -D "$$package" 2>/dev/null || true; \
					echo "  UNLINK: $$package_name"; \
				fi; \
			done && \
			cd ..; \
		fi; \
	done


# Restow (uninstall + install)
restow: check-stow
	@cd $(PACKAGES_DIR) && for category in shell editors development desktop tools management; do \
		if [ -d "$$category" ]; then \
			cd "$$category" && \
			for package in */; do \
				if [ -d "$$package" ]; then \
					package_name=$${package%/}; \
					if $(STOW) -R "$$package" 2>/dev/null; then \
						echo "  RESTOW: $$package_name"; \
					else \
						echo "  SKIP: $$package_name (conflicts)"; \
					fi; \
				fi; \
			done && \
			cd ..; \
		fi; \
	done

# Individual package targets
bash: check-stow
	@mkdir -p "$(HOME)/.config/bash"
	@cd $(PACKAGES_DIR)/shell && $(STOW) bash && echo "  LINK: bash"

uninstall-bash:
	@if [ -L "$(HOME)/.config/bash" ]; then \
		rm "$(HOME)/.config/bash"; \
		echo "  UNLINK: .config/bash"; \
	fi
	@if [ -L "$(HOME)/.bashrc" ]; then \
		rm "$(HOME)/.bashrc"; \
		echo "  UNLINK: .bashrc"; \
	fi
	@cd $(PACKAGES_DIR)/shell && $(STOW) -D bash 2>/dev/null && echo "  UNLINK: bash" || true

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
	@mkdir -p "$(HOME)/.config"
	@cd $(PACKAGES_DIR)/editors && $(STOW) emacs && echo "  LINK: emacs"

uninstall-emacs:
	@if [ -L "$(HOME)/.config/emacs" ]; then \
		rm "$(HOME)/.config/emacs"; \
		echo "  UNLINK: .config/emacs"; \
	fi
	@if [ -L "$(HOME)/.emacs.d" ]; then \
		rm "$(HOME)/.emacs.d"; \
		echo "  UNLINK: .emacs.d"; \
	fi
	@cd $(PACKAGES_DIR)/editors && $(STOW) -D emacs 2>/dev/null && echo "  UNLINK: emacs" || true

nano: check-stow
	@echo "Installing nano configuration..."
	@cd $(PACKAGES_DIR)/editors && $(STOW) nano

uninstall-nano:
	@echo "Uninstalling nano configuration..."
	@cd $(PACKAGES_DIR)/editors && $(STOW) -D nano 2>/dev/null || true

vscode: check-stow
	@echo "Installing VSCode configuration..."
	@mkdir -p "$(HOME)/.config/Code/User"
	@cd $(PACKAGES_DIR)/editors && $(STOW) vscode && echo "  LINK: vscode (XDG compliant)"

uninstall-vscode:
	@echo "Uninstalling VSCode configuration..."
	@if [ -L "$(HOME)/.config/Code" ]; then \
		rm "$(HOME)/.config/Code"; \
		echo "  UNLINK: .config/Code"; \
	fi
	@cd $(PACKAGES_DIR)/editors && $(STOW) -D vscode 2>/dev/null || true

git: check-stow
	@mkdir -p "$(HOME)/.config/git"
	@cd $(PACKAGES_DIR)/development && $(STOW) git && echo "  LINK: git"

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

uninstall-terminal:
	@echo "Uninstalling terminal configuration..."
	@if [ -L "$(HOME)/.config/terminal" ]; then \
		rm "$(HOME)/.config/terminal"; \
		echo "Removed ~/.config/terminal symlink"; \
	fi
	@cd $(PACKAGES_DIR)/desktop && $(STOW) -D terminal 2>/dev/null || true

uninstall-curlrc:
	@echo "Uninstalling curlrc configuration..."
	@if [ -L "$(HOME)/.config/.curlrc" ]; then \
		rm "$(HOME)/.config/.curlrc"; \
		echo "  UNLINK: .config/.curlrc"; \
	fi
	@if [ -L "$(HOME)/.config/curl" ]; then \
		rm "$(HOME)/.config/curl"; \
		echo "  UNLINK: .config/curl"; \
	fi
	@cd $(PACKAGES_DIR)/tools && $(STOW) -D curlrc 2>/dev/null || true

i3: check-stow
	@echo "Installing i3 configuration..."
	@cd $(PACKAGES_DIR)/desktop && $(STOW) i3

terminal: check-stow
	@echo "Installing terminal configuration..."
	@mkdir -p "$(HOME)/.config/terminal"
	@cd $(PACKAGES_DIR)/desktop && $(STOW) terminal

htop: check-stow
	@echo "Installing htop configuration..."
	@cd $(PACKAGES_DIR)/tools && $(STOW) htop

tmux: check-stow
	@echo "Installing tmux configuration..."
	@cd $(PACKAGES_DIR)/tools && $(STOW) tmux

gnupg: check-stow
	@echo "Installing gnupg configuration..."
	@cd $(PACKAGES_DIR)/tools && $(STOW) gnupg

curlrc: check-stow
	@echo "Installing curlrc configuration..."
	@cd $(PACKAGES_DIR)/tools && $(STOW) curlrc
	@echo "Creating .curlrc symlink in ~/.config/..."
	@ln -sf "$(HOME)/.config/curl/curlrc" "$(HOME)/.config/.curlrc"
	@echo "  LINK: .config/.curlrc -> curl/curlrc"

stow: check-stow
	@echo "Installing stow configuration..."
	@cd $(PACKAGES_DIR)/../management && $(STOW) stow

# Category targets
shell: bash zsh
editors: emacs nano vscode
development: git
desktop: i3 terminal
tools: htop tmux gnupg curlrc
management: stow

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
