# Tools Makefile
# Individual tool configurations

.PHONY: all install-% uninstall-% status-% test-% clean-%

# Default target
all: install-curlrc install-gnupg install-htop install-inputrc install-ripgrep install-tmux

# Individual package targets
install-%:
	@echo "Installing $*..."
	@cd $* && stow --target=$$HOME --restow .
	@echo "$* installed successfully"

uninstall-%:
	@echo "Uninstalling $*..."
	@cd $* && stow --target=$$HOME --delete .
	@echo "$* uninstalled successfully"

status-%:
	@echo "Status for $*:"
	@cd $* && stow --target=$$HOME --simulate --verbose=2 . 2>&1 | head -10

test-%:
	@echo "Testing $* (dry run):"
	@cd $* && stow --target=$$HOME --simulate --no-folding .

clean-%:
	@echo "Cleaning broken symlinks for $*..."
	@find $$HOME -type l -exec test ! -e {} \; -print | grep $* || true

# Convenience targets
install-all: all
uninstall-all: uninstall-curlrc uninstall-gnupg uninstall-htop uninstall-inputrc uninstall-ripgrep uninstall-tmux

status-all: status-curlrc status-gnupg status-htop status-inputrc status-ripgrep status-tmux

test-all: test-curlrc test-gnupg test-htop test-inputrc test-ripgrep test-tmux

list:
	@echo "Available tools packages:"
	@echo "  curlrc    - curl configuration"
	@echo "  gnupg     - GnuPG configuration"
	@echo "  htop      - htop configuration"
	@echo "  inputrc   - readline configuration"
	@echo "  ripgrep   - ripgrep search configuration"
	@echo "  tmux      - tmux terminal multiplexer"
