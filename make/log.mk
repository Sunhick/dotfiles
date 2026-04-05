# Shared logging helpers for Makefiles
# Include with: include <path>/make/log.mk
# Usage:       @$(call log,ok,LINK bash)
#              @$(call log,warn,SKIP powershell)
#              @$(call log,fail,stow not found)
#              @$(call log,info,Installing packages...)

_GREEN  := \033[0;32m
_YELLOW := \033[0;33m
_RED    := \033[0;31m
_CYAN   := \033[0;36m
_NC     := \033[0m

define log
case "$(1)" in \
	ok)   printf "  $(_GREEN)OK$(_NC)   %s\n" "$(2)";; \
	warn) printf "  $(_YELLOW)WARN$(_NC) %s\n" "$(2)";; \
	fail) printf "  $(_RED)FAIL$(_NC) %s\n" "$(2)";; \
	info) printf "$(_CYAN)%s$(_NC)\n" "$(2)";; \
esac
endef
