# XDG base directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_DATA_DIRS="/usr/local/share:/usr/share"
export XDG_CONFIG_DIRS="/etc/xdg"

# Setting $XDG_RUNTIME_DIR
export XDG_RUNTIME_DIR=/tmp/${UID}-runtime-dir
if ! test -d "${XDG_RUNTIME_DIR}"; then
	mkdir "${XDG_RUNTIME_DIR}"
	chmod 0700 "${XDG_RUNTIME_DIR}"
fi
