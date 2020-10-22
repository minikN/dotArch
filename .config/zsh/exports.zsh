# Exports

# XDG base directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$XDG_DATA_HOME/cache"
export XDG_DATA_DIRS="/usr/local/share:/usr/share"
export XDG_CONFIG_DIRS="/etc/xdg"

# Setting $XDG_RUNTIME_DIR
if test -z "${XDG_RUNTIME_DIR}"; then
	export XDG_RUNTIME_DIR=/tmp/${UID}-runtime-dir
	if ! test -d "${XDG_RUNTIME_DIR}"; then
		mkdir "${XDG_RUNTIME_DIR}"
		chmod 0700 "${XDG_RUNTIME_DIR}"
	fi
 fi

# Emacs
export EMACS_USER_DIRECTORY="$XDG_CONFIG_HOME/emacs"

# GnuPG
export GNUPGHOME="$XDG_DATA_HOME/gnupg"

# SSH
export SSH_AUTH_SOCK_WSL="$HOME/.ssh/agent.sock"
export SSH_AUTH_SOCK_LINUX=$(gpgconf --list-dirs agent-ssh-socket)
export SSH_AUTH_SOCK="$SSH_AUTH_SOCK_LINUX"

# DIRS
export GITDIR="$XDG_DATA_HOME/git"
export BINDIR="$XDG_DATA_HOME/bin"

# Adding to PATH
export PATH="$XDG_DATA_HOME/bin:$PATH"
export PATH="$XDG_CONFIG_HOME/emacs/bin:$PATH"

# CUDA
export CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv
