# Exports

# XDG base directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
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
 
# Xorg
export XINITRC="$XDG_CONFIG_HOME/X11/xinitrc"
export XSERVERRC="$XDG_CONFIG_HOME/X11/xserverrc"
export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"

# Emacs
export EMACS_USER_DIRECTORY="$XDG_CONFIG_HOME/emacs"

# GnuPG
export GNUPGHOME="$XDG_DATA_HOME/gnupg"

# SSH
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

# GIT
export GITDIR="$XDG_DATA_HOME/git"

# Adding to PATH
export PATH="$PATH:$XDG_DATA_HOME/bin"
export PATH="$PATH:$XDG_CONFIG_HOME/emacs/bin"