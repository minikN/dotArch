# Exports
#
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
