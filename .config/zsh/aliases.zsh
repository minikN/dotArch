# Aliases
#
# XOrg
alias eme="startx ${XDG_CONFIG_HOME}/X11/xinitrc -- vt1"

# GnuPG
alias gpg2="gpg2 --homedir ${GNUPGHOME}"

# ls
alias lss="ls --group-directories-first --color=always -laAhU"

# git
function gc {
	git clone "$1" "$GITDIR/$(basename $1 .git)"
}
