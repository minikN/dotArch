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

# ssh
alias elftower="ssh -p 22200 demis@116.203.110.209"
alias mailserver="ssh -p 22 root@195.201.20.120"

# qutebrowser
alias qutebrowser="qutebrowser -R"
