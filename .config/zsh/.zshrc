# Lines configured by zsh-newuser-install
HISTFILE="$XDG_DATA_HOME"/zsh/history
HISTSIZE=1000
SAVEHIST=1000
setopt nomatch
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/demis/.config/zsh/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall


source "$ZDOTDIR/exports.zsh"
source "$ZDOTDIR/aliases.zsh"
