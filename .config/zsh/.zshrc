# Includes
source "$ZDOTDIR/exports.zsh"
source "$ZDOTDIR/aliases.zsh"

# Colors
autoload -U colors && colors

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

# Syntax highlighting (last)
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2> /dev/null