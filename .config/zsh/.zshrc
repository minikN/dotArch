# Common configuration
## Includes
source "$ZDOTDIR/exports.zsh"
source "$ZDOTDIR/aliases.zsh"
source "$ZDOTDIR/prompt.zsh"

## Functions
for file in $ZDOTDIR/functions/*; do
    source "$file"
done

# Environment specific configuration
if [[ ! -z "$WSLENV" ]]; then
    source "$ZDOTDIR/wsl/exports.zsh"
    source "$ZDOTDIR/wsl/aliases.zsh"
    source "$ZDOTDIR/wsl/functions.zsh"
else
    source "$ZDOTDIR/linux/exports.zsh"
    source "$ZDOTDIR/linux/aliases.zsh"
    source "$ZDOTDIR/linux/functions.zsh"
fi

# Setup correct gnupg permissions
chown -R $(whoami) $GITDIR/dotArch/.local/share/gnupg
find $GITDIR/dotArch/.local/share/gnupg -type f -exec chmod 600 {} \;
find $GITDIR/dotArch/.local/share/gnupg -type d -exec chmod 700 {} \;

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
