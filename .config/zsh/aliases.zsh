# Aliases (Common)

# GnuPG
alias gpg2="gpg2 --homedir ${GNUPGHOME}"

# Navigation
alias lss="ls --group-directories-first --color=always -laAh"
alias ..="cd .."
alias ...="cd ../../"
alias ....="cd ../../../"

# ssh
alias elftower="SSH_AUTH_SOCK="$SSH_AUTH_SOCK_LINUX" ssh -p 22200 demis@116.203.110.209"
alias mailserver="SSH_AUTH_SOCK="$SSH_AUTH_SOCK_LINUX" ssh -p 22 root@195.201.20.120"

# qutebrowser
alias qutebrowser="qutebrowser -R"

# next browser
alias next="next --session nil"

# docker
alias d-stp="docker stop $(docker ps -aq)"
alias d-rmc="docker rm $(docker ps -aq)"
alias d-rmv="docker volume rm $(docker volume ls -q)"
alias d-rmi="docker rmi $(docker images -q)"
alias d-all="d-stp; d-rmc; d-rmv; d-rmi"
