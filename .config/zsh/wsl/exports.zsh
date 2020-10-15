# Exports

export WSL_DISPLAY="$(grep -m 1 nameserver /etc/resolv.conf | awk '{print $2}'):0.0"
export WORKDIR="$(wslpath $(wslvar USERPROFILE))/code" # requires wslu
export SSH_AUTH_SOCK="$SSH_AUTH_SOCK_WSL"

# Fonts
[[ ! -d "$HOME/.fonts/windows" ]] && mkdir -p "$HOME/.fonts/windows" && ln -s /mnt/c/Windows/Fonts/ ~/.fonts/windows && fc-cache -f

## Requires socat to be installed!
ss -a | grep -q $SSH_AUTH_SOCK
if [ $? -ne 0 ]; then
        rm -f $SSH_AUTH_SOCK
        (setsid nohup socat UNIX-LISTEN:$SSH_AUTH_SOCK,fork EXEC:$BINDIR/wsl2-ssh-pageant.exe >/dev/null 2>&1 &)
fi
