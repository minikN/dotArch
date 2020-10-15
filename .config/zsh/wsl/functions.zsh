# Functions

# checks to see if we are in a windows or linux dir
function isWinDir {
  case $PWD/ in
    /mnt/*) return $(true);;
    *) return $(false);;
  esac
}

# wrap the git command to either run windows git or linux
function git {
  if isWinDir
  then
    SSH_AUTH_SOCK="$SSH_AUTH_SOCK_WSL" git.exe "$@"
  else
    SSH_AUTH_SOCK="$SSH_AUTH_SOCK_LINUX" /usr/bin/git "$@"
  fi
}


function node {
  if isWinDir
  then
    node.exe "$@"
  else
    /usr/bin/node "$@"
  fi
}
