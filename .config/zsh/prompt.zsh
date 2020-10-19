# Set timer before every command.
function preexec() {
  timer=$(date +%s%3N)
}

# Calculate time elapsed since setting the timer.
function precmd() {
  if [ $timer ]; then
    local tc
    local now=$(date +%s%3N)
    local d_ms=$(($now-$timer))

    case $((
              d_ms <= 20 ? 1 :
              d_ms <= 100 ? 2 :
              d_ms <= 250 ? 3 :
              d_ms <= 500 ? 4 :
              d_ms <= 999 ? 5 : 6)) in
        (1) tc="%F{green}" ;;
        (2) tc="%F{yellow}" ;;
        (3) tc="%F{cyan}" ;;
        (4) tc="%F{blue}" ;;
        (5) tc="%F{magenta}" ;;
        (6|*) tc="%F{red}" d_ms=$((d_ms / 1000)) ;;
    esac

    ms=$(printf '%03d' $d_ms)

    export PROMPT="${tc}${ms}%{$reset_color%} $(mnml_cwd 2 0)%F{yellow} ➜%{$reset_color%} "
    unset timer
  fi
}
