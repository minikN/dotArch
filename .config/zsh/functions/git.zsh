MNML_OK_COLOR="${MNML_OK_COLOR:-2}"
MNML_ERR_COLOR="${MNML_ERR_COLOR:-1}"
MNML_WHITE_COLOR="${MNML_WHITE_COLOR:-7}"

MNML_USER_CHAR="${MNML_USER_CHAR:-λ}"
MNML_INSERT_CHAR="${MNML_INSERT_CHAR:-›}"
MNML_NORMAL_CHAR="${MNML_NORMAL_CHAR:-·}"

# clone repo in $GITDIR
function gc {
	git clone "$1" "$GITDIR/$(basename $1 .git)"
}

function mnml_cwd {
    local segments="${1:-2}"
    local seg_len="${2:-0}"

    local _w="%{\e[0m%}"
    local _g="%{\e[38;5;244m%}"

    if [ "$segments" -le 0 ]; then
        segments=1
    fi
    if [ "$seg_len" -gt 0 ] && [ "$seg_len" -lt 4 ]; then
        seg_len=4
    fi
    local seg_hlen=$((seg_len / 2 - 1))

    local cwd="%${segments}~"
    cwd="${(%)cwd}"
    cwd=("${(@s:/:)cwd}")

    local pi=""
    for i in {1..${#cwd}}; do
        pi="$cwd[$i]"
        if [ "$seg_len" -gt 0 ] && [ "${#pi}" -gt "$seg_len" ]; then
            cwd[$i]="${pi:0:$seg_hlen}$_w..$_g${pi: -$seg_hlen}"
        fi
    done

    echo -n "$_g${(j:/:)cwd//\//$_w/$_g}$_w"
}

function mnml_git {
    local statc="%{\e[0;3${MNML_OK_COLOR}m%}" # assume clean
    local bname="$(git rev-parse --abbrev-ref HEAD 2> /dev/null)"

    if [ -n "$bname" ]; then
        local rs="$(git status --porcelain -b)"
        if $(echo "$rs" | grep -v '^##' &> /dev/null); then # is dirty
            statc="%{\e[0;3${MNML_ERR_COLOR}m%}"
        elif $(echo "$rs" | grep '^## .*diverged' &> /dev/null); then # has diverged
            statc="%{\e[0;3${MNML_ERR_COLOR}m%}"
        elif $(echo "$rs" | grep '^## .*behind' &> /dev/null); then # is behind
            statc="%{\e[0;3${MNML_WHITE_COLOR}m%}"
        elif $(echo "$rs" | grep '^## .*ahead' &> /dev/null); then # is ahead
            statc="%{\e[0;3${MNML_WHITE_COLOR}m%}"
        else # is clean
            statc="%{\e[0;3${MNML_OK_COLOR}m%}"
        fi

        echo -n "$statc$bname%{\e[0m%}"
    fi
}

function mnml_wrap {
    local arr=()
    local cmd_out=""
    for cmd in ${(P)1}; do
        cmd_out="$(eval "$cmd")"
        if [ -n "$cmd_out" ]; then
            arr+="$cmd_out"
        fi
    done

    echo -n "${(j: :)arr}"
}

setopt prompt_subst

RPROMPT='$(mnml_git)'
