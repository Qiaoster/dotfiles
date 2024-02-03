#
# ~/.bashrc
#

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_RUNTIME_HOME="${HOME}/.local.runtime"

export HISTFILE="${XDG_STATE_HOME}/bash/history"

export CARGO_HOME="$${XDG_DATA_HOME}/cargo"
export CUDA_CACHE_PATH="${XDG_CACHE_HOME}/nv"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

alias project='xrandr --output DP-3 --auto --same-as DP-1'
alias sony='bluetoothctl connect 38:18:4C:1A:3E:4B'
alias steam='flatpak run com.valvesoftware.Steam'
alias shut='sudo systemctl poweroff'
alias semacs='sudo emacs -nw'
alias aienv='source ~/ai/aienv/bin/activate'
