#
# ~/.bashrc
#

export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_RUNTIME_HOME="${HOME}/.local.runtime"

export HISTFILE="${XDG_STATE_HOME}/bash/history"

export RUSTUP_HOME="$XDG_DATA_HOME"/rustup
export CARGO_HOME="${XDG_DATA_HOME}/cargo"
export CUDA_CACHE_PATH="${XDG_CACHE_HOME}/nv"
export ICEAUTHORITY="$XDG_CACHE_HOME"/ICEauthority

export GPODDER_HOME="${HOME}/media/podcast"

export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/npmrc

export WGETRC="$XDG_CONFIG_HOME/wgetrc"
alias wget='wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"'
export PATH=~/.config/emacs/bin:~/scripts:$PATH

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias grep='grep --color=auto'
PS1='[\u@\h \W]\$ '

alias sony='bluetoothctl connect 38:18:4C:1A:3E:4B'
alias shut='sudo systemctl poweroff'
alias sus='sudo systemctl suspend'
alias semacs='sudo emacs -nw'
alias aienv='source ~/ai/aienv/bin/activate'
alias emacsserver='LC_CTYPE=zh_CN.UTF-8 emacs --daemon'
alias pico='~/pico-8/pico8_dyn -home ~/pico-8/data -root_path ~/pico-8/data/carts'
alias lofi='mpv --no-video https://www.youtube.com/live/jfKfPfyJRdk?si=T3DbMwDLc691Uoz4'
alias ls='eza -l'
