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

#export GTK_IM_MODULE=fcitx
#export QT_IM_MODULE=fcitx
#export XMODIFIERS=@im=fcitx
#export SDL_IM_MODULE=fcitx

export WGETRC="$XDG_CONFIG_HOME/wgetrc"
alias wget='wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"'

export R_ENVIRON_USER="$XDG_CONFIG_HOME/R/Renviron"
export R_LIBS_USER="$XDG_DATA_HOME/R/library"

# if [ -z "${WAYLAND_DISPLAY}" ]; then
#     export XDG_CURRENT_SESSION=Hyprland
#     export XDG_SESSION_TYPE=wayland
#     export XDG_SESSION_DESKTOP=Hyprland
#     export GDK_BACKEND=wayland
#     export QT_QPA_PLATFORM=wayland
#     export SDL_VIDEODRIVER=wayland
#     export CLUTTER_BACKEND=wayland
#     export XCURSOR_SIZE=40
#     export QT_QPA_PLATFORMTHEME=qt6ct
# fi

export PATH=~/.config/emacs/bin:~/scripts:$PATH
#
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
