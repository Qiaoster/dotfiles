# Lines configured by zsh-newuser-install
HISTFILE=~/.config/zsh/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd beep extendedglob nomatch notify
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/qiao/.config/zsh/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

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

export PATH=~/.config/emacs/bin:~/scripts:$PATH

setopt PROMPT_SUBST
PROMPT="%{$(tput setaf 117)%}%D%{$(tput setaf 109)%}@%{$(tput setaf 152)%}%T %{$(tput setaf 81)%}%1~ %{$(tput sgr0)%}$ "
alias wget='wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"'

alias ls='ls --color=auto'
alias grep='grep --color=auto'

alias sony='bluetoothctl connect 38:18:4C:1A:3E:4B'
alias shut='sudo systemctl poweroff'
alias sus='sudo systemctl suspend'
alias semacs='sudo emacs -nw'
alias emacsserver='LC_CTYPE=zh_CN.UTF-8 emacs --daemon'
alias pico='~/pico-8/pico8_dyn -home ~/pico-8/data -root_path ~/pico-8/data/carts'
