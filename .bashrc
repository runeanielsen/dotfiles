# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# wal colors
(cat ~/.cache/wal/sequences)

# history control
## ignore duplicate history entries
histcontrol=ignoreboth

## append to history file
shopt -s histappend

## history to infinite (the value does not matter, as long as it is not numeric)
HISTSIZE="INFINITE"
HISTFILESIZE="INFINITE"
HISTCONTROL="ignoredups"

# window size
shopt -s checkwinsize

# prompt
export PS1="λ: "

# ls colors
export LS_COLORS='di=0;1:ex=31:tw=0;1:ow=0;1'

# aliases
alias ls='ls --color=auto'
alias dotf='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

alias pm='pulsemixer'
alias copy='xsel --clipboard --input'
alias paste='xsel --clipboard --output'
alias rss='newsboat -C ~/.config/newsboat/config -c ~/nas/rss/cache.db -u ~/nas/rss/urls'

alias ma='mount /home/notation/nas'
alias r='ranger'
alias bt='bluetoothctl'
alias tm='f() { timer "$@" -f /home/notation/.config/timer-cli/finished; unset -f f; }; f'
alias di='dict -d gcide'
alias th='dict -d moby-thesaurus'

alias k='kubectl'
alias kaf='kubectl apply -f'
alias kpa='kubectl get pods --all-namespaces'
alias ksvca='kubectl get svc --all-namespaces'

## personal scripts
alias qrc='~/.config/scripts/qrc.sh'
alias scf='~/.config/scripts/snake-case-files.sh'
alias spell='~/.config/scripts/spell.sh'
alias task='~/.config/scripts/task'

## screen setup for work
alias xwork='xrandr --output DP1 --mode 3440x1440 --output eDP1 --off && nitrogen --restore'

# exports
export BROWSER="/usr/bin/firefox"
export TERM=xterm-256color vim
export PATH="$PATH:$HOME/.config/scripts"
export PATH="$PATH:$HOME/.local/bin"

## dotnet
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export DOTNET_ROOT="/usr/share/dotnet"
export MSBuildSDKsPath=$(echo /usr/share/dotnet/sdk/6.*/Sdks);
export PATH="$PATH:$HOME/.dotnet/tools"

## rust
export PATH="$PATH:$HOME/.cargo/bin"

## python
export PYTHONPATH="/usr/share/qgis/python:/usr/share/qgis/python/plugins:/usr/lib/python3.9"

## go
export GOPATH=$HOME/go
export GOBIN=$HOME/go/bin
export PATH=$PATH:$(go env GOPATH)/bin
export GO111MODULE="on"

## node
export PATH=~/.nvm/versions/node/v16.13.0/bin:$PATH
export NVM_DIR=~/.nvm
[[ -s "$NVM_DIR/nvm.sh" ]] && source "$NVM_DIR/nvm.sh" --no-use

## emacs custom
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi
