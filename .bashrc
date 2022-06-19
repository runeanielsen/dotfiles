# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='λ: '

# wal colors
(cat ~/.cache/wal/sequences)

# aliases
alias ls='exa'
alias dotf='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

alias copy='xsel --clipboard --input'
alias paste='xsel --clipboard --output'
alias rss='newsboat -C ~/.config/newsboat/config -c ~/nas/rss/cache.db -u ~/nas/rss/urls'

alias ma='mount /home/notation/nas'
alias r='ranger'
alias bt='bluetoothctl'
alias pomo='pomodoro-cli'

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
export TERM=xterm vim
export PATH="$PATH:$HOME/.config/scripts"

## dotnet
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export DOTNET_ROOT="/usr/share/dotnet"
export MSBuildSDKsPath=$(echo /usr/share/dotnet/sdk/6.*/Sdks);
export PATH="$PATH:$HOME/.dotnet/tools"

## python
export PYTHONPATH="/usr/share/qgis/python:/usr/share/qgis/python/plugins:/usr/lib/python3.9"

## node
export PATH=~/.nvm/versions/node/v16.13.0/bin:$PATH
export NVM_DIR=~/.nvm

## go
export GOPATH=$HOME/go
export GOBIN=$HOME/go/bin
export PATH=$PATH:$(go env GOPATH)/bin
export GO111MODULE="on"

[[ -s "$NVM_DIR/nvm.sh" ]] && source "$NVM_DIR/nvm.sh" --no-use
