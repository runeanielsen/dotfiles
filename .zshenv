# Fix for https://github.com/starship/starship/issues/2176
export LC_ALL=en_US.UTF-8

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$XDG_CONFIG_HOME/local/share"
export XDG_CACHE_HOME="$XDG_CONFIG_HOME/cache"

export PATH=~/.local/bin:$PATH
export PATH="$PATH:$HOME/.npm-packages/bin"

export ALTERNATE_EDITOR=""
export EDITOR="nvim"
export VISUAL="nvim"
export PAGER="less"

export GOPATH=$HOME/go
export GOBIN=$HOME/go/bin
export PATH=$PATH:$(go env GOPATH)/bin
export GO111MODULE="on" 

export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

export BROWSER="/usr/bin/firefox"

export DOTNET_CLI_TELEMETRY_OPTOUT=1
export DOTNET_ROOT="/usr/share/dotnet"
export MSBuildSDKsPath=$(echo /usr/share/dotnet/sdk/6.*/Sdks);
export PATH="$PATH:$HOME/.dotnet/tools"

export PYTHONPATH="/usr/share/qgis/python:/usr/share/qgis/python/plugins:/usr/lib/python3.9"

export TODO_FILENAME="/home/notation/.todo.json"

export TERM=xterm vim

export PATH="$PATH:$HOME/.nvm/versions/node/v16.13.0/bin"
export NVM_DIR=~/.nvm

export PATH="$PATH:$HOME/.config/scripts"

[[ -s "/usr/share/nvm/nvm.sh" ]] && source "/usr/share/nvm/nvm.sh" --no-use
