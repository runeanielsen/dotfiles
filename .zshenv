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

export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

export BROWSER="/usr/bin/firefox"

export DOTNET_CLI_TELEMETRY_OPTOUT=1
export DOTNET_ROOT="/usr/share/dotnet"
export MSBuildSDKsPath=$(echo /usr/share/dotnet/sdk/5.*/Sdks);

export PYTHONPATH="/usr/share/qgis/python:/usr/share/qgis/python/plugins:/usr/lib/python3.9"

export TODO_FILENAME="/home/notation/.todo.json"

export TERM=xterm vim
