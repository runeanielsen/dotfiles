export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$XDG_CONFIG_HOME/local/share"
export XDG_CACHE_HOME="$XDG_CONFIG_HOME/cache"

export ALTERNATE_EDITOR=""
export EDITOR="nvim"
export VISUAL="emacsclient -c -a emacs"
export PAGER="less"

export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

export BROWSER="/usr/bin/firefox"

export DOTNET_CLI_TELEMETRY_OPTOUT=1
export DOTNET_ROOT="/usr/share/dotnet"
export MSBuildSDKsPath=$(echo /usr/share/dotnet/sdk/5.*/Sdks);

export PYTHONPATH="/usr/share/qgis/python:/usr/share/qgis/python/plugins"

export TODO_FILENAME="/home/notation/.todo.json"

export GOBIN="/usr/bin"

export TERM=xterm vim
