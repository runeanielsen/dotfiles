set -x PATH $PATH /sbin/
set -x PATH $PATH $HOME/.node_modules/bin/

function ll
    ls -lh $argv
end

set -e MSBuildSDKsPath 
set -e DOTNET_ROOT 

set -U EDITOR nvim
set -U theme_display_date no
set -Ux DOTNET_CLI_TELEMETRY_OPTOUT 1
set -Ux DOTNET_ROOT /usr/share/dotnet
set -Ux MSBuildSDKsPath /usr/share/dotnet/sdk/3.1.103/Sdks
set -Ux PATH $HOME/.node_modules/bin $PATH

export npm_config_prefix=~/.node_modules

function fish_right_prompt; end
function fish_greeting; end

set -U fish_key_bindings fish_vi_key_bindings

if status --is-interactive
    set PATH $PATH ~/.local/bin;
end

fish_ssh_agent

