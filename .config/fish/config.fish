set -x PATH $PATH /sbin/

function ll
    ls -lh $argv
end

set -U EDITOR nvim
set -U theme_display_date no
set -Ux DOTNET_CLI_TELEMETRY_OPTOUT 1
set -Ux DOTNET_ROOT /usr/share/dotnet
set -Ux MSBuildSDKsPath /usr/share/dotnet/sdk/3.1.103/Sdks

function fish_right_prompt; end
function fish_greeting; end

set -U fish_key_bindings fish_default_key_bindings

if status --is-interactive
    set PATH $PATH ~/.local/bin;
end

fish_ssh_agent

