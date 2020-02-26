set -x PATH $PATH /sbin/

function ll
    ls -lh $argv
end

set -U EDITOR nvim
set -g theme_display_date no

function fish_right_prompt; end
function fish_greeting; end

set -U fish_key_bindings fish_default_key_bindings

if status --is-interactive
    set PATH $PATH ~/.local/bin;
end

fish_ssh_agent

fish_vi_key_bindings
