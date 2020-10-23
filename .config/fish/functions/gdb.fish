# Defined in - @ line 1
function gdb --wraps='git branch -D' --description 'alias gdb git branch -D'
  git branch -D $argv;
end
