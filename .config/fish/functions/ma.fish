# Defined in - @ line 1
function ma --wraps='mount -a' --description 'alias ma mount -a'
  mount -a $argv;
end
