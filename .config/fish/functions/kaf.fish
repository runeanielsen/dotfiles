# Defined in - @ line 1
function kaf --wraps='kubectl apply -f' --description 'alias kaf kubectl apply -f'
  kubectl apply -f $argv;
end
