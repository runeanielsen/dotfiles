# Defined in - @ line 1
function kpa --wraps='kubectl get pods --all-namespaces' --description 'alias kpa kubectl get pods --all-namespaces'
  kubectl get pods --all-namespaces $argv;
end
