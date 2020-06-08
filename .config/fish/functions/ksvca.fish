# Defined in - @ line 1
function ksvca --wraps='kubectl get svc --all-namespaces' --description 'alias ksvca kubectl get svc --all-namespaces'
  kubectl get svc --all-namespaces $argv;
end
