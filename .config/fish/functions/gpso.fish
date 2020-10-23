# Defined in - @ line 1
function gpso --wraps='git push --set-upstream origin' --description 'alias gpso git push --set-upstream origin'
  git push --set-upstream origin $argv;
end
