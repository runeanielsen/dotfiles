# Defined in - @ line 1
function yta --wraps='youtube-dl -x -f bestaudio/best' --description 'alias yta youtube-dl -x -f bestaudio/best'
  youtube-dl -x -f bestaudio/best $argv;
end
