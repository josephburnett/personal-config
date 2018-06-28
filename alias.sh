alias em='emacs'
alias ff='firefox'
alias fm='xdg-open https://fastmail.com >/dev/null &'
alias gd='git diff'
alias gs='git status'
alias gb="git for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))'"
alias kickshell='exec bash -l'
alias op='xdg-open'
alias up="upower -i /org/freedesktop/UPower/devices/battery_BAT1 | awk '/percentage/ { print \$2 }'"
alias gotest="while true ; do inotifywait -qq -e close_write -r . 2>/dev/null ; go test *.go ; done"
alias pd='pushd'

