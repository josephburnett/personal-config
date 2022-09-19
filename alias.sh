alias gd='git diff'
alias gs='git status'
# https://stackoverflow.com/questions/5188320/how-can-i-get-a-list-of-git-branches-ordered-by-most-recent-commit
alias gb="git for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))'"
alias gt="glab api https://gitlab.com/api/v4/todos | jq  '.[] | { target_url, body }'"
alias kickshell='exec bash -l'
alias k='kubectl'
alias when="ts '[%Y-%m-%d %H:%M:%.S]'"
alias gotest='while true; do inotifywait -e close_write -r . 2>/dev/null ; clear ; go test ./... ; done'
alias org='emacs ~/org/log.org'
