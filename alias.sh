alias gd='git diff'
alias gs='git status'
# https://stackoverflow.com/questions/5188320/how-can-i-get-a-list-of-git-branches-ordered-by-most-recent-commit
alias gb="git for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))'"
alias kickshell='exec bash -l'
alias kk='kubectl'
alias kkn='kubectl -n knative-serving'
alias kkc='kubectl config current-context'
alias koa='ko apply'
alias kod='ko delete  --ignore-not-found'
alias when="ts '[%Y-%m-%d %H:%M:%.S]'"
