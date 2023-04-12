alias gd = git diff
alias gddd = git diff --no-index --word-diff=color --word-diff-regex=.
alias gs = git status
# https://stackoverflow.com/questions/5188320/how-can-i-get-a-list-of-git-branches-ordered-by-most-recent-commit
# alias gb = (git for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:green)%(committerdate:relative)%(color:reset))' ; git remote get-url origin")

alias k = kubectl

alias when = ts '[%Y-%m-%d %H:%M:%.S]'

alias gotest = (while true; do inotifywait -e close_write -r . 2>/dev/null ; clear ; go test ./... ; done)

alias org = emacs ~/org/log.org

alias sshsock = eval "$(ssh-agent -s)"

alias sighup = kill -1
alias sigint = kill -2
alias sigquit = kill -3
alias sigkill = kill -9
alias sigterm = kill -15

def gt [] {
    glab api 'https://gitlab.com/api/v4/todos?per_page=50&order_by=last_activity_at&sort=asc' | from json | sort-by updated_at | each { |todo| { date: ($todo.updated_at | date format "%Y-%m-%d %H:%M:%S"), url: $todo.target_url, body: ($todo.body | str substring 0..50) } } | table
}

def gtw [] {
    clear
    while true {
        gt
        sleep 5min
        clear
    }
}