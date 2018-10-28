# This is needed so emacs' TRAMP mode doesn't hang when I try to connect to a
# server that has my config.
[[ $TERM = dumb ]] && return

export PATH=~/bin:$PATH
export EDITOR=emacs
export MANWIDTH=80

PS1="\\[\\e[36m\\]\\w \\[\\e[31m\\]❯ \\[\\e[0m\\]"
if [[ -n "$SSH_CLIENT" || -n "$SSH_TTY" ]]; then
  PS1="(\\[\\e[33m\\]\\h) $PS1"
fi
export PS1

alias ..="cd .."
alias ...="..; .."
alias dc="cd"
alias l="ls -1"
alias sl="ls"
alias md="mkdir -p"
alias maek="make"
alias gerp="grep"
alias g="git"
alias v="vagrant"
alias gdb="gdb -q"
alias py="python"
alias py3="python3"

dotfiles() {
  git --git-dir="$HOME"/.dotfiles/ --work-tree="$HOME" "$@"
}

OS=$(uname)
if [[ "$OS" = "Darwin" ]]; then
  export CLICOLOR=1
  export HOMEBREW_NO_AUTO_UPDATE=1
  export HOMEBREW_NO_ANALYTICS=1
  alias make="gmake"
  e() { open -a /Applications/Emacs.app "$@"; }
  t() { mv "$@" ~/.Trash; }
elif [[ "$OS" = "Linux" ]]; then
  shopt -s globstar
  alias ls="ls --color=auto"
fi

# pipsi
export PATH=~/.local/bin:$PATH
