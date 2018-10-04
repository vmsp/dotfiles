[[ $TERM = dumb ]] && return

export PATH=~/bin:$PATH
export EDITOR=emacs
export MANWIDTH=80

export PS1="\\[\\e[33m\\]\\h \\[\\e[36m\\]\\w \\[\\e[31m\\]❯ \\[\\e[0m\\]"

alias ..="cd .."
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
alias make="gmake"

dotfiles() {
  git --git-dir="$HOME"/.dotfiles/ --work-tree="$HOME" "$@"
}

if [[ $(uname) == "Darwin" ]]; then
  export CLICOLOR=1
  export HOMEBREW_NO_AUTO_UPDATE=1
  export HOMEBREW_NO_ANALYTICS=1
  e() { open -a /Applications/Emacs.app "$@"; }
  t() { mv "$@" ~/.Trash; }
elif [[ $(uname) == "Linux" ]]; then
  shopt -s globstar
  alias ls="ls --color=auto"
fi

# pipsi
export PATH=~/.local/bin:$PATH
