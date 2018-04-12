set fish_greeting

set -gx PATH ~/bin $PATH
set -gx EDITOR emacs

function fish_prompt
  set_color $fish_color_cwd
  echo -n (prompt_pwd)
  set_color normal
  echo -n ' ❯ '
end

abbr -a dc cd
abbr -a l ls -1
abbr -a sl ls
abbr -a md mkdir -p
abbr -a maek make
abbr -a gerp grep
abbr -a g git
abbr -a v vagrant
abbr -a gdb gdb -q

function dotfiles
  git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME $argv
end

if test (uname) = Darwin
  set -gx HOMEBREW_NO_AUTO_UPDATE 1
  set -gx HOMEBREW_NO_ANALYTICS 1
  abbr -a make gmake
  alias e 'open -a /Applications/Emacs.app'
  function t
    mv $argv ~/.Trash
  end
else
  alias e 'emacs'
end
