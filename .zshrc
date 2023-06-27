# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

setopt SHARE_HISTORY
setopt HIST_IGNORE_ALL_DUPS
unsetopt HIST_IGNORE_DUPS
#!/bin/zsh

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
# if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#   source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
# fi

# Stop script of any command return non-zero
# set -e

#zsh_theme="powerlevel10k/powerlevel10k"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
#[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# export LC_CTYPE="en_US.UTF-8"

###########################
plugins=(
    colorize
    command-not-found
    zsh-completions
    zsh-autosuggestions
    zsh-syntax-highlighting
)
###########################

# precmd_vcs_info() { vcs_info }
# precmd_functions += ( precmd_vcs_info )
setopt prompt_subst
# RPROMPT='${vcs_info_msg_0_}'
# PROMPT='${vcs_info_msg_0_}%# '
zstyle ':vcs_info:git:*' formats '%b'
#fpath+=${ZSH_CUSTOM:-${ZSH:-~/.oh-my-zsh}/custom}/plugins/zsh-completions/src
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=5'
bindkey "^[[1;3C" forward-word
bindkey "^[[1;3D" backward-word

# Make any executable placed in ~/bin discoverable on $PATH
if [ -d "$HOME/bin" ]; then
    export PATH="$HOME/bin:$PATH"
fi

# Make any executable placed in ~/.local/bin discoverable on $PATH
if [ -d "$HOME/.local/bin" ]; then
    export PATH="$HOME/.local/bin:$PATH"
fi

setopt INC_APPEND_HISTORY
export HISTTIMEFORMAT="[%F %T] "
# Add timestampt to command
setopt EXTENDED_HISTORY
# No Duplicates
setopt HIST_IGNORE_ALL_DUPS

# Color for manpages
#
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# Increase Bash history size. Allow 32³ entries; the default is 500.
#
HISTFILE=~/.zsh_history
export HISTSIZE='32768'
export HISTFILESIZE="${HISTSIZE}"

# Ignore duplicates, ls without options and builtin commands
#
export HISTCONTROL=ignoreboth
export HISTIGNORE="&:ls:[bf]g:exit"

# Make emacs the default editor.
#
export EDITOR='emacs'

# Change caps lock to left CTRL
#xmodmap ~/.Xmodmap

# Make Python use UTF-8 encoding for output to stdin, stdout, and stderr.
#
export PYTHONIOENCODING='UTF-8'

# Make Python use UTF-8 encoding for output to stdin, stdout, and stderr.
#
export PYTHONIOENCODING='UTF-8'

# Prefer US English and use UTF-8.
#
export LANG='en_US.UTF-8'
export LC_ALL='en_US.UTF-8'

# Highlight section titles in manual pages.
#
export LESS_TERMCAP_md="${yellow}"

# Golang Library and dependancies
export PATH=$PATH:/usr/local/go/bin

# Replace Emacs hash table deserialization method.
export LSP_USE_PLISTS=true

#######################################
# User specific aliases
#######################################

# alias python='/usr/local/bin/python3.10'
# alias pip='/usr/local/bin/python3.10 -m pip'
[ "$TERM" = "xterm-kitty" ] && alias ssh='kitty +kitten ssh'
alias ubuntu_box='kitty +kitten ssh hunter_desk@10.10.30.119'
#alias venv="python -m venv venv && source venv/bin/activate && pip install -U pip setuptools &> /dev/null && git init &> /dev/null && touch README.md && git add . && git cm 'init git' && git st"
alias webcam="sudo modprobe v4l2loopback exclusive_caps=1 max_buffers=2; pkill -f gphoto2;  gphoto2 --stdout --capture-movie | ffmpeg -i - -vcodec rawvideo -pix_fmt yuv420p -threads 4 -f v4l2 /dev/video0"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias l="ls -lFh --color=auto"
alias ll="pls --align --multi-cols -d group -d size -d atime -u decimal -s atime --dirs --files"
alias icat="kitty +kitten icat"
alias la="pls -a -d -s mtime -u decimal -i nerd -c --no-dirs"
alias dif="kitty +kitten diff"

# cd into the old directory
#
alias bd='cd "$OLDPWD"'

# Use the emacs server
#
#alias emacs='emacsclient'

# Grep, egrep, zgrep colors
#
alias grep='grep --color=auto' # GREP_OPTIONS is deprecated
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# Allow aliases to be sudo'ed
#
alias sudo='sudo '

# Get the week number
#
alias week='date +%V'

# Clean alias and path list
#
alias aliases="alias | sed 's/=.*//'"
alias paths='echo -e ${PATH//:/\\n}'

# Return only the WAN IP
#
#alias ip="curl -s ipinfo.io | jq -r '.ip'"

# 100MB Speedtest
#
alias speedtest="wget -O /dev/null http://speed.transip.nl/100mb.bin"

# Clear DELUGE
#
alias DELUGE='rm -rf ~/Downloads/DELUGE/*'

# Make CAPS LOCK into CTRL
#
#xmodmap ~/.Xmodmap

######################################
# User defined functions
######################################

# Replace up../../../../
#
up() {
    local d=""
    limit=$1
    for ((i = 1; i <= limit; i++)); do
        d=$d/..
    done
    d=$(echo $d | sed 's/^\///')
    if [ -z "$d" ]; then
        d=..
    fi
    cd $d
    ll
}

# Create a new directory and enter it
#
function mkd() {
    mkdir -p "$@" && cd "$_"
}

# Use Git’s colored diff when available
#
hash git &>/dev/null
if [ $? -eq 0 ]; then
    function diff() {
        git diff --no-index --color-words "$@"
    }
fi

# Create a data URL from a file
#
function dataurl() {
    local mimeType=$(file -b --mime-type "$1")
    if [[ $mimeType == text/* ]]; then
        mimeType="${mimeType};charset=utf-8"
    fi
    echo "data:${mimeType};base64,$(openssl base64 -in "$1" | tr -d '\n')"
}

# `tre` is a shorthand for `tree` with hidden files and color enabled, ignoring
# the `.git` directory, listing directories first. The output gets piped into
# `less` with options to preserve color and line numbers, unless the output is
# small enough for one screen.
#
function tre() {
    tree -aC -I '.git|node_modules|bower_components' --dirsfirst "$@" | less -FRNX
}

function r_sync() {
    rsync -Paurvh --stats --progress $1 $2
}

# `venv` is a function that will initial a python virtualenv virtual envrionment with the name 'venv'
# The virtual environment will be activated and pip updated.  Basic directory structure will be set up.
# The venv function will suppress all output except the last line; git init.
function venv() {

    python3.11 -m venv venv
    source venv/bin/activate 
    pip install -U pip setuptools >/dev/null 
    git init &>/dev/null 
    git branch -M trunk 
    [[ -e ./README.md ]] && echo > README.md
    [[ -e ./LICENSE ]] && echo > LICENSE
    [[ -e ./CHANGELOG.md ]] && echo > CHANGELOG.md 
    cat <<EOT >> CHANGELOG.md

# CHANGELOG										       
											       
Author: Hunter, Christerpher								       
											       
All notable changes will be appended here.						       
											       
This project, henceforth, will recongnize [semantic versioning](https://semver.org/).	       
											       
## [⭐.✴️.✳️] - YEAR MONTH DAY								       
											       
Here we write upgrade and change notes.						       
											       
⭐              MAJOR version when you make incompatible API changes,				       
											       
✴️ MINOR version when you add functionality in a backwards compatible manner		       
											       
✳️ PATCH version when you make backwards compatible bug fixes.				       
											       
--------------------------------------

✳️[0.0.1] - 2023 JUNE 25

- Automatic initialization of the project structure 

EOT

    [[ -f ~/.gitignore_global ]] && cat /home/djhunter67/.gitignore_global > .gitignore
    mkdir -p ./src
    mkdir -p .github 
    mkdir -p .github/workflows
    pushd .github
    [[ -e pull_request_template ]] && echo >> pull_request_template.md
    cat <<EOT >> pull_request_template.md

This PR...										       
											       
## Changes										       
											       
-											       
## Screenshots10									       
											       
(prefer animated gif)									       
											       
## Checklist										       
											       
- [ ] synced trunk									       
- [ ] tested locally									       
- [ ] added new dependencies								       
- [ ] updated the docs								       
- [ ] added a test									       
											       
# Fixes 										       
											       
EOT
    popd
    git add . &>/dev/null &&
        git cm "init: initializing the project" &>/dev/null &&
        tre -L 3 -I venv
}

source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme

# Lines configured by zsh-newuser-install
# HISTFILE=~/.zsh_history
# HISTSIZE=4294967296
# SAVEHIST=4294967296
setopt beep extendedglob nomatch
source /home/djhunter67/dotfiles/.BUILDS/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
