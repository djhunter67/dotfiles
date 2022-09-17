# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi



# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export LC_CTYPE="en_US.UTF-8"

plugins=(
    fzf
    history-substring-search
    colored-man-pages
    colorize
    zsh-z
    command-not-found
    zsh-completions
    zsh-autosuggestions
    zsh-syntax-highlighting
)

fpath+=${ZSH_CUSTOM:-${ZSH:-~/.oh-my-zsh}/custom}/plugins/zsh-completions/src


source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh


#autoload -U compinit && compinit



# Make any executable placed in ~/bin discoverable on $PATH
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
fi


# Make any executable placed in ~/.local/bin discoverable on $PATH
if [ -d "$HOME/.local/bin" ] ; then
    export PATH="$HOME/.local/bin:$PATH"
fi


#############################################################################
# HISTORY
#############################################################################

HISTFILE=~/.zhistory
# Increase history size. Allow 32Â³ entries;
HISTFILESIZE=32768
HISTSIZE=${HISTFILESIZE}
HISTCONTROL=ignoreboth
HISTIGNORE="&:ls:[bf]g:exit";
SAVEHIST=${HISTSIZE}

setopt INC_APPEND_HISTORY
export HISTTIMEFORMAT="[%F %T] "
# Add timestampt to command
setopt EXTENDED_HISTORY
# No Duplicates
setopt HIST_IGNORE_ALL_DUPS


# # Change caps lock to left CTRL
#xmodmap ~/.Xmodmap

# Color for manpages
#
export MANPAGER="less -R --use-color -Dd+r -Du+b"
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# Increase Bash history size. Allow 32Â³ entries; the default is 500.
#
export HISTSIZE='32768';
export HISTFILESIZE="${HISTSIZE}";

# Ignore duplicates, ls without options and builtin commands
#
export HISTCONTROL=ignoreboth
export HISTIGNORE="&:ls:[bf]g:exit";

# Make emacs the default editor.
#
export EDITOR=emacs

# Change caps lock to left CTRL
#xmodmap ~/.Xmodmap

# Make Python use UTF-8 encoding for output to stdin, stdout, and stderr.
#
export PYTHONIOENCODING='UTF-8';

# Make Python use UTF-8 encoding for output to stdin, stdout, and stderr.
#
export PYTHONIOENCODING='UTF-8';

# Prefer US English and use UTF-8.
#
export LANG='en_US.UTF-8';
export LC_ALL='en_US.UTF-8';

# Highlight section titles in manual pages.
#
export LESS_TERMCAP_md="${yellow}";

# Golang Library and dependancies
#
#export PATH=$PATH:/usr/local/go/bin

#######################################
# User specific aliases 
#######################################

# alias python='/usr/local/bin/python3.10'
# alias pip='/usr/local/bin/python3.10 -m pip'
[[ "$TERM" == "xterm-kitty" ]] && alias ssh="kitty +kitten ssh"
alias Ripley='ssh root@192.168.110.24'
alias Pi='ssh pi@192.168.110.78'
alias ripley_01='ssh ripley_01@192.168.110.1'
alias ripley_02='ssh ripley_02@192.168.110.2'
alias ripley_03='ssh ripley_03@192.168.110.3'
alias ripley_04='ssh ripley_04@192.168.110.4'
alias ripley_05='ssh ripley_05@192.168.110.5'
alias ripley_06='ssh ripley_06@192.168.110.6'
alias ripley_07='ssh ripley_07@192.168.110.7'
alias ripley_08='ssh ripley_08@192.168.110.8'
alias ripley_00='ssh -X ripley_00@192.168.110.9'
alias anse='cd ~/ansible && ansible-playbook -i inventory --forks 18 --ask-vault-pass --extra-vars '@passwords.yml' $1'
alias venv="python3.10 -m venv venv; unalias python pip; source venv/bin/activate; pip install -U pip setuptools &> /dev/null"
alias webcam="sudo modprobe v4l2loopback exclusive_caps=1 max_buffers=2; gphoto2 --stdout --capture-movie | ffmpeg -hwaccel nvdec -c:v mjpeg_cuvid -i - -vcodec rawvideo -pix_fmt yuv420p -threads 4 -f v4l2 /dev/video4"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias l="ls -lFh --color=auto"
alias ll="pls -a -d type -d size -d atime -d std -s atime -u decimal"
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
alias grep='grep --color=auto'  # GREP_OPTIONS is deprecated
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
up(){
  local d=""
  limit=$1
  for ((i=1 ; i <= limit ; i++))
    do
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
	mkdir -p "$@" && cd "$_";
}

# Use Gitâs colored diff when available
#
hash git &>/dev/null;
if [ $? -eq 0 ]; then
	function diff() {
		git diff --no-index --color-words "$@";
	}
fi;

# Create a data URL from a file
#
function dataurl() {
	local mimeType=$(file -b --mime-type "$1");
	if [[ $mimeType == text/* ]]; then
		mimeType="${mimeType};charset=utf-8";
	fi
	echo "data:${mimeType};base64,$(openssl base64 -in "$1" | tr -d '\n')";
}

# `tre` is a shorthand for `tree` with hidden files and color enabled, ignoring
# the `.git` directory, listing directories first. The output gets piped into
# `less` with options to preserve color and line numbers, unless the output is
# small enough for one screen.
#
function tre() {
	tree -aC -I '.git|node_modules|bower_components' --dirsfirst "$@" | less -FRNX;
}
r_sync(){
rsync -Paurvh --stats --progress "$1" "$2"
}
source ~/powerlevel10k/powerlevel10k.zsh-theme

source /home/djhunter67/.BUILDS/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
