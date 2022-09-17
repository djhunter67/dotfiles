# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Make any executable placed in ~/bin discoverable on $PATH
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
fi

#######################################
# User specific exports
#######################################

# History persists across all terminals
#
#shopt -s histappend
PROMPT_COMMAND='history -a'

# Case-insensitive globbing (used in pathname expansion)
#
#shopt -s nocaseglob

# Autocorrect typos in path names when using `cd`
#
#shopt -s cdspell

# Color for manpages
#
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
export EDITOR='emacs';

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

##########################################                                                                                                               
# COLORS                                                                                                                                                  
##########################################

orange=$(tput setaf 166)
yellow=$(tput setaf 228)
green=$(tput setaf 71)
white=$(tput setaf 15)
bold=$(tput bold)
reset=$(tput sgr0)

#########################################                                                                                      # PROMPT MANIPULATION
#########################################
#PS1="\[${green, bold}\]\n"    # newline                                                                                                                 
#PS1+="\[${orange}\]\u" # username
#PS1+="\[${white}\] at "
#PS1+="\[${yellow}\]\h"  # Host
#PS1+="\[${white}\] in "
#PS1+="\[${green}\]\W"  # Working Directory                                                                                                              
#PS1+="\n"
#PS1+="\[${white}\]\$ \[${reset}\]"  # '$' (and reset color)                                                                                             
export PS1

#######################################
# User specific aliases 
#######################################

alias python='/usr/local/bin/python3.11'
alias pip='/usr/local/bin/python3.11 -m pip'
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
alias venv="/usr/local/bin/python3 -m venv venv; unalias python pip;  source venv/bin/activate; pip install -U pip setuptools -q &> /dev/null"
alias webcam="gphoto2 --stdout --capture-movie | ffmpeg -hwaccel nvdec -c:v mjpeg_cuvid -i - -vcodec rawvideo -pix_fmt yuv420p -threads 4 -f v4l2 /dev/video0"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ll="ls -lFh --color=auto"
alias l.="ls -ldFh --color=auto"

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



[ -f ~/.fzf.bash ] && source ~/.fzf.bash

. "$HOME/.cargo/env"
