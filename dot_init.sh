#!/bin/bash

# Stop on a nonzero return of any command
set -e
# Run as root
#if [ "$UID" -ne "$ROOT_UID" ]
#then
#    echo "Must be root to run this script."
#    exit $E_NOTROOT
#fi

# packages to install
PACKAGES=(
    "wakatime-cli"
    "v4l2loopback-dkms"
    "gphoto2"
    "ffmpeg"
    "nodejs"
    "npm"
    "ruff-lsp"
    "tree"
    "libusb"
    "webkit2gtk"
    "gtk3"
    "neofetch"
)

# Make the work directory
pushd ~/Documents 
[[ ! -d "work_worK_woRk_wOrk_Work_WORK" ]] && mkdir work_worK_woRk_wOrk_Work_WORK;

popd  # go to  home directory

# Make the development directory
[[ ! -d "dev" ]] && mkdir dev

# Create the .BUILD directory
[[ ! -d ".BUILDS" ]] && mkdir .BUILDS

# Create ssh key to pull the dotfiles repo from github
[[ ! -f ".ssh/id_ed22519_base_key.pub"  ]] && ssh-keygen -t ed25519 -N "" -f /home/${USER}/.ssh/id_ed25519_base_key -q

# Blocking command to show the pub key; Add keys to github repo(s)
less ~/.ssh/id_ed25519_base_key.pub

# When command closes clone dotfiles repo
sudo pacman -S git base-devel --noconfirm
[[ ! -d "dotfiles"  ]] && git clone git@github.com:djhunter67/dotfiles.git

# Link to .zshrc that came from dotfiles
[[ ! -f ".zshrc"  ]] && ln -s dotfiles/.zshrc .zshrc

# Link to .zsh_history
[[ ! -f ".zsh_history" ]] && rm .zsh_history && ln -s dotfiles/.zsh_history .zsh_history

# Link to .emacs; remove any existing file
[[ ! -f ".emacs" ]] && rm -rf ~/.emacs && ln -s dotfiles/.emacs .emacs

# Link gitconfig
ln -s dotfiles/.gitconfig .gitconfig



