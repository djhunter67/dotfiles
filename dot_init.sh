#!/bin/bash

# Stop on a nonzero return of any command
set -e
# Run as root
#if [ "$UID" -ne "$ROOT_UID" ]
#then
#    echo "Must be root to run this script."
#    exit $E_NOTROOT
#fi

#########
# NOTES #
#########
# If Nvidia card is installed, append the following line:
# pcie_aspm=off
# to /boot/loader/entries/{the present kernel} at the end of 
# EX: options root=UUID=0a3407de-014b-458b-b5c1-848e92a327a3 rw quiet splash {enter option here}

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
    "texlive"
    "hunspell-en_us"
    "arduino"
    "zsh"
    "zsh-theme-powerlevel10k"
    "tree"
    "texlive-basic"
    "ruff"
    "redis"
    "openvpn"
    "openssh"
    "emacs"
    "mesa"
    "man-db"
    "btop"
    "ripgrep"
    "firefox"
    "docker"
    "docker-compose"
    "sqlite"
    "bat"
    "perf"
    "pls"
    "awesome-terminal-fonts"
)

# Make the work directory
pushd ~/Documents 
![[  -d "work_worK_woRk_wOrk_Work_WORK" ]] && mkdir work_worK_woRk_wOrk_Work_WORK;

popd  # go to  home directory

# Make the .zsh directory if it doesn't exist
![[ -d "~/.zsh" ]] && mkdir .zsh

# Make the development directory
![[ -d "dev" ]] && mkdir dev

# Create the .BUILD directory
![[ -d ".BUILDS" ]] && mkdir .BUILDS

# Create ssh key to pull the dotfiles repo from github
![[ -f ".ssh/id_ed22519_base_key.pub"  ]] && ssh-keygen -t ed25519 -N "" -f /home/${USER}/.ssh/id_ed25519_base_key -q

# Blocking command to show the pub key; Add keys to github repo(s)
echo "###############################################"
echo "Place the key onto GitHub"
echo "###############################################"
more ~/.ssh/id_ed25519_base_key.pub

# When command closes clone dotfiles repo
sudo pacman -S git base-devel --noconfirm

# Install the zsh autocomplete
pushd ~/.zsh
git clone  git@github.com:zsh-users/zsh-autosuggestions.git
popd

# Install the straight installation manager
![[ -d "~/.emacs.d/straight.el" ]] && pushd ~/.emacs.d/ && git clone git@github.com:radian-software/straight.el.git
 

# Link to .zshrc that came from dotfiles
![[ -L "~/.zshrc"  ]] && ln -s dotfiles/.zshrc .zshrc

# Link to .zsh_history
![[ -f "~/.zsh_history" ]] && rm .zsh_history && ln -s dotfiles/.zsh_history .zsh_history

# Link to .emacs; remove any existing file
[[ -f "~/.emacs" ]] && rm -rf ~/.emacs && ln -s dotfiles/.emacs .emacs

# Link gitconfig
[[ -L "~/.gitconfig" ]] && ln -s dotfiles/.gitconfig .gitconfig

# Install yay
echo "###############################################"
echo "Installing yay"
echo "###############################################"


if ![ -d "~/.BUILDS" ]; then
    mkdir ~/.BUILDS
fi

if ![ -d "~/.BUILDS/yay" ]; then
    git clone https://aur.archlinux.org/yay.git .BUILDS/yay
    pushd .BUILDS/yay && makepkg -si --noconfirm

    popd
fi

# Install all of the PACKAGES
yay -S ${PACKAGES[@]} --noconfirm

# Install rust
echo "###############################################"
echo "Installing Rust"
echo "###############################################"

pushd ~/.BUILDS
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

source "$HOME/.cargo/env"

rustup install stable
rustup default stable

popd

# Install the latest stable version of python from source
echo "###############################################"
echo "Installing Python 3.12.0 from source"
echo "###############################################"

export PYTHON_VERSION=$(python -V)

pushd ~/.BUILDS

if [[ ! -d "python" ]]; then
    
    wget https://www.python.org/ftp/python/3.12.0/Python-3.12.0.tar.xz
    tar -xf Python*.tar.xz
    pushd Python-3.12.0

    ./configure --prefix=/home/${USER}/.local/ --exec-prefix=/home/${USER}/.local/ --enable-optimizations --bindir=/home/${USER}/.local/ --with-ensurepip=install --enable-shared --enable-profiling --enable-pystats --enable-loadable-sqlite-extensions --enable-ipv6 --oldincludedir=/home/${USER}/.local/ --enable-ipv6

    make -j install

    
    popd
fi
popd
