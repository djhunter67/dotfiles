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
    "rsync"
    "wakatime"
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
    # "texlive"
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
    "ttf-firacode"
    "kitty"
    "rust-analyzer"
    "texlive-fontsrecommended"
    "pandoc"
    "texlive-plaingeneric"
    "texlive-latexextra"
    "texlive-bin-extra"
    "texlive-latex-extra"
    "texlive-luatex"
    "texlive-latex"
    "pdflatex"
    "libreoffice-still"
    "hunspell"
    "wget"
)



# Make the work directory
echo "###############################################"
echo "Making the work directory"
echo "###############################################"
if [[ ! -d "$HOME/Documents" ]]; then
    mkdir -p $HOME/Documents
fi

pushd $HOME/Documents 
if [[ ! -d "work_worK_woRk_wOrk_Work_WORK" ]]; then
    mkdir -p work_worK_woRk_wOrk_Work_WORK
fi

popd  # go to  home directory

# Make the development directory
echo "###############################################"
echo "Making the dev directory"
echo "###############################################"
if [[ ! -d "dev" ]]; then
    mkdir -p $HOME/dev
fi

# Create the .BUILD directory
echo "###############################################"
echo "Making the .BUILD directory"
echo "###############################################"
if [[ ! -d ".BUILDS" ]]; then
    mkdir -p $HOME/.BUILDS
fi

# Create ssh key to pull the dotfiles repo from github
echo "###############################################"
echo "Creating ssh key"
echo "###############################################"
if [[ ! -f ".ssh/id_ed22519_base_key.pub"  ]]; then
    ssh-keygen -t ed25519 -N "" -f /home/${USER}/.ssh/id_ed25519_base_key -q
fi


# Refresh the package database
echo "###############################################"
echo "Refreshing the package database"
echo "###############################################"
sudo pacman -Syu --noconfirm


# Install yay
echo "###############################################"
echo "Installing yay"
echo "###############################################"
sudo pacman -S --needed base-devel git --noconfirm

# check if the command return a non-zero value
if ! command -v yay &> /dev/null; then
    echo "Yay is not installed. Installing yay"

    if [[ ! -d "/.BUILDS/yay" ]]; then
	pushd $HOME/.BUILDS
	git clone https://aur.archlinux.org/yay.git
	pushd $HOME/.BUILDS/yay
	export USER=$(whoami)
	makepkg -si --noconfirm
	popd
	popd
    else
	pushd $HOME/.BUILDS/yay
	makepkg -si --noconfirm
	popd
    fi
fi

# Install firefox
echo "###############################################"
echo "Installing Firefox"
echo "###############################################"
yay -S firefox --noconfirm

# Link to .zsh that came from dotfiles
echo "###############################################"
echo "Linking to .zsh"
echo "###############################################"
if [[ -d "$HOME/.zsh" || -L "$HOME/.zsh" ]]; then
    rm -rf $HOME/.zsh
fi
ln -s $HOME/dotfiles/.zsh $HOME/.zsh

# Install the zsh autocomplete
if [[ ! -d "$HOME/.zsh/zsh-autosuggestions" ]]; then

    pushd $HOME/.zsh
    git clone https://github.com/zsh-users/zsh-autosuggestions.git
    popd
fi

# check for then install emacs.d directory
if [[ ! -d "$HOME/.emacs.d" ]]; then
	mkdir -p $HOME/.emacs.d
fi

# Install the straight installation manager
if [[ ! -d "$HOME/.emacs.d/straight.el" ]]; then
    pushd $HOME/.emacs.d/
    git clone https://github.com/radian-software/straight.el.git
    popd
fi
 
echo "###############################################"
echo "Linking .zshrc"
echo "###############################################"
if [[ -d "$HOME/.zshrc" || -L "$HOME/.zshrc" ]]; then
    rm -rf $HOME/.zshrc
fi
ln -s $HOME/dotfiles/.zshrc $HOME/.zshrc

# Link to .zsh_history
if [[ -f "$HOME/.zsh_history" ]]; then
    rm $HOME/.zsh_history
fi
ln -s $HOME/dotfiles/.zsh_history $HOME/.zsh_history

echo "###############################################"
echo "REMOVING THE OLD .emacs FILE"
echo "###############################################"
# Link to .emacs; remove any existing file
if [[ -f "$HOME/.emacs" ]]; then
    rm -rf $HOME/.emacs
    ln -s $HOME/dotfiles/.emacs $HOME/.emacs
fi
# Link gitconfig
if [[ -L "$HOME/.gitconfig" ]]; then
    rm $HOME/.gitconfig
    ln -s $HOME/dotfiles/.gitconfig $HOME/.gitconfig
fi

if [[ ! -d "$HOME/.BUILDS" ]]; then
    mkdir -p $HOME/.BUILDS
fi

if [[ ! -d "$HOME/.BUILDS/yay" ]]; then
    git clone https://aur.archlinux.org/yay.git .BUILDS/
    yay
    pushd $HOME/.BUILDS/yay && makepkg -si --noconfirm

    popd
fi

# Install rust
echo "###############################################"
echo "Installing Rust"
echo "###############################################"


# if rustup is not a valid command then install rustup
if ! command -v rustup &> /dev/null; then
    pushd $HOME/.BUILDS
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

    source "$HOME/.cargo/env"

    rustup install stable
    rustup default stable

    popd
fi

# Echo the packages to install
echo "###############################################"
echo "Installing the following packages"
echo "###############################################"
for i in "${PACKAGES[@]}"
do
    echo "$i"
done

# Check if the packages are installed and if not install them
for i in "${PACKAGES[@]}"
do
    if ! pacman -Qi "$i" &> /dev/null; then
	yay -S "$i" --noconfirm --needed
    fi
done


# Install the latest stable version of python from source
echo "###############################################"
echo "Installing Python 3.12.0 from source"
echo "###############################################"

export PYTHON_VERSION=$(python -V)

pushd $HOME/.BUILDS

if [[ ! -d "Python-3.12.2" ]];then
    wget https://www.python.org/ftp/python/3.12.2/Python-3.12.2.tar.xz
    tar -xf Python*.tar.xz
fi
pushd $HOME/.BUILDS/Python-3.12.2

./configure --enable-optimizations --with-ensurepip=install --enable-shared --enable-profiling --enable-pystats --enable-loadable-sqlite-extensions --enable-ipv6

make -j
sudo make altinstall
popd
popd

# Install the Golang programming language
echo "###############################################"
echo "Installing Golang"
echo "###############################################"
if ! command -v go &> /dev/null; then
    pushd $HOME/.BUILDS
    wget https://golang.org/dl/go1.17.6.linux-amd64.tar.gz
    sudo tar -C /usr/local -xzf go1.17.6.linux-amd64.tar.gz
    popd
fi
echo "GOLANG is installed at /usr/local/go"


# Blocking command to show the pub key; Add keys to github repo(s)
echo "###############################################"
echo "Place the key onto GitHub"
echo "###############################################"
less $HOME/.ssh/id_ed25519_base_key.pub

# Setup Github ssh commit signing key
echo "###############################################"
echo "Setting up the ssh key for commit signing"
echo "###############################################"
git config --global user.signingkey $(ssh-keygen -lf $HOME/.ssh/id_ed25519_base_key.pub | awk '{print $2}')
git config --global commit.gpgsign true

# Evaluate the ssh-agent
echo "###############################################"
echo "Evaluating the ssh-agent"
echo "###############################################"
eval "$(ssh-agent -s)"
ssh-add $HOME/.ssh/id_ed25519_base_key

echo -n "\n\n\n"
echo "###############################################"
echo "Setup zsh and put ssh key on github"
echo "###############################################"
# Show the command to change the shell to zsh
echo "To change the shell to zsh, run the following command:"
echo "chsh -s $(which zsh)"


