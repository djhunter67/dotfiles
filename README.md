# Hunter's dotfiles

## Description
- The configuration of several software related programs.
  - Emacs
  - Git
  - Zsh
  - some helper scripts


## Installation
- Clone the repository
  - `git clone git@git.com/djhunter67/dotfiles.git`
- Run the install script
  - `./dot_init.sh` -- Especially for Arch based distros
  
### Install script 
The install script will check for the existence of various folder/directories.
If they do not exist, they will be created and symbolic links will be created
to the dotfiles in this repository.

The install script will also install several packages that are needed for the
developnemt environment I am accustomed to using.  These packages are listed
in the `dot_init.sh` script.

### kernel_check
This script will check for a new kernel --after and update-- and will print
a message if a new kernel is available.  This script will also print all of the 
packages that have been updated since the last update.

### parse_git_log
This script will parse the output of `git log` and will print the number of
all of the commit messages in the repo.

### log_read
This script will read the output of a log file and print in color to stdout.








