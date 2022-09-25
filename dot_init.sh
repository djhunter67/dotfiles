#!/usr/bin/bash

# Make the work directory
pushd ~/Documents 
[[ ! -d "work_worK_woRk_wOrk_Work_WORK" ]] && mkdir work_worK_woRk_wOrk_Work_WORK;

popd  # go to  home directory

# Make the development directory
[[ ! -d "dev" ]] && mkdir dev

# Create the .BUILD directory
[[ ! -d ".BUILDS" ]] && mkdir .BUILDS

