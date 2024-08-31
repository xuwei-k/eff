#!/bin/bash -xe

PANDOC_FILE_NAME="pandoc-3.3-1-amd64.deb"
wget "https://github.com/jgm/pandoc/releases/download/3.3/${PANDOC_FILE_NAME}"
sudo dpkg -i "${PANDOC_FILE_NAME}"
pandoc --version
