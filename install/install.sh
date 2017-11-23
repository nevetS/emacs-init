#!/bin/bash
cd ..
git submodule init
git submodule update
cd install

cd pymacs-rope/Pymacs
./install-pymacs.sh

cd ../../../plugins/pymacs
make

