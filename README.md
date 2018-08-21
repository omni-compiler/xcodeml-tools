README

## How to install
    $ ./configure --prefix=(INSTALL PATH)
    $ make
    $ make install
    $ export PATH=(INSTALL PATH)/bin:$PATH

## Run
    $ C_Front a.c -o a.xml
    $ C_Back  a.xml

    $ F_Front a.f90 -o a.xml
    $ F_Back  a.xml

[![Build Status](https://travis-ci.org/omni-compiler/xcodeml-tools.svg?branch=master)](https://travis-ci.org/omni-compiler/xcodeml-tools)
