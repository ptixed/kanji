#!/usr/bin/env bash

set -ex

VERSION=6.9.10-82

# wget http://www.imagemagick.org/download/ImageMagick-${VERSION}.tar.gz -O res/ImageMagick-${VERSION}.tar.gz

cd res
tar -xzvf ImageMagick-${VERSION}.tar.gz
cd ImageMagick-${VERSION}  && ./configure --prefix=/usr && make && sudo make install

