#!/usr/bin/env bash

set -ex

VERSION=6.9.10-82

wget http://www.imagemagick.org/download/ImageMagick-${VERSION}.tar.gz
tar -xzvf ImageMagick-${VERSION}.tar.gz
cd ImageMagick-${VERSION}  && ./configure --prefix=/usr && make && sudo make install

