#! /bin/bash

XARGS="-compress None -define tga:image-origin=TopLeft"
RESIZE="-resize 50%"


convert $RESIZE $XARGS debug-0.tga debug-1.tga
convert $RESIZE $XARGS debug-1.tga debug-2.tga
convert $RESIZE $XARGS debug-2.tga debug-3.tga
convert $RESIZE $XARGS debug-3.tga debug-4.tga
convert $RESIZE $XARGS debug-4.tga debug-5.tga
convert $RESIZE $XARGS debug-5.tga debug-6.tga
convert $RESIZE $XARGS debug-6.tga debug-7.tga
convert $RESIZE $XARGS debug-7.tga debug-8.tga
convert $RESIZE $XARGS debug-8.tga debug-9.tga
convert $RESIZE $XARGS debug-9.tga debug-10.tga

