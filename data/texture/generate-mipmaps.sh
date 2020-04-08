#! /bin/bash

XARGS="-compress Zip"
RESIZE="-resize 50%"


convert $RESIZE $XARGS debug-0.tiff debug-1.tiff
convert $RESIZE $XARGS debug-1.tiff debug-2.tiff
convert $RESIZE $XARGS debug-2.tiff debug-3.tiff
convert $RESIZE $XARGS debug-3.tiff debug-4.tiff
convert $RESIZE $XARGS debug-4.tiff debug-5.tiff
convert $RESIZE $XARGS debug-5.tiff debug-6.tiff
convert $RESIZE $XARGS debug-6.tiff debug-7.tiff
convert $RESIZE $XARGS debug-7.tiff debug-8.tiff
convert $RESIZE $XARGS debug-8.tiff debug-9.tiff
convert $RESIZE $XARGS debug-9.tiff debug-10.tiff
