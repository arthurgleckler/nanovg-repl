#!/bin/bash

IMAGE=-1
WIDTH=800
HEIGHT=$WIDTH

loop () {
  ANGLE=`echo "$1 * 0.0042"|bc -l`
  HALF_WIDTH=`echo "$WIDTH/2"|bc -l`
  HALF_HEIGHT=`echo "$HEIGHT/2"|bc -l`

  echo "frame-buffer-size"
  read FB_WIDTH FB_HEIGHT
  echo "window-size"
  read WIN_WIDTH WIN_HEIGHT

  PXRATIO=`echo "$FB_WIDTH / $WIN_WIDTH"|bc -l`

  echo "viewport 0 0 $FB_WIDTH $FB_HEIGHT"
  echo "clear-color 0 0 0 1"
  echo "clear 1 1 1"
  echo "begin-frame $WIN_WIDTH $WIN_HEIGHT $PXRATIO"
  echo "translate $HALF_WIDTH $HALF_HEIGHT"
  echo "rotate $ANGLE"
  echo "translate -$HALF_WIDTH -$HALF_HEIGHT"
  echo "scale 12 12"
  echo "image-pattern 0 0 79.8 60 0 $IMAGE 1"
  read IMAGE_PAINT
  echo "fill-paint $IMAGE_PAINT"
  echo "begin-path"
  echo "rounded-rect 0 0 $WIDTH $HEIGHT 5"
  echo "fill-paint $IMAGE_PAINT"
  echo "fill"
  echo "unregister $IMAGE_PAINT"
  echo "end-frame"
  echo "swap-buffers"
  echo "poll-events"
}

initialize() {
  echo "create-image 22 ../examples/balisp.png 6"
  read IMAGE
}

windowShouldClose() {
  echo "window-should-close?"
  read windowShouldClose
  return $windowShouldClose
}

image() {
  let t=0

  initialize
  while (windowShouldClose)
  do
    let t++
    loop $t
    processEvents
  done
  echo "shutdown"
}

processEvents() {
  while (read -t 0 < $PRIVATE/events)
  do
    read -a words < $PRIVATE/events
    if [ ${words[0]} == "key-input" ]; then
      >&2 echo ${words[*]}
    elif [ ${words[0]} == "mouse-button" ]; then
      >&2 echo ${words[*]}
      if [ ${words[2]} == "0" ]; then
	echo "mouse-position-events 0"
      else
	echo "mouse-position-events 1"
      fi
    elif [ ${words[0]} == "mouse-position" ]; then
      >&2 echo ${words[*]}
    elif [ ${words[0]} == "text-input" ]; then
      >&2 echo ${words[*]}
    elif [ ${words[0]} == "window-size-changed" ]; then
      WIDTH=${words[1]}
      HEIGHT=$WIDTH
    fi
  done
}

cleanup() {
  if [ $IMAGE != -1 ]; then
    echo "delete-image $IMAGE"
  fi
  rm -rf $PRIVATE/
}

trap cleanup EXIT

PRIVATE=`mktemp -d`

mkfifo -m 600 $PRIVATE/events
mkfifo -m 600 $PRIVATE/responses

image \
  < $PRIVATE/responses \
  | ./repl \
      $WIDTH \
      $HEIGHT \
      image \
      $PRIVATE/events \
      > $PRIVATE/responses