#!/bin/bash

WIDTH=800
HEIGHT=$WIDTH
STROKE_WIDTH=`echo "1/50"|bc -l`

loop () {
  ANGLE=`echo "$1 * 0.01"|bc -l`
  HALF_WIDTH=`echo "$WIDTH/2"|bc -l`
  HALF_HEIGHT=`echo "$HEIGHT/2"|bc -l`
  TWELFTH_WIDTH=`echo "$WIDTH/12"|bc -l`
  TWELFTH_HEIGHT=`echo "$HEIGHT/12"|bc -l`

  echo "frame-buffer-size"
  read FB_WIDTH FB_HEIGHT
  echo "window-size"
  read WIN_WIDTH WIN_HEIGHT

  PXRATIO=`echo "$FB_WIDTH / $WIN_WIDTH"|bc -l`
  echo "viewport 0 0 $FB_WIDTH $FB_HEIGHT"
  echo "clear-color 0 0 0 1"
  echo "clear 1 1 1"
  echo "begin-frame $WIN_WIDTH $WIN_HEIGHT $PXRATIO"
  echo "font-face 4 sans"
  echo "font-size 50"
  echo "text 10 50 21 Ὀστομάχιον"
  read textResult
  echo "translate $HALF_WIDTH $HALF_HEIGHT"
  echo "rotate $ANGLE"
  echo "translate -$HALF_WIDTH -$HALF_HEIGHT"
  echo "scale $TWELFTH_WIDTH $TWELFTH_HEIGHT"
  echo "radial-gradient 6 6 0 6 1 0 0 255 255 0 0 200"
  read GRADIENT1
  echo "radial-gradient 6 6 0 6 0 1 0 255 0 255 0 200"
  read GRADIENT2
  echo "radial-gradient 6 6 0 6 0 0 1 255 0 0 255 200"
  read GRADIENT3
  echo "fill-paint $GRADIENT1"
  echo "begin-path"
  echo "move-to 0 0"
  echo "line-to 6 0"
  echo "line-to 4 4"
  echo "line-to 0 0"
  echo "fill"
  echo "fill-paint $GRADIENT2"
  echo "begin-path"
  echo "move-to 0 0"
  echo "line-to 2 2"
  echo "line-to 0 12"
  echo "line-to 0 0"
  echo "fill"
  echo "fill-paint $GRADIENT3"
  echo "begin-path"
  echo "move-to 2 2"
  echo "line-to 4 4"
  echo "line-to 0 12"
  echo "line-to 2 2"
  echo "fill"
  echo "unregister $GRADIENT1"
  echo "unregister $GRADIENT2"
  echo "unregister $GRADIENT3"
  echo "fill-color 127 0 0 200"
  echo "begin-path"
  echo "move-to 0 12"
  echo "line-to 3 12"
  echo "line-to 2 8"
  echo "line-to 0 12"
  echo "fill"
  echo "fill-color 0 127 0 200"
  echo "begin-path"
  echo "move-to 3 6"
  echo "line-to 3 12"
  echo "line-to 2 8"
  echo "line-to 3 6"
  echo "fill"
  echo "fill-color 0 0 127 200"
  echo "begin-path"
  echo "move-to 3 6"
  echo "line-to 4 4"
  echo "line-to 6 6"
  echo "line-to 6 12"
  echo "line-to 3 12"
  echo "line-to 3 6"
  echo "fill"
  echo "fill-color 63 0 0 200"
  echo "begin-path"
  echo "move-to 6 0"
  echo "line-to 6 6"
  echo "line-to 4 4"
  echo "line-to 6 0"
  echo "fill"
  echo "fill-color 0 63 0 200"
  echo "begin-path"
  echo "move-to 6 0"
  echo "line-to 9 6"
  echo "line-to 8 8"
  echo "line-to 6 6"
  echo "line-to 6 0"
  echo "fill"
  echo "fill-color 0 0 63 200"
  echo "begin-path"
  echo "move-to 6 6"
  echo "line-to 8 8"
  echo "line-to 6 12"
  echo "line-to 6 6"
  echo "fill"
  echo "fill-color 192 0 0 200"
  echo "begin-path"
  echo "move-to 9 6"
  echo "line-to 12 12"
  echo "line-to 8 8"
  echo "line-to 9 6"
  echo "fill"
  echo "fill-color 0 192 0 200"
  echo "begin-path"
  echo "move-to 8 8"
  echo "line-to 12 12"
  echo "line-to 6 12"
  echo "line-to 8 8"
  echo "fill"
  echo "fill-color 0 0 192 200"
  echo "begin-path"
  echo "move-to 6 0"
  echo "line-to 12 0"
  echo "line-to 12 4"
  echo "line-to 9 6"
  echo "line-to 6 0"
  echo "fill"
  echo "fill-color 255 0 0 200"
  echo "begin-path"
  echo "move-to 9 6"
  echo "line-to 12 4"
  echo "line-to 12 6"
  echo "line-to 9 6"
  echo "fill"
  echo "fill-color 0 255 0 200"
  echo "begin-path"
  echo "move-to 9 6"
  echo "line-to 12 12"
  echo "line-to 8 8"
  echo "line-to 9 6"
  echo "fill"
  echo "fill-color 0 0 255 200"
  echo "begin-path"
  echo "move-to 9 6"
  echo "line-to 12 6"
  echo "line-to 12 12"
  echo "line-to 9 6"
  echo "fill"
  echo "stroke-color 255 255 255 255"
  echo "stroke-width $STROKE_WIDTH"
  echo "begin-path"
  echo "move-to 0 0"
  echo "line-to 12 0"
  echo "line-to 12 12"
  echo "line-to 0 12"
  echo "line-to 0 0"
  echo "line-to 12 12"
  echo "line-to 6 0"
  echo "line-to 0 12"
  echo "line-to 2 2"
  echo "move-to 3 6"
  echo "line-to 3 12"
  echo "line-to 2 8"
  echo "move-to 6 0"
  echo "line-to 6 12"
  echo "line-to 9 6"
  echo "line-to 12 4"
  echo "move-to 9 6"
  echo "line-to 12 6"
  echo "stroke"
  echo "end-frame"
  echo "swap-buffers"
  echo "poll-events"
}

initialize() {
  echo "create-font 4 sans 42 ../examples/subset-GFSNeohellenic-Bold.ttf"
  read fontId
  echo "mouse-button-events 1"
  echo "window-size-change-events 1"
}

windowShouldClose() {
  echo "window-should-close?"
  read windowShouldClose
  return $windowShouldClose
}

stomachion() {
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
  rm -rf $PRIVATE/
}

trap cleanup EXIT

PRIVATE=`mktemp -d`

mkfifo -m 600 $PRIVATE/events
mkfifo -m 600 $PRIVATE/responses

stomachion \
  < $PRIVATE/responses \
  | ./repl \
      $WIDTH \
      $HEIGHT \
      Stomachion \
      $PRIVATE/events \
      > $PRIVATE/responses