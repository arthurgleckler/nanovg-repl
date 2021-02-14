#!/bin/bash

HEIGHT=1000
WIDTH=1000

loop () {
  STEP=$((($1 % 100) + 1))
  STROKE_WIDTH=$((($1 % 10) + 1))

  echo "frame-buffer-size"
  read FB_WIDTH FB_HEIGHT
  echo "window-size"
  read WIN_WIDTH WIN_HEIGHT

  PXRATIO=`echo "$FB_WIDTH / $WIN_WIDTH"|bc -l`
  echo "viewport 0 0 $FB_WIDTH $FB_HEIGHT"
  echo "clear-color 0 1 0 1"
  echo "clear 1 1 1"
  echo "begin-frame $WIN_WIDTH $WIN_HEIGHT $PXRATIO"
  echo "rotate .05"
  echo "stroke-color 255 0 0 255"
  echo "stroke-width $STROKE_WIDTH"
  echo "begin-path"
  for (( X=0; X<=$WIDTH; X+=$STEP ))
  do
    echo "move-to $X 0"
    echo "line-to $((1000 - X)) 1000"
  done
  for (( Y=0; Y<=$HEIGHT; Y+=$STEP ))
  do
    echo "move-to 0 $Y"
    echo "line-to 1000 $((1000 - Y))"
  done
  echo "stroke"
  echo "end-frame"
  echo "swap-buffers"
  echo "poll-events"
}

windowShouldClose() {
  echo "window-should-close?"
  read windowShouldClose
  return $windowShouldClose
}

moire() {
  let t=0

  while (windowShouldClose)
  do
    let t++
    loop $t
  done
  echo "shutdown"
}

cleanup() {
  rm -rf $PRIVATE/
}

trap cleanup EXIT

PRIVATE=`mktemp -d`

mkfifo -m 600 $PRIVATE/events
mkfifo -m 600 $PRIVATE/responses

moire \
  < $PRIVATE/responses \
  | ../build/repl \
      800 \
      800 \
      Moire \
      $PRIVATE/events \
      > $PRIVATE/responses