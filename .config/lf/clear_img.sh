#!/bin/sh
if [ -n "$FIFO_UEBERZUG" ]; then

  if [ "$1" != "$f" ]; then
    case "$(file -Lb --mime-type -- "$f")" in
      image/*) exit;;
    esac
  fi

  printf '{"action": "remove", "identifier": "preview"}\n' >"$FIFO_UEBERZUG"
fi
