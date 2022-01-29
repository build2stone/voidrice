#!/bin/sh
if [ -n "$FIFO_UEBERZUG" ]; then

  hash() {
    printf '%s/.cache/lf/%s' "$HOME" \
      "$(stat --printf '%n\0%i\0%F\0%s\0%W\0%Y' -- "$(readlink -f "$1")" | sha256sum | awk '{print $1}')"
  }

  # Don't skip cleanup if previous ($1) and current ($f) file are the same
  if [ "$1" != "$f" ]; then
    # Skip cleanup if current file has cached preview
    if [ -f "$(hash "$f").jpg" ]; then
      exit
    fi
    # Skip cleanup if current file can be previewed
    case "$(file -Lb --mime-type -- "$f")" in
      image/*|video*) exit;;
      application/zip)
        case "${f##*.}" in
          cbz|kra) exit;;
        esac
        ;;
    esac
  fi

  printf '{"action": "remove", "identifier": "preview"}\n' >"$FIFO_UEBERZUG"
fi
