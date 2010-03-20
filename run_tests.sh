#!/bin/bash -e
golden=0
[[ $1 == "--golden" ]] && { golden=1; shift; }
[[ $# != 0 ]] && { echo "Usage: $0 [--golden]"; exit 1; }
parse() { runghc Parse "$@" ; }
for f in tests/*.golden; do
  test_input="${f/.golden}"
  diff -u "$f" <(parse < $test_input) && echo PASS || {
    [[ $golden == 1 ]] && {
      echo "Rewriting the golden file: $f"
      parse < $test_input > $f
    } || echo FAIL
  }
done
