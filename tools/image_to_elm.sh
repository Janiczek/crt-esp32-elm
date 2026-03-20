#!/usr/bin/env bash
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

if [ "$#" -lt 3 ]; then
  echo "Usage: $0 input-image --bit-depth N" >&2
  exit 1
fi

if [ ! -x "$script_dir/pgm_quantize" ]; then gcc "$script_dir/pgm_quantize.c" -o "$script_dir/pgm_quantize"; fi
if [ ! -x "$script_dir/pgm_to_elm"   ]; then gcc "$script_dir/pgm_to_elm.c"   -o "$script_dir/pgm_to_elm"; fi

input="$1"
shift

bit_depth=""
stem=""

while [ "$#" -gt 0 ]; do
  case "$1" in
    --bit-depth)
      bit_depth="$2"
      shift 2
      ;;
    *)
      echo "Unknown argument: $1" >&2
      exit 1
      ;;
  esac
done

if [ -z "$bit_depth" ]; then
  echo "Missing required arguments" >&2
  exit 1
fi

stem=$(basename -- "$input")
stem=${stem%.*}

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

mkdir -p "$tmpdir/pgm8" "$tmpdir/quantized"
pgm8="$tmpdir/pgm8/$stem.pgm"
quantized="$tmpdir/quantized/$stem.pgm"

magick "$input" -alpha off -colorspace gray -depth 8 "PGM:$pgm8" >/dev/null

if [ "$bit_depth" = "8" ]; then
  final_pgm="$pgm8"
else
  "$script_dir/pgm_quantize" "$pgm8" "$bit_depth" "$quantized" >/dev/null
  final_pgm="$quantized"
fi

"$script_dir/pgm_to_elm" "$final_pgm" --bit-depth "$bit_depth"
