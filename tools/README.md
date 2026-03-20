# Font conversion tools

Convert font sources to C header files compatible with `FontMono1B` in `vdom/font.h`. Output `.h` files use `#include "../font.h"` and are intended to live under `vdom/font/`.

## Build

```bash
cc -o bdf2carray bdf2carray.c
cc -o bfm2carray bfm2carray.c
cc -o glyphs2carray glyphs2carray.c
cc -o pcf2carray pcf2carray.c
```

## Usage

- **bdf2carray** — BDF (Bitmap Distribution Format): `./bdf2carray input.bdf output.h`
- **bfm2carray** — BitFontMaker2 JSON: `./bfm2carray input.json output.h`
- **glyphs2carray** — FontStruct Glyphs app `.glyphs` (plist-style): `./glyphs2carray input.glyphs output.h`
- **pcf2carray** — PCF (Portable Compiled Format): `./pcf2carray input.pcf output.h`

All tools produce ASCII 32–126 (95 glyphs), monospace, glyph width ≤ 8.

## Bitmap image tools

- `pgm_quantize input.pgm [1|2|4] output.pgm`
- `image_to_elm.sh bd8_64_64_duke.png --bit-depth 8 > output.elm`

Use these together with `magick input.png output.pgm` and `magick image.png -format "%k\n" info:`