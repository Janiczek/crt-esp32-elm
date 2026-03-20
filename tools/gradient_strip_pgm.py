#!/usr/bin/env python3
"""
Write a 256×H binary PGM (P5): horizontal grey ramp 0..255, identical on every row.
"""

from __future__ import annotations

import argparse
import sys


def main() -> None:
    parser = argparse.ArgumentParser(
        description="256 px wide greyscale gradient strip (0..255) as PGM P5."
    )
    parser.add_argument(
        "height",
        type=int,
        help="Image height in pixels (width is always 256)",
    )
    parser.add_argument(
        "-o",
        "--output",
        metavar="PATH",
        help="Output file (default: gradient_<height>x256.pgm in cwd)",
    )
    args = parser.parse_args()

    if args.height < 1:
        print("height must be >= 1", file=sys.stderr)
        sys.exit(1)

    out_path = args.output or f"gradient_{args.height}x256.pgm"
    row = bytes(range(256))
    header = f"P5\n256 {args.height}\n255\n".encode("ascii")

    with open(out_path, "wb") as f:
        f.write(header)
        for _ in range(args.height):
            f.write(row)


if __name__ == "__main__":
    main()
