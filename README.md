![compilation workflow](https://github.com/frno7/cf2149/actions/workflows/compilation.yml/badge.svg)

# CF2149 programmable sound generator (PSG)

The CF2149 module in C is designed to be compatible with the [General
Instrument AY-3-8910][AY-3-8910] 3-voice programmable sound generator (PSG)
made in 1978, and the Yamaha YM2149F. These devices were used in many arcade
games and computers such as the [Atari ST] in the 1980s.

The CF2149 repository is made to be included as a Git submodule in larger
designs.

# Manuals and references

- The [AY-3-8910/8912 programmable sound generator data manual][AY-3-8910/8912 manual].
- The [YM2149 software-controlled sound generator (SSG) manual][YM2149 manual].
- The [Hatari] emulator is well-researched with extensive source code comments.
- [MiSTery] has a VHDL implementation.
- [JT49] has a Verilog implementation.

[AY-3-8910]: https://en.wikipedia.org/wiki/General_Instrument_AY-3-8910
[Atari ST]: https://en.wikipedia.org/wiki/Atari_ST

[AY-3-8910/8912 manual]: https://archive.org/embed/AY-3-8910-8912_Feb-1979
[YM2149 manual]: https://archive.org/embed/bitsavers_yamahaYM21_3070829

[Hatari]: https://github.com/hatari/hatari
[MiSTery]: https://github.com/gyurco/MiSTery
[JT49]: https://github.com/jotego/jt49
