# Uncensored by Booze Design (Dissassembly)

This is a partial disassembly of the C64 demo [Uncensored](https://csdb.dk/release/?id=133934) by Booze Design. I started on this in 2017 and only got through commenting the bootloader and the first chain of the trackmo.  In late 2021 I found the files and decided to put them on GitHub. 

Hopefully Booze don't mind me sharing this. I'm a huge fan and I'm just trying to learn from the masters and help others do the same.  I hope to finish the disassembly someday and get to the interesting parts of the demo.  I have already learned a ton about trackmos and drive code from this.

## Notes

The demo is compressed using [ByteBoozer 1.1](https://csdb.dk/release/?id=109317). It uses the [BoozeLoader](https://csdb.dk/release/?id=145208) trackmo loader and [Disk](https://csdb.dk/release/?id=145209) utility by HCL/Booze Design.

Originally, I extracted the individual sectors of the first few trackmo chains from the d64 image using VICE's [c1541](https://vice-emu.sourceforge.io/vice_14.html) utility and combined them using cat and [xxd](https://linux.die.net/man/1/xxd). I decompressed the bootloader and the trackmo chains using the [debooze.c](https://gist.github.com/jblang/9fd38f21d64d1cba4c5d549733219d93) program I translated from the original decruncher in 6502 assembly. I disassembled the decrunched code using [Regenerator](https://csdb.dk/release/?id=149429) by Nostalgia. I did the original label editing using Regenerator and then did further manual editing in a text editor.  I have verified that the sources reassemble to the original binary using 64tass.

This involved a lot of manual work, so I have recently written some [demotools](https://github.com/jblang/demotools), which fully automate extraction and decrunching of the files and trackmo chains from the D64 image.
