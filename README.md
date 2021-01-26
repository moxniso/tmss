# tmss
Full cleaned-up disassembly of Sega Genesis TMSS bootrom

## WTF is TMSS?
In 1990, video game publisher Accolade began releasing unlicensed games for the Sega Genesis using their own reverse engineered devtools. In retaliation, Sega created TMSS, the Trademark Security System, and implemented it in all Genesis consoles Model 2 and onwards. It's a small 2K bootrom which displays the "Produced by or under license from Sega Enterprises Ltd" before locking the VDP, which can then only be unlocked by the game cart by writing the string "SEGA" to address $A14000. If this is not done, the screen would simply stay black.

![The TMSS message](https://r.mprd.se/Sega%20Genesis/Titles/[BIOS]%20Genesis%20TMSS%20(U).png)

This was done to serve as a legal challenge torwards Accolade and other unlicensed publishers. As the console would falsely claim the game running was licensed by Sega if Accolade were to include the aforementioned VDP unlocking code. 

Further reading: https://en.wikipedia.org/wiki/Sega_v._Accolade

## To build a ROM:
I cleaned up the source code to follow the syntax of SNASM68k, an M68k assembler for DOS from 1995. 
You can find SNASM68K.EXE at the following links:
* Modified DOSBOX version for 64-bit Windows:
https://github.com/pascalorama/snasm68kdb/releases=

* Original DOS version:
http://devster.monkeeh.com/sega/basiegaxorz/snasm68k_202ex.zip

Run the following command in the directory where the source file is
```
SNASM68k.EXE /p tmss.asm,tmss.bin,tmss.lst,tmss.map
```
and ``TMSS.BIN`` will be outputted, which can then be used in any Genesis emulator.

