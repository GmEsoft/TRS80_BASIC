TRS80-BASIC
===========



Some BASIC implementations for the TRS-80
-----------------------------------------

:construction: WORK IN PROGRESS :construction: 


This repository contains source code for several implementations of Microsoft's BASIC-80 
interpreter, incorporating original label names and comments wherever possible 
from ALTAIR BASIC and the GW-BASIC GitHub project.

I also included 2 subprojects containing the LSI (Logical System Inc. / Misosys) extensions for
the Model 4 editions of BASIC and BASICG.

All implementations can be built using [George Phillips' ZMAC](http://www.48k.ca/zmac.html).
The ZMAC assembler is expected to be extracted into the `zmac` subfolder.

The following BASIC versions are included:


### Model I Level II BASIC

In the subfolder `LEVEL2`. Can build Level II BASIC versions known as `1.00`, `1.02` (`MEMORY SIZE?`)
and `1.03` (`MEM SIZE?`).


### Model 4 TRSDOS 6.2 Disk BASIC and Hi-Res BASICG

In the subfolder `BASIC-BASICG`. Can build TRSDOS 6.2 Disk BASIC and BASICG v:`01.01.00`. Including
`bash` scripts to split the binary into `BASIC[G].CMD` and BASIC[G].OV1`.


### Model 4 LS-DOS 6.3 Disk BASIC with LSI enhancements

In the subfolder `BASIC-OV2`. Version `01.01.02`. From the now-archived Frank Durda IV LS-DOS reconstruction 
project.

The original files have been fixed to allow a successful build of the LSI-enhanced BASIC.


### Model 4 LS-DOS 6.3 Hi-Res BASICG with LSI enhancements

In the subfolder `BASICG-OV2`. Version `01.01.03`. From the now-archived Frank Durda IV LS-DOS reconstruction 
project. Adapted for BASICG (I think there has never been an LSI-enhanced version of BASICG).

I added a new feature to support overlaid ('Intermix') text and hi-res mode: `SCREEN 2`. The start-up
code contains an addition to identify the type of board 'Grafyx' vs. 'Tandy' and select the appropriate
port to enable Intermix.


### CP/M `MBASIC.COM` version `5.21`

In the subfolder `MBASIC`. Also included a `MMBASIC.ASM` file, demonstrating how additional statements
can be added to the original MBASIC interpreter, in this example: `CLS`, `LOCATE` and `COLOR`,
supported by Montezuma Micro CP/M BIOS 2.30 and higher.



Legal Notice
------------

This repository contains a derivative work based on multiple publicly available sources. It does not 
represent the original source code in its authentic form. No claim of ownership is made over the 
original code; all rights remain with the respective original authors and copyright holders.



License
-------

    MIT License

    Copyright (c) Microsoft Corporation.

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE



External Links
--------------

- ALTAIR BASIC:
  - Scanned Source Listing Printout: 
    - https://www.gatesnotes.com/meet-bill/source-code/reader/microsoft-original-source-code
    - https://images.gatesnotes.com/12514eb8-7b51-008e-41a9-512542cf683b/34d561c8-cf5c-4e69-af47-3782ea11482e/Original-Microsoft-Source-Code.pdf

- GW-BASIC
  - Github: https://github.com/microsoft/GW-BASIC
  - Microsoft Dev Blogs: https://devblogs.microsoft.com/commandline/microsoft-open-sources-gw-basic/

- NASCOM BASIC:
  - Home page: http://www.nascomhomepage.com/
  - Mirror: https://tommythorn.github.io/nascomhomepage.com/
  - Feilipu: https://github.com/feilipu/NASCOM_BASIC_4.7

- GRANT SEARLE BASIC:
  - Home page: http://searle.wales/
  - Simple Z80: http://searle.x10host.com/sbcZ80.jpg

- MSX BASIC:
  - Github: https://github.com/z88dk/techdocs/tree/master/targets/msx

- CP/M MBASIC:
  - Seems directly derived from Microsoft source code, including the symbols ...

    https://github.com/z88dk/techdocs/tree/master/targets/cpm



Acknowledgements and Credits
----------------------------

- **Microsoft**, for making the source codes of ALTAIR BASIC and GW-BASIC generally available to the public;
- **Philip Stevens** 'feilipu' for the source of NASCOM BASIC 4.7;
- Members of **z88dk** mainly for the MSX BASIC especially the Hi-Res code, also for the CP/M MBASIC;
- **Jack Decker**'s 'TRS-80 Rom Routines Documented (The Alternate Source)';
- **Frank Durda IV** for the LS-DOS 6.3.1 Source Code Restoration Project;
- **Tim Mann** for making TRS-80 resources available to the community;
- **George Phillips** for the ZMAC Z-80 Macro Assembler.

Many thanks to all of them!
