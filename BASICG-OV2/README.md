LS-DOS 6.3.1 BASICG 01.01.03 (c) 1984 by Microsoft with LSI enhancements
========================================================================


This version of the BASICG interpreter, originally published by Tandy, extends 
the standard BASIC interpreter with support for high-resolution graphics. It 
has now been further upgraded to incorporate the LSI enhancements previously 
available only in the non-hires edition of BASIC.

These LSI enhancements introduce a range of code editing features, including:
- Arrow key navigation within BASIC program code;
- One-key editing mode for streamlined input;
- An improved RENUM command;
- Abbreviated forms of key commands such as LIST, DELETE, AUTO, EDIT, and SYSTEM;
- Additional utilities like Copy, Mode, Find, and Search for enhanced workflow.

Those LSI enhancements changed the BASIC version from 01.01.00 to 01.01.02.

The original source code of the enhancements to the stock BASIC interpreted
were put to public domain by Frank Durda IV on his site at this 
[address](http://archives.oldskool.org/pub/drivers/Tandy/nemesis.lonestar.org/computers/tandy/software/os/logical_systems/lsdos6/src631/utilities_index.html)
which is part of his "LS-DOS 6.3.1 Source Code Restoration Project". See section
"LS-DOS 6.3.1 BASIC (/CMD)".

> Items for April and June 2000:
>
> [**LS-DOS 6.3.1 Restoration Project**](http://archives.oldskool.org/pub/drivers/Tandy/nemesis.lonestar.org/computers/tandy/software/os/logical_systems/lsdos6/src631/)
>
> A complete operating system and utility suite (with source code) for Z80 
processors that was commercially sold in binary-only form for the Tandy/Radio 
Shack Model 4 series of computers in the 1980s. Now released to the public 
domain, this is the original source code, along with a reconstruction of the 
final version distributed by the developers.
>
> COMPUTER SOFTWARE, TANDY, MODEL 4, TRSDOS 6/LS-DOS 6 Z80 ASSEMBLER


> [**LS-DOS 6.3.1 BASIC (/CMD)**](http://archives.oldskool.org/pub/drivers/Tandy/nemesis.lonestar.org/computers/tandy/software/os/logical_systems/lsdos6/src631/utilities_index.html)
>
>As part of the LS-DOS 6.3.x releases, a modified version of the Model 4 BASIC 
from the TRSDOS 6.2 release was included. This version added some additional 
commands and also required one additional code overlay, BASIC/OV2. The various 
files needed to construct the 6.3.1 version of BASIC are listed here.
>
> The source code for these modifications to BASIC were not published in 
"The Source" for TRSDOS 6.2.0.

The original site (http://nemesis.lonestar.org) seems now 
[archived](http://archives.oldskool.org/pub/drivers/Tandy/nemesis.lonestar.org/) 
and no longer maintained. A copyright statement tells this:

> [Copyright 1999,2002,2004 Frank Durda IV, All Rights Reserved.
Mirroring of any material on this page in any form is expressly prohibited.
The official web site for this material is:  http://nemesis.lonestar.org
Contact this address for use clearances: clearance at nemesis.lonestar.org
Comments and queries to this address: web_software_2011 at nemesis.lonestar.org]


SCREEN 2
--------

One additional enhancement has also been added to support the text-graphics
overlay mode a.k.a. the "Intermix" mode. This "Intermix" mode is not documented
in the "Model 4 Computer Graphics" (26-1126) manual, but is well documented
in the "Service Manual" for the "TRS-80 Model 4P, 4P Gate Array" (26-1080),
page 189. This manual indicates that the bit 0 of the port 8Eh when set to 1,
enables the mixing of text and graphics on the display, with a resolution of 
640x260 in 80x24 text mode, and 512x192 in 64x16 mode. This "Intermix" mode
is also possible with the 3rd-party Grafyx hi-res adapter, but using another
port, the bit 1 of port 83h, but this time when set to 0. This "Intermix" mode
can be enabled from BASICG with the enhanced command SCREEN 2.

This enhancement moves the BASICG version to 01.01.03.


Additional note
---------------

There is one undocumented option to the `PSET`, `PRESET`, `LINE` and `GET` 
commands in BASICG to enable a "relative" move from the start or current pixel 
position to a new relative location, by including STEP before the destination 
coordinate. The syntax is:

```BASIC
	PSET STEP(xd,yd)
	PRESET STEP(xd,yd)
	GET (x1,y1)-STEP(xd,yd),ARRAY%
	LINE (x1,y1)-STEP(xd,yd)[,options...]
	LINE -STEP(xd,yd)[,options...]
```

The first form of `LINE` is equivalent to:
```BASIC
	LINE (x1,y1)-(x1+xd,y1+yd)[,options...]
```

Example:
```basic
10 SCREEN:CLR            ' 0 is optional
20 PSET (50, 50)         ' Top left corner
30 LINE -STEP(100, 0)    ' Top edge
40 LINE -STEP(0, 100)    ' Right edge
50 LINE -STEP(-100, 0)   ' Bottom edge
60 LINE -STEP(0, -100)   ' Left edge
70 A$=INPUT$(1)
```

It must be also noted that this undocumented STEP option to the aforementioned
commands is unrecognized by the BASIC Compiler from Microsoft also published 
by Tandy (BASCOM4 - 26-2218, MS version 5.35, R/S version 01.00.00). Nor is 
`SCREEN 2`, of course!


Credits
-------

Credits go to:

- Microsoft, of course, for authoring both BASIC and BASICG;
- Microsoft again, for releasing the original ALTAIR BASIC and GW-BASIC source
  codes, helping me understand how the BASIC and BASICG interpreters work inside
  (the hi-res extensions included in BASICG are indeed a subset of GW-BASIC,
  including the undocumented `STEP` option to `LINE`, `PSET`, `PRESET` and `GET`!);
- Frank Durda IV, for releasing the source code of the LST enhancements to the
  public domain.

