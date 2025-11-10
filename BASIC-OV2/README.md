LS-DOS 6.3.1 BASIC 01.01.02 (c) 1984 by Microsoft with LSI enhancements
=======================================================================


Here's a version of the BASIC interpreter published by Tandy, as an extension
of the stock BASIC interpreter enhanced to include the LSI enhancements.

Those LSI enhancements add some code editing facilities, like arrow keys to
navigate in the BASIC program code, one-key editing mode, enhanced RENUM,
abbreviated commands for LIST, DELETE, AUTO, EDIT and SYSTEM, and some other 
facilities like Copy, Mode, Find and Search.

Those LSI enhancements changed the BASIC version from 01.01.00 to 01.01.02.

The original source code of the enhancements to the stock BASIC interpreter
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



Credits
-------

Credits go to:

- Microsoft, obviously, for authoring both BASIC and BASICG;
- Microsoft again, for releasing the original ALTAIR BASIC and GW-BASIC source
  codes, helping me understand how the BASIC and BASICG interpreters work inside
  (the hi-res extensions included in BASICG are indeed a subset of GW-BASIC,
  including the undocumented `STEP` option to `LINE`, `PSET`, `PRESET` and `GET`!);
- Frank Durda IV, for releasing the source code of the LST enhancements to the
  public domain.

