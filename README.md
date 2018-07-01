# Amstrad for MiST board
This core is refactored and cleaned version of [CoreAmstrad by Renaud Hélias](https://github.com/renaudhelias/CoreAmstrad).

It's been done in order to understand the core and port it to other platform.

## Features
Besides the refactoring, this version has completely rewritten Floppy Disk Controller allowing real write to floppy.
u765 by Gyorgy Szombathelyi is used as FDC. Core doesn't access SD card directly and thus additional
restrictions like SD card type, file system type and fragmentation are not applicable anymore.

Unlike original CoreAmstrad, this core uses consolidated ROM (amstrad.rom) which is hardcoded to use lowROM + highROM0 + highROM7.
The ROM part probably need to be improved in order to use custom ROMs split across separate parts. It will depends on future requests.

## Disk support
Put some *.DSK files to SD card and mount it from OSD menu.
important Basic commands:
* cat - list the files on mounted disk.
* run" - load and start the program. ex: run"equinox

There is currently known issue: after mounting the disk, you need to reset the Core (through OSD or CTRL+LALT+DEL/RALT).
