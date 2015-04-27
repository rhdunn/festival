# The Festival Speech Synthesis System

- [Compilation](#compilation)
- [Installation](#installation)
- [License](#license)

----------

This directory contains the
[Festival Speech Synthesis System](http://www.cstr.ed.ac.uk/projects/festival.html),
developed at CSTR, University of Edinburgh. The project was originally
started by Alan W Black and Paul Taylor but [many others](ACKNOWLEDGMENTS)
have been involved.

Festival offers a general framework for building speech synthesis
systems as well as including examples of various modules.  As a whole
it offers full text to speech through a number APIs: from shell level,
though a Scheme command interpreter, as a C++ library, and an Emacs
interface.  Festival is multi-lingual (currently English (US and UK)
and Spanish are distributed but a host of other voices have been
developed by others) though English is the most advanced.

The system is written in C++ and uses the Edinburgh Speech Tools
for low level architecture and has a Scheme (SIOD) based command
interpreter for control.  Documentation is given in the FSF texinfo
format which can generate, a printed manual, info files and HTML.

## Compilation

In order to build festival, the Edinburgh Speech Tools project needs to
be built. The location of build tree of these tools needs to be specified
in the `ESTDIR` environment variable. For example:

    export ESTDIR=`pwd`/../speech_tools

The `festival` project uses a standard autogen-based build system. It
can be built using the following commands:

    ./autogen.sh
    ./configure --prefix=/usr
    make
    sudo make install

## Installation

Festival should run on any standard Unix platform.  It has already run
on Solaris, SunOS, Linux and FreeBSD.  It requires a C++ compiler (GCC
2.7.2, 2.8.1, 2.95.[123], 3.2.3 3.3.2 RedHat "gcc-2.96" and gcc 3.3,
3.4, 4.0, 4.0.1 4.1 are our standard compilers) to install. A port to Windows
XP/NT/95/98 and 2000 using either Cygnus GNUWIN32 and Visual C++ is
included, this is still new but many people are successfully using it.

A detailed description of installation and requirements for the whole
system is given in the [INSTALL](INSTALL) file.

## License

Festival is free. Earlier versions were restricted to non-commercial
use but we have now relaxed those conditions.  The [4-clause BSD licence](COPYING)
is an X11 style licence thus it can be incorporated in commercial products
and free source products without restriction. It has the following copyright:

    Centre for Speech Technology Research
    University of Edinburgh, UK
    Copyright (c) 1996-2004
    All Rights Reserved.

The changes to the project are described in the [CHANGELOG.md](CHANGELOG.md)
file in order to comply with clause 2 of the BSD license. This also includes
the changes in the official festival releases.
