# Change Log

## [1.4.1-1]

Add and modernize the standard project files:

  * Added a CHANGELOG.md file to track the project changes.
  * Added a .gitignore file to ignore the build output.

Add autotools support:

  * Added an autogen.sh script to setup the configure script.
  * Backport configure.in from 1.4.2 as configure.ac.
  * Link to the automake files needed to run the configure script.
  * Use `autoconf` to generate the configure script.

Build system fixes:

  * `config/config.in`: Use the `ESTDIR` environment variable instead of
    hard-coding the Edinburgh Speech Tools location.

## [1.4.1]

Upstream version 1.4.1.
