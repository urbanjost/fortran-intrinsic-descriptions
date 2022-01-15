# NAME

**--runner**(1) - a shared option for specifying an application to
launch executables.

# SYNOPSIS

**fpm** run|test **--runner** *CMD* \`\`\` -- *SUFFIX\_OPTIONS*

# DESCRIPTION

The **--runner** option allows specifying a program to launch
executables selected via the **fpm**(1) subcommands "run" and "test".
This gives easy recourse to utilities such as debuggers and other tools
that wrap other executables.

These external commands are not part of **fpm**(1) itself as they vary
from platform to platform or require independent installation.

# OPTION

  - ****--runner** '*CMD*'**  
    quoted command used to launch the **fpm**(1) executables. Available
    for both the "run" and "test" subcommands. If the keyword is
    specified without a value the default command is "echo".

  - **-- *SUFFIX\_OPTIONS***  
    additional options to suffix the command *CMD* and executable file
    names with.

# EXAMPLES

Use cases for '**fpm** run|test **--runner** "*CMD*"' include employing
the following common GNU/Linux and Unix commands:

## INTERROGATE

  - nm - list symbols from object files

  - size - list section sizes and total size.

  - ldd - print shared object dependencies

  - ls - list directory contents

  - stat - display file or file system status

  - file - determine file type

## PERFORMANCE AND DEBUGGING

  - gdb - The GNU Debugger

  - valgrind - a suite of tools for debugging and profiling

  - time - time a simple command or give resource usage

  - timeout - run a command with a time limit

## COPY

  - install - copy files and set attributes

  - tar - an archiving utility

## ALTER

  - rm - remove files or directories

  - chmod - change permissions of a file

  - strip - remove unnecessary information from strippable files

For example

``` 
  fpm test --runner gdb
  fpm run --runner "tar cvfz $HOME/bundle.tgz"
  fpm run --runner ldd
  fpm run --runner strip
  fpm run --runner 'cp -t /usr/local/bin'

  # options after executable name can be specified after the -- option
  fpm --runner cp run -- /usr/local/bin/
  # generates commands of the form "cp $FILENAME /usr/local/bin/"

  # bash(1) alias example:
  alias fpm-install=\
  "fpm run --profile release --runner 'install -vbp -m 0711 -t ~/.local/bin'"
  fpm-install
```
