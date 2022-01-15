# NAME

**fpm**(1) - A Fortran package manager and build system

# SYNOPSIS

**fpm** *SUBCOMMAND* \[SUBCOMMAND\_OPTIONS\]

**fpm** **--help**|**--version**|**--list**

# DESCRIPTION

**fpm**(1) is a package manager that helps you create Fortran projects
from source -- it automatically determines dependencies\!

Most significantly **fpm**(1) lets you draw upon other **fpm**(1)
packages in distributed **git**(1) repositories as if the packages were
a basic part of your default programming environment, as well as letting
you share your projects with others in a similar manner.

All output goes into the directory "build/" which can generally be
removed and rebuilt if required. Note that if external packages are
being used you need network connectivity to rebuild from scratch.

# SUBCOMMANDS

Valid **fpm**(1) subcommands are:

  - build Compile the packages into the "build/" directory.

  - new Create a new Fortran package directory with sample files.

  - update Update the project dependencies.

  - run Run the local package binaries. defaults to all binaries for
    that release.

  - test Run the tests.

  - help Alternate to the **--help** switch for displaying help text.

  - list Display brief descriptions of all subcommands.

  - install Install project

Their syntax is

``` 
    build [--profile PROF] [--flag FFLAGS] [--list] [--compiler COMPILER_NAME]
          [--tests]
    new NAME [[--lib|--src] [--app] [--test] [--example]]|
             [--full|--bare][--backfill]
    update [NAME(s)] [--fetch-only] [--clean]
    run [[--target] NAME(s)] [--profile PROF] [--flag FFLAGS] [--list] [--all]
        [--example] [--runner "CMD"] [--compiler COMPILER_NAME] [-- ARGS]
    test [[--target] NAME(s)] [--profile PROF] [--flag FFLAGS] [--list]
         [--runner "CMD"] [--compiler COMPILER_NAME] [-- ARGS]
    help [NAME(s)]
    list [--list]
    install [--profile PROF] [--flag FFLAGS] [--no-rebuild] [--prefix PATH]
    [options]
```

# SUBCOMMAND OPTIONS

**-C**, **--directory** PATH Change working directory to PATH before
running any command

  - ****--profile** PROF**  
    selects the compilation profile for the build. Currently available
    profiles are "release" for high optimization and "debug" for full
    debug options. If **--flag** is not specified the "debug" flags are
    the default.

  - ****--compiler** NAME**  
    Specify a compiler name. The default is "gfortran" unless set by the
    environment variable FPM\_FC.

  - ****--c-compiler** NAME**  
    Specify the C compiler name. Automatically determined by default
    unless set by the environment variable FPM\_CC.

  - ****--archiver** NAME**  
    Specify the archiver name. Automatically determined by default
    unless set by the environment variable FPM\_AR.

  - ****--flag****  
    FFLAGS selects compile arguments for the build, the default value is
    set by the FPM\_FFLAGS environment variable. These are added to the
    profile options if **--profile** is specified, else these these
    options override the defaults. Note object and .mod directory
    locations are always built in.

  - ****--c-flag** CFLAGS**  
    selects compile arguments specific for C source in the build. The
    default value is set by the FPM\_CFLAGS environment variable.

  - ****--link-flag** LDFLAGS**  
    select arguments passed to the linker for the build. The default
    value is set by the FPM\_LDFLAGS environment variable.
    
      - ****--list****  
        List candidates instead of building or running them. On the
        **fpm**(1) command this shows a brief list of subcommands.
    
      - ****--runner** CMD**  
        Provides a command to prefix program execution paths.
    
      - **-- ARGS**  
        Arguments to pass to executables.

# VALID FOR ALL SUBCOMMANDS

  - ****--help****  
    Show help text and exit

  - ****--verbose****  
    Display additional information when available

  - ****--version****  
    Show version information and exit.

@file You may replace the default options for the **fpm**(1) command
from a file if your first options begin with @file. Initial options will
then be read from the "response file" "file.rsp" in the current
directory.

> If "file" does not exist or cannot be read, then an error occurs and
> the program stops. Each line of the file is prefixed with "options"
> and interpreted as a separate argument. The file itself may not
> contain @file arguments. That is, it is not processed recursively.
> 
> For more information on response files see
> 
> ``` 
>       https://urbanjost.github.io/M_CLI2/set_args.3m_cli2.html
> ```
> 
> The basic functionality described here will remain the same, but other
> features described at the above reference may change.
> 
> An example file:
> 
> ``` 
>      # my build options
>      options build
>      options --compiler gfortran
>      options --flag "-pg -static -pthread -Wunreachable-code -Wunused \
>       -Wuninitialized -g -O -fbacktrace -fdump-core -fno-underscoring \
>       -frecord-marker=4 -L/usr/X11R6/lib -L/usr/X11R6/lib64 -lX11"
> ```
> 
> Note **--flag** would have to be on one line as response files do not
> (currently) allow for continued lines or multiple specifications of
> the same option.

# ENVIRONMENT VARIABLES

  - **FPM\_FC**  
    sets the path to the Fortran compiler used for the build, will be
    overwritten by **--compiler** command line option

  - **FPM\_FFLAGS**  
    sets the arguments for the Fortran compiler will be overwritten by
    **--flag** command line option

  - **FPM\_CC**  
    sets the path to the C compiler used for the build, will be
    overwritten by **--c-compiler** command line option

  - **FPM\_CFLAGS**  
    sets the arguments for the C compiler will be overwritten by
    **--c-flag** command line option

  - **FPM\_AR**  
    sets the path to the archiver used for the build, will be
    overwritten by **--archiver** command line option

  - **FPM\_LDFLAGS**  
    sets additional link arguments for creating executables will be
    overwritten by **--link-flag** command line option

# EXAMPLES

sample commands:

``` 
    fpm new mypackage --app --test
    fpm build
    fpm test
    fpm run
    fpm run --example
    fpm new --help
    fpm run myprogram --profile release -- -x 10 -y 20 --title "my title"
    fpm install --prefix ~/.local
```

# SEE ALSO

  - The **fpm**(1) home page is at
    https://github.com/fortran-lang/fpm

  - Registered **fpm**(1) packages are at
    https://fortran-lang.org/packages

  - The **fpm**(1) TOML file format is described at
    https://github.com/fortran-lang/fpm/blob/main/manifest-reference.md
