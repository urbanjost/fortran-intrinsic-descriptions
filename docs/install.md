# NAME

**install**(1) - install fpm projects

# SYNOPSIS

**fpm** *install* \[-**-profile** *PROF*\] \[-**-flag** *FFLAGS*\]
\[-**-list**\] \[-**-no-rebuild**\] \[-**-prefix** *DIR*\]
\[-**-bindir** *DIR*\] \[-**-libdir** *DIR*\] \[-**-includedir** *DIR*\]
\[-**-verbose**\]

# DESCRIPTION

Subcommand to *install* **fpm** projects. Running *install* will export
the current project to the selected prefix, this will by default
*install* all executables (tests and examples are excluded) which are
part of the projects. Libraries and module files are only installed for
projects requiring the installation of those components in the package
manifest.

# OPTIONS

  - ****--list****  
    list all installable targets for this project, but do not *install*
    any of them

  - ****--profile** *PROF***  
    selects the compilation profile for the build. Currently available
    profiles are "release" for high optimization and "debug" for full
    debug options. If **--flag** is not specified the "debug" flags are
    the default.

  - ****--flag****  
    *FFLAGS* selects compile arguments for the build, the default value
    is set by the FPM\_FFLAGS environment variable. These are added to
    the profile options if **--profile** is specified, else these these
    options override the defaults. Note object and .mod directory
    locations are always built in.

  - ****--c-flag** CFLAGS**  
    selects compile arguments specific for C source in the build. The
    default value is set by the FPM\_CFLAGS environment variable.

  - ****--link-flag** LDFLAGS**  
    select arguments passed to the linker for the build. The default
    value is set by the FPM\_LDFLAGS environment variable.

  - ****--no-rebuild****  
    do not rebuild project before installation

  - ****--prefix** *DIR***  
    path to installation directory (requires write access), the default
    prefix on Unix systems is $HOME/.local and %APPDATA%\\local on
    Windows

  - ****--bindir** *DIR***  
    subdirectory to place executables in (default: bin)

  - ****--libdir** *DIR***  
    subdirectory to place libraries and archives in (default: lib)

  - ****--includedir** *DIR***  
    subdirectory to place headers and module files in (default: include)

  - ****--verbose****  
    print more information

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

1.  Install release version of project:

<!-- end list -->

``` 
    fpm install --profile release
```

2.  Install the project without rebuilding the executables:

<!-- end list -->

``` 
    fpm install --no-rebuild
```

3.  Install executables to a custom prefix into the exe directory:

<!-- end list -->

``` 
    fpm install --prefix $PWD --bindir exe
```
