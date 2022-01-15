# NAME

**build**(1) - the **fpm**(1) subcommand to build a project

# SYNOPSIS

**fpm** *build* \[-**-profile** *PROF*\] \[-**-flag** *FFLAGS*\]
\[-**-compiler** *COMPILER\_NAME*\] \[-**-list**\] \[-**-tests**\]

**fpm** *build* **--help**|**--version**

# DESCRIPTION

The "**fpm** *build*" command

>   - Fetches any dependencies
> 
>   - Scans your sources
> 
>   - Builds them in the proper order

The Fortran source files are assumed by default to be in

>   - src/ for modules and procedure source
> 
>   - app/ main **program**(s) for applications
> 
>   - test/ main **program**(s) and support files for project tests
> 
>   - example/ main **program**(s) for example programs Changed or new
>     files found are rebuilt. The results are placed in the *build*/
>     directory.

Non-default pathnames and remote dependencies are used if specified in
the "fpm.toml" file.

# OPTIONS

  - ****--profile** *PROF***  
    selects the compilation profile for the *build*. Currently available
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
    *FFLAGS* selects compile arguments for the *build*, the default
    value is set by the FPM\_FFLAGS environment variable. These are
    added to the profile options if **--profile** is specified, else
    these these options override the defaults. Note object and .mod
    directory locations are always built in.

  - ****--c-flag** CFLAGS**  
    selects compile arguments specific for C source in the *build*. The
    default value is set by the FPM\_CFLAGS environment variable.

  - ****--link-flag** LDFLAGS**  
    select arguments passed to the linker for the *build*. The default
    value is set by the FPM\_LDFLAGS environment variable.

  - ****--list****  
    list candidates instead of building or running them

  - ****--tests****  
    *build* all tests (otherwise only if needed)

  - ****--show-model****  
    show the model and exit (do not *build*)

  - ****--help****  
    print this help and exit

  - ****--version****  
    print program version information and exit

# ENVIRONMENT VARIABLES

  - **FPM\_FC**  
    sets the path to the Fortran compiler used for the *build*, will be
    overwritten by **--compiler** command line option

  - **FPM\_FFLAGS**  
    sets the arguments for the Fortran compiler will be overwritten by
    **--flag** command line option

  - **FPM\_CC**  
    sets the path to the C compiler used for the *build*, will be
    overwritten by **--c-compiler** command line option

  - **FPM\_CFLAGS**  
    sets the arguments for the C compiler will be overwritten by
    **--c-flag** command line option

  - **FPM\_AR**  
    sets the path to the archiver used for the *build*, will be
    overwritten by **--archiver** command line option

  - **FPM\_LDFLAGS**  
    sets additional link arguments for creating executables will be
    overwritten by **--link-flag** command line option

# EXAMPLES

Sample commands:

``` 
  fpm build                   # build with debug options
  fpm build --profile release # build with high optimization
```
