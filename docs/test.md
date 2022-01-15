# NAME

**test**(1) - the **fpm**(1) subcommand to run project tests

# SYNOPSIS

fpm test \[\[-**-target**\] **NAME**(*s*)\] \[-**-profile** PROF\]
\[-**-flag** FFLAGS\] \[-**-compiler** COMPILER\_NAME \] \[-**-runner**
"CMD"\] \[-**-list**\]\[-- ARGS\]

fpm test **--help**|**--version**

# DESCRIPTION

Run applications you have built to test your project.

# OPTIONS

  - ****--target** **NAME**(*s*)**  
    optional list of specific test names to execute. The default is to
    run all the tests in test/ or the tests listed in the "fpm.toml"
    file.
    
    Basic "globbing" is supported where "?" represents any single
    character and "\*" represents any string. Note The glob string
    normally needs quoted to protect the special characters from shell
    expansion.

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

  - ****--runner** CMD**  
    A command to prefix the program execution paths with. see "fpm help
    runner" for further details.

  - ****--list****  
    list candidate basenames instead of running them. Note they

  - ****--list****  
    will still be built if not currently up to date.

  - **-- ARGS**  
    optional arguments to pass to the test **program**(*s*). The same
    arguments are passed to all test names specified.

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

run tests

``` 
 # run default tests in /test or as specified in "fpm.toml"
 fpm test

 # run using compiler command "f90"
 fpm test --compiler f90

 # run a specific test and pass arguments to the command
 fpm test mytest -- -x 10 -y 20 --title "my title line"

 fpm test tst1 tst2 --profile PROF  # run production version of two tests
```
