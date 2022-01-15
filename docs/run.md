# NAME

**run**(1) - the **fpm**(1) subcommand to run project applications

# SYNOPSIS

fpm run \[\[-**-target**\] **NAME**(*s*) \[-**-profile** PROF\]
\[-**-flag** FFLAGS\] \[-**-compiler** COMPILER\_NAME\] \[-**-runner**
"CMD"\] \[-**-example**\] \[-**-list**\] \[-**-all**\] \[-- ARGS\]

fpm run **--help**|**--version**

# DESCRIPTION

Run the applications in your **fpm**(1) package. By default applications
in /app or specified as "executable" in your "fpm.toml" manifest are
used. Alternatively demonstration programs in example/ or specified in
the "example" section in "fpm.toml" can be executed. The applications
are automatically rebuilt before being run if they are out of date.

# OPTIONS

  - ****--target** **NAME**(*s*)**  
    list of application names to execute. No name is required if only
    one target exists. If no name is supplied and more than one
    candidate exists or a name has no match a list is produced and
    **fpm**(1) exits.
    
    Basic "globbing" is supported where "?" represents any single
    character and "\*" represents any string. Note The glob string
    normally needs quoted to the special characters from shell
    expansion.

  - ****--all****  
    Run all examples or applications. An alias for **--target** '\*'.

  - ****--example****  
    Run example programs instead of applications.

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
    list basenames of candidates instead of running them. Note
    out-of-date candidates will still be rebuilt before being listed.

  - **-- ARGS**  
    optional arguments to pass to the **program**(*s*). The same
    arguments are passed to all program names specified.

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

**fpm**(1) - run or display project applications:

``` 
  fpm run        # run a target when only one exists or list targets
  fpm run --list # list basename of all targets, running nothing.
  fpm run "demo*" --list # list target basenames starting with "demo*".
  fpm run "psi*" --runner # list target pathnames starting with "psi*".
  fpm run --all  # run all targets, no matter how many there are.

  # run default program built or to be built with the compiler command
  # "f90". If more than one app exists a list displays and target names
  # are required.
  fpm run --compiler f90

  # run example programs instead of the application programs.
  fpm run --example "*"

  # run a specific program and pass arguments to the command
  fpm run myprog -- -x 10 -y 20 --title "my title line"

  # run production version of two applications
  fpm run --target prg1,prg2 --profile release

  # install executables in directory (assuming install(1) exists)
  fpm run --runner 'install -b -m 0711 -p -t /usr/local/bin'
```
