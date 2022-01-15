# NAME

**new**(1) - the **fpm**(1) subcommand to initialize a new project

# SYNOPSIS

**fpm** *new* *NAME* \[\[-**-lib**|**--src**\] \[-**-app**\]
\[-**-test**\] \[-**-example**\]\]|
\[-**-full**|**--bare**\]\[-**-backfill**\] **fpm** *new*
**--help**|**--version**

# DESCRIPTION

"**fpm** *new*" creates and populates a *new* programming project
directory. It

>   - creates a directory with the specified name
> 
>   - runs the command "git init" in that directory
> 
>   - populates the directory with the default project directories
> 
>   - adds sample Fortran source files

The default file structure (that will be automatically scanned) is

``` 
     NAME/
       fpm.toml
       src/
           NAME.f90
       app/
           main.f90
       test/
           check.f90
       example/
           demo.f90
```

Using this file structure is highly encouraged, particularly for small
packages primarily intended to be used as dependencies.

If you find this restrictive and need to customize the package structure
you will find using the **--full** switch creates a heavily annotated
manifest file with references to documentation to aid in constructing
complex package structures.

Remember to update the information in the sample "fpm.toml" file with
your name and e-mail address.

# OPTIONS

  - ***NAME***  
    the name of the project directory to create. The name must be made
    of up to 63 ASCII letters, digits, underscores, or hyphens, and
    start with a letter.

The default is to create the src/, app/, and test/ directories. If any
of the following options are specified then only the selected
subdirectories are generated:

  - ****--lib**,**--src****  
    create directory src/ and a placeholder module named "NAME.f90" for
    use with subcommand "build".

  - ****--app****  
    create directory app/ and a placeholder main program for use with
    subcommand "run".

  - ****--test****  
    create directory test/ and a placeholder program for use with the
    subcommand "test". Note that sans "**--lib**" it really does not
    have anything to test.

  - ****--example****  
    create directory example/ and a placeholder program for use with the
    subcommand "run **--example**". It is only created by default if
    "**--full** is" specified.

So the default is equivalent to

``` 
    fpm NAME --lib --app --test
```

  - ****--backfill****  
    By default the directory must not exist. If this option is present
    the directory may pre-exist and only subdirectories and files that
    do not already exist will be created. For example, if you previously
    entered "**fpm** *new* myname **--lib**" entering "**fpm** *new*
    myname **-full** **--backfill**" will create any missing app/,
    example/, and test/ directories and programs.

  - ****--full****  
    By default a minimal manifest file ("fpm.toml") is created that
    depends on auto-discovery. With this option a much more extensive
    manifest sample is written and the example/ directory is created and
    populated. It is designed to facilitate creating projects that
    depend extensively on non-default build options.

  - ****--bare****  
    A minimal manifest file ("fpm.toml") is created and "README.md" file
    is created but no directories or sample Fortran are generated.

  - ****--help****  
    print this help and exit

  - ****--version****  
    print program version information and exit

# EXAMPLES

Sample use

``` 
   fpm new myproject  # create new project directory and seed it
   cd myproject       # Enter the new directory
   # and run commands such as
   fpm build
   fpm run            # run lone example application program
   fpm test           # run example test program(s)
   fpm run --example  # run lone example program

   fpm new A --full # create example/ and an annotated fpm.toml as well
   fpm new A --bare # create no directories
   create any missing files in current directory
   fpm new --full --backfill
```
