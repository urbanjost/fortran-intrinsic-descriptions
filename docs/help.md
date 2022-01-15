# NAME

**help**(1) - the **fpm**(1) subcommand to display help

# SYNOPSIS

*fpm* *help* \[fpm\] \[new\] \[build\] \[run\] \[test\] \[help\]
\[version\] \[manual\] \[runner\]

# DESCRIPTION

The "*fpm* *help*" command is an alternative to the --*help* parameter
on the *fpm*(1) command and its subcommands.

# OPTIONS

  - ****NAME**(s)**  
    A list of topic names to display. All the subcommands have their own
    page (*new*, *build*, *run*, *test*, \`\`\`).
    
    The special name "*manual*" displays all the *fpm*(1) built-in
    documentation.
    
    The default is to display *help* for the *fpm*(1) command itself.

# EXAMPLES

Sample usage:

``` 
     fpm help           # general fpm(1) command help
     fpm help version   # show program version
     fpm help new       # display help for "new" subcommand
     fpm help manual    # All fpm(1) built-in documentation

```
