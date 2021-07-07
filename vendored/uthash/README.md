
# moonlark

Commit: bf15263081be6229be31addd48566df93921cb46

Moonlark uses
[makeheaders](https://fossil-scm.org/home/doc/trunk/src/makeheaders.html).
To make the UTHash headers work better with makeheaders, the "guard" macros have been removed, e.g.

```
/* moonlark - prep for makeheaders */
/* #ifndef UTARRAY_H */
/* #define UTARRAY_H */
...
/* moonlark - prep for makeheaders */
/* #endif /\* UTARRAY_H *\/ */
```

# original README:

[![Build status](https://api.travis-ci.org/troydhanson/uthash.svg?branch=master)](https://travis-ci.org/troydhanson/uthash)

Documentation for uthash is available at:

https://troydhanson.github.io/uthash/


