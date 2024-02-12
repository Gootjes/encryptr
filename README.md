# encryptr
R package to encrypt R objects with AES encryption (relies on openSSL).

This package masks the `load` and `save` functions from `base` for convenience.

## Usage

Install the package
```r
remotes::install_github("Gootjes/encryptr")
```

Use `save` to save objects to a file, when no password is specified you will be asked for a password.
```r
library(encryptr)

save(really_sensitive_data, file = "path/to/file", password = "the password")
```

Use `load` to load objects from the file, and store them in the Global environment or in a list.
```r
objs <- list()
load("path/to/file", envir = objs, password = "the password")
```
