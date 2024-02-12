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

### Note
Note that `load()` and `save()` mask the functions from the `base` package.
If you want to save your data to a file without a password, make sure you call the right function.
Or use lower-level functions such as `saveRDS()` and `readRDS()` from the `base` package.

```r
library(encryptr)

objs <- list()
# calls encryptr::load, alternative is to not library() the encryptr package at all and do encryptr::load() instead of load() to avoid the masking
load("path/to/file", envir = objs, password = "the password")
base::save(objs, file = "path/to/file")
```
