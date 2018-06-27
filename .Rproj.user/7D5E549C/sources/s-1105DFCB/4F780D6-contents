# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

askPassword <- function(password = NULL) {
  if(!is.null(password)) {
    if(nchar(password) > 0) {
      return(password)
    }
  }

  if(!interactive()) {
    if(!requireNamespace("getPass", quietly = TRUE)) {
      stop("Package \"getPass\" is needed for asking for passwords when R is not in interactive mode.")
    } else {
      password <- getPass::getPass("Please enter your password", noblank = T)
      return(password)
    }
  }

  if(is.null(password)) {
    return(openssl::askpass())
  }
  if(nchar(password) == 0) {
    return(openssl::askpass())
  }

  stop("Cannot set password")
}

#' Load encrypted R objects
#'
#' This function allows you to load encrypted R objects.
#' Loaded R objects are attached to the environment.
#' @param file the path to the file.
#' @param envir the environment to attach to.
#' @param password the password to use, if NULL, the function asks for a password.
#' @note Asking for a password relies on being in interactive() mode (RStudio, RGui).
#' Outside interactive() mode (rmarkdown, knitr), the package "getPass" is required.
#' @export
#' @details Uses AES 256-bit encryption (in CBC mode) with a SHA-256 hashed password.
#' @examples
#' load("mydata.Rdata")
#'
load <- function(file, envir = parent.frame(), password = NULL) {

  conn <- base::file(description = file, open = "rb")
  bytes <- unserialize(conn)
  on.exit({close(conn)})

  password <- askPassword(password)

  decrypted_bytes <- tryCatch(unserialize(openssl::aes_cbc_decrypt(bytes, openssl::sha256(charToRaw(password)), iv = attr(bytes,"iv"))), error = function(e){
    if(e$message == "OpenSSL error in EVP_DecryptFinal_ex: bad decrypt") {
      stop("Incorrect password", call.=F)
    } else if(e$message == "OpenSSL error in win32_load: could not load the shared library") {
      stop("Could not load shared library (openSSL). Wrong password?", call.=F)
    } else {
      stop(e)
    }
  })

  sapply(names(decrypted_bytes), function(name){
    envir[[name]] = decrypted_bytes[[name]]
  })

  invisible()
}


#' Save R objects as an encrypted file.
#'
#' This function allows you to save encrypted R objects.
#' R objects are taken from the environment.
#' @param ... the variables to be saved.
#' @param file the path to the file.
#' @param envir the environment the variables are token from.
#' @param password the password to use, if NULL, the package tries to ask for a password.
#' @param list an optional list of names that identity the R objects to store.
#' @note Asking for a password relies on being in interactive() mode (RStudio, RGui).
#' Outside interactive() mode (rmarkdown, knitr), the package "getPass" is required.
#' @export
#' @details Uses AES 256-bit encryption (in CBC mode) with a SHA-256 hashed password.
#' @examples
#' df <- data.frame(rnorm(100))
#' save(df, file="mydata.Rdata")
#'
save <- function(..., list = character(),
                 file = stop("'file' must be specified"), envir = parent.frame(), password = NULL) {

  conn <- base::file(description = file, open = "wb")
  on.exit({close(conn)})

  names <- as.character(substitute(base::list(...)))[-1L]
  names <- c(list, names)
  names(names) <- names



  ok <- vapply(names, exists, NA, envir = envir)
  if (!all(ok)) {
    n <- sum(!ok)
    stop(sprintf(ngettext(n, "object %s not found", "objects %s not found"), paste(sQuote(names[!ok]), collapse = ", ")), domain = NA, call. = F)
  }

  data <- lapply(names, function(name){
    if(is.null(envir[[name]])) {
      stop(paste("Cannot find variable:",name), call. = F)
    }
    envir[[name]]
  })

  bytes <- serialize(data, NULL)

  password <- askPassword(password)

  iv <- openssl::rand_bytes(16)
  encrypted_bytes <- openssl::aes_cbc_encrypt(bytes, openssl::sha256(charToRaw(password)), iv)
  attr(encrypted_bytes, "iv") <- iv

  serialize(encrypted_bytes, conn)

  invisible()
}
