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

askPassword <- function(password = NULL, question = "Please enter password") {
  if(!is.null(password)) {
    if(nchar(password) > 0) {
      return(password)
    }
  }

  if(!interactive()) {
    if(!requireNamespace("getPass", quietly = TRUE)) {
      stop("Package \"getPass\" is needed for asking for passwords when R is not in interactive mode.")
    } else {
      password <- getPass::getPass(question, noblank = T)
      return(password)
    }
  }

  if(is.null(password)) {
    return(openssl::askpass(prompt = question))
  }
  if(nchar(password) == 0) {
    return(openssl::askpass(prompt = question))
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



  tryDecrypt <- function(bytes, password = NULL, first = T) {
    result <- list()

    if(!first) {
      password <- askPassword(password, question = "Something went wrong, please re-enter password")
    } else {
      password <- askPassword(password)
    }

    if(length(password) == 0) {
      stop("No password provided.", call. = F)
    }

    raw_password <- charToRaw(password)

    hashed_password <- openssl::sha256(raw_password)

    iv <- attr(bytes,"iv")

    result <- tryCatch(list(result = 0, data = openssl::aes_cbc_decrypt(bytes, hashed_password, iv = iv)),
                                error = function(e) {
                                  if(e$message == "OpenSSL error in EVP_DecryptFinal_ex: bad decrypt") {
                                    cat("Incorrect password?\n")
                                    return(list(result = 1))
                                  } else if(e$message == "OpenSSL error in win32_load: could not load the shared library") {
                                    #cat("Could not load shared library (openSSL). Wrong password?\n")
                                    cat("Incorrect password?\n")
                                    return(list(result = 2))
                                  } else {
                                    warning(e)
                                    return(list(result = 3))
                                  }
                                }
                                )

    return(result)

  }

  tries <- 3

  while(tries > 0) {
    result <- tryDecrypt(bytes = bytes, password = password, first = (tries == 3))

    if(result$result != 0) {
      tries <- tries - 1
      password <- NULL
    } else {
      break
    }

  }

  if(result$result != 0) {
    stop("Failed to decrypt file")
  }

  decrypted_bytes <- unserialize(result$data)

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

  getPassword <- function(password) {

    password1 <- askPassword(password)
    password2 <- askPassword(password, question = "Please re-enter password:")

    if(password1 != password2) {
      stop("Passwords do not match", call. = F)
    }

    return(password1)
  }

  password <- getPassword(password)

  iv <- openssl::rand_bytes(16)
  encrypted_bytes <- openssl::aes_cbc_encrypt(bytes, openssl::sha256(charToRaw(password)), iv)
  attr(encrypted_bytes, "iv") <- iv

  serialize(encrypted_bytes, conn)

  invisible()
}
