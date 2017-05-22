#::::::::::::::::::::::::::::::::::::::::::::::::::::
# General helper functions
#::::::::::::::::::::::::::::::::::::::::::::::::::::

# Get random string
.random_string <-function(.length = 7){
  index <- sample(1:26, .length)
  paste(letters[index], collapse = "")
}

# get file extension
.file_ext <- function(x){
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}


# extract device name from file name
.device <- function (filename) {

  device <- .file_ext(filename)
  devices <- list(eps = grDevices::postscript, ps = grDevices::postscript,
                  pdf = grDevices::pdf,
                  # svg = svglite::svglite,
                  png = grDevices::png,
                  jpg = grDevices::jpeg,
                  jpeg = grDevices::jpeg,
                  bmp = grDevices::bmp,
                  tiff = grDevices::tiff)

  dev <- devices[[device]]
  if (is.null(dev)) {
    stop("Unknown graphics device '", device, "'", call. = FALSE)
  }

  dev
}

