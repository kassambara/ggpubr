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
                  svg = grDevices::svg,
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


# Grouping data by variables
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.group_by <- function(data, grouping.vars){

  . <- NULL # used in pipes

  # Grouping the data ==> list of data sets
  grouped.d <- data %>%
    df_nest_by(vars = grouping.vars)

  # Defining names for the list of data sets.
  # names = combination of the levels of the grouping variables
  .names.df <- grouped.d[, grouping.vars, drop = FALSE]
  .names <- .paste_colnames(.names.df, sep = ":") %>%
    apply(1, paste, collapse = ", ")
  names(grouped.d$data) <- .names
  return(grouped.d)
}


# Pasting the column name to each value of a dataframe
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
.paste_colnames <- function(data, sep = "."){

  data <- as.data.frame(data)

  if(ncol(data) == 1){

    res <- paste0(colnames(data), ".", data[[1]])
    res <- data.frame(x = res, stringsAsFactors = FALSE)
    colnames(res) <- colnames(data)
    return(res)
  }

  res <- apply(data, 1,
               function(row, cname){paste(cname, row, sep = sep)},
               colnames(data)
  ) %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)
  colnames(res) <- colnames(data)
  res
}


# Setting seed with possibility to restore initial random state
#:::::::::::::::::::::::::::::::::::::::::::::::
# Ref: https://github.com/florianhartig/DHARMa/blob/master/DHARMa/R/random.R
# seed numeric value to be used for setting seed
# return a function named restore_random_state()
# for restoring intitial random state
#
# Examples:
# seed <- set_seed(123)
# Restore back initial state
# seed$restore_random_state()
 set_seed  <- function(seed){
    # Record current random state
    current  <- mget(".Random.seed", envir = .GlobalEnv, ifnotfound = list(NULL))[[1]]
    restore_random_state <- function(){
      if(is.null(current)) rm(".Random.seed", envir = .GlobalEnv)
      else assign(".Random.seed", current , envir = .GlobalEnv)
    }

    # Setting seed
    if(is.numeric(seed)) set.seed(seed)
    # ensuring that RNG has been initialized
    if (is.null(current)) stats::runif(1)

    # Returning function for restoring state
    invisible(list(restore_random_state = restore_random_state))
  }
