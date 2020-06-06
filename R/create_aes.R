#' @include utilities.R
NULL
#' Create Aes Mapping from a List
#'
#' @description Create aes mapping to make programming easy with ggplot2.
#' @param .list a list of aesthetic arguments; for example .list = list(x = "dose", y = "len", color = "dose").
#' @param parse logical. If TRUE, parse the input as an expression.
#'
#' @examples
#' # Simple aes creation
#' create_aes(list(x = "Sepal.Length", y = "Petal.Length" ))
#'
#' # Parse an expression
#' x <- "log2(Sepal.Length)"
#' y <- "log2(Petal.Length)"
#' create_aes(list(x = x, y = y ), parse = TRUE)
#'
#' # Create a ggplot
#' mapping <- create_aes(list(x = x, y = y ), parse = TRUE)
#' ggplot(iris, mapping) +
#'  geom_point()
#'
#' @export
create_aes <- function(.list, parse = FALSE){
  if(parse){
    return(create_aes.parse(.list))
  } else{
    return(create_aes.name(.list))
  }
}


# Ref:  https://rpubs.com/hadley/97970
# Parse as name. If x_var is "a + b", as.name() will turn it into a variable called `a + b`.
create_aes.name <- function(.list){
  .list <- .list %>% purrr::map(function(x) to_name(x))
  do.call(ggplot2::aes, .list)
}

# Parse an expression. If x_var is "a + b", parse() will turn it into the function call a + b
create_aes.parse <- function(.list){
  .list <- .list %>% purrr:: map(function(x) parse_expression(x))
  do.call(ggplot2::aes, .list)
}

parse_expression <- function(x){
  if(is.character(x) & !is_numeric_char(x)){
    x <- parse(text = x)[[1]]
  }
  x
}

to_name <- function(x){
  if(is.character(x) & !is_numeric_char(x)){
    x <- as.name(x)
  }
  x
}

# return TRUE for 1, 2, "1", "2", etc
is_numeric_char <- function(x){
  grepl("^[[:digit:]]+$", x)
}
