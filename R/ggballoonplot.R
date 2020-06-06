#' Ballon plot
#'
#' @description Plot a graphical matrix where each cell contains a dot whose
#'   size reflects the relative magnitude of the corresponding component. Useful
#'   to visualize contingency table formed by two categorical variables.
#'
#' @inheritParams ggpar
#' @param data a data frame. Can be: \itemize{ \item \bold{a standard
#'   contingency table} formed by two categorical variables: a data frame with
#'   row names and column names. The categories of the first variable are
#'   columns and the categories of the second variable are rows. \item \bold{a
#'   streched contingency table}: a data frame containing at least three columns
#'   corresponding, respectively, to (1) the categories of the first variable,
#'   (2) the categories of the second varible, (3) the frequency value. In this
#'   case, you should specify the argument x and y in the function
#'   \code{ggballoonplot()}}.
#' @param x,y the column names specifying, respectively, the first and the
#'   second variable forming the contingency table. Required only when the data
#'   is a stretched contingency table.
#' @param color point border line color.
#' @param fill point fill color. Default is "lightgray". Considered only for
#'   points 21 to 25.
#' @param shape points shape. The default value is 21. Alternaive values include
#'   22, 23, 24, 25.
#' @param size point size. By default, the points size reflects the relative
#'   magnitude of the value of the corresponding cell (\code{size = "value"}).
#'   Can be also numeric (\code{size = 4}).
#' @param size.range a numeric vector of length 2 that specifies the minimum and
#'   maximum size of the plotting symbol. Default values are \code{size.range =
#'   c(1, 10)}.
#' @param facet.by character vector, of length 1 or 2, specifying grouping variables for
#'   faceting the plot into multiple panels. Should be in the data.
#' @param show.label logical. If TRUE, show the data cell values as point
#'   labels.
#' @param font.label a vector of length 3 indicating respectively the size
#'   (e.g.: 14), the style (e.g.: "plain", "bold", "italic", "bold.italic") and
#'   the color (e.g.: "red") of point labels. For example font.label = c(14,
#'   "bold", "red"). To specify only the size and the style, use font.label =
#'   c(14, "plain").
#' @param rotate.x.text logica. If TRUE (default), rotate the x axis text.
#' @param ... other arguments passed to the function \code{\link{ggpar}}
#'
#' @examples
#' # Define color palette
#' my_cols <- c("#0D0887FF", "#6A00A8FF", "#B12A90FF",
#' "#E16462FF", "#FCA636FF", "#F0F921FF")
#'
#' # Standard contingency table
#' #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' # Read a contingency table: housetasks
#' # Repartition of 13 housetasks in the couple
#' data <- read.delim(
#'   system.file("demo-data/housetasks.txt", package = "ggpubr"),
#'   row.names = 1
#'   )
#' data
#'
#' # Basic ballon plot
#' ggballoonplot(data)
#'
#' # Change color and fill
#' ggballoonplot(data, color = "#0073C2FF", fill = "#0073C2FF")
#'
#'
#' # Change color according to the value of table cells
#' ggballoonplot(data, fill = "value")+
#'    scale_fill_gradientn(colors = my_cols)
#'
#' # Change the plotting symbol shape
#' ggballoonplot(data, fill = "value",  shape = 23)+
#'   gradient_fill(c("blue", "white", "red"))
#'
#'
#' # Set points size to 8, but change fill color by values
#' # Sow labels
#' ggballoonplot(data, fill = "value", color = "lightgray",
#'               size = 10, show.label = TRUE)+
#'   gradient_fill(c("blue", "white", "red"))
#'
#' # Streched contingency table
#' #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'
#' # Create an Example Data Frame Containing Car x Color data
#' carnames <- c("bmw","renault","mercedes","seat")
#' carcolors <- c("red","white","silver","green")
#' datavals <- round(rnorm(16, mean=100, sd=60),1)
#' car_data <- data.frame(Car = rep(carnames,4),
#'                    Color = rep(carcolors, c(4,4,4,4) ),
#'                    Value=datavals )
#'
#' car_data
#'
#' ggballoonplot(car_data, x = "Car", y = "Color",
#'               size = "Value", fill = "Value") +
#'    scale_fill_gradientn(colors = my_cols) +
#'   guides(size = FALSE)
#'
#'
#' # Grouped frequency table
#' #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'data("Titanic")
#'dframe <- as.data.frame(Titanic)
#'head(dframe)
#'ggballoonplot(
#'  dframe, x = "Class", y = "Sex",
#'  size = "Freq", fill = "Freq",
#'  facet.by = c("Survived", "Age"),
#'  ggtheme = theme_bw()
#')+
#'   scale_fill_gradientn(colors = my_cols)
#'
#'# Hair and Eye Color of Statistics Students
#'data(HairEyeColor)
#'ggballoonplot( as.data.frame(HairEyeColor),
#'               x = "Hair", y = "Eye", size = "Freq",
#'               ggtheme = theme_gray()) %>%
#'  facet("Sex")
#'
#'
#' @export
ggballoonplot <- function(
  data, x = NULL, y = NULL, size = "value",
  facet.by = NULL, size.range = c(1, 10),
  shape = 21, color = "black", fill = "gray",
  show.label = FALSE, font.label = list(size = 12, color = "black"),
  rotate.x.text = TRUE,
  ggtheme = theme_minimal(), ...
  )
  {

  if(inherits(data, "matrix"))
    data <- as.data.frame(data)

  # Check the data
  #::::::::::::::::::::::::::::::::::::::::::::::::::::::


  # case 1: x and y specified. we assume that the data is streched
  # if size not specified, then take the third column as size values
  # detect fill variable if misspecified
  if(!is.null(x) & !is.null(y)){

    if(missing(size)) size <- colnames(data)[3]
    if(fill == "value" & !("value" %in% colnames(data)))
      fill <- colnames(data)[3]
    label <- colnames(data)[3]
  }


  # case 2: x and y are not specified
  # - check if the data is streched:
  #     If yes, then consider the first 3 columns as x, y and size values
  #     If no, then strech the data and continu
  else if(is.null(x) | is.null(y)){

    if(.is_streched(data)){

      .cnames <- colnames(data)
      x <- .cnames[1]
      y <- .cnames[2]
      if(missing(size)) size <- .cnames[3]
      if(fill == "value" & !("value" %in% .cnames)) fill <- .cnames[3]
      label <- .cnames[3]
      # Reverse y levels so that it appears in the right order on the plot
      y.val <- dplyr::pull(data, 2)
      data[, 2] <- y.val %>% factor(levels = rev(.levels(y.val)))
    }

    else {
      data <- .df_strech(data)  # Strech the data into 3 columns: .row|.col|.value
      x <- ".col"
      y <- ".row"
      if(missing(size)) size <- "value"
      label <- "value"
    }
  }

  p <- ggplot(data, create_aes(list(x = x, y = y))) +
    geom_exec(
    geom_point, data = data,
    size = size, fill = fill, shape = shape,
    color = color
  )

  p <- p +
    scale_size(range = size.range)+
    guides( size = guide_legend(reverse=TRUE))+
    ggtheme +
    theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
      )

  if (show.label){
    font.label <- .check_lab_font(font.label)
    text.args <- font.label %>%
      .add_item(
        geomfunc = geom_text, data = data,
        x = x, y = y, label = label
        )
    p <- p + do.call(geom_exec, text.args)
  }

  if(rotate.x.text)
    p <- p + rotate_x_text(45, hjust = 1, vjust = 1)

  if(!is.null(facet.by))
    p <- facet(p, facet.by)

  ggpar(p, ...)
}



# Font label
.check_lab_font <- function(font.label){
  font.label <- .parse_font(font.label)
  font.label$size <- ifelse(is.null(font.label$size), 4, font.label$size/3)
  font.label$color <- ifelse(is.null(font.label$color), "black", font.label$color)
  font.label$face <- ifelse(is.null(font.label$face), "plain", font.label$face)
  font.label
}


# strech a data frame with row names
# returns a data frame with 3 columns .row, .col, value
.df_strech <- function(data){
  .col.names <- colnames(data)
  .row.names <- rownames(data)
  data <- data %>%
    dplyr::mutate(.row = .row.names) %>%
    dplyr::select(.row, dplyr::everything())

  # Sretch the data into three columns
  .col <- .row <- NULL
  data <- data %>%
    tidyr::gather(
      key= ".col", value = "value", -.row
    ) %>%
    dplyr::mutate(
      .col = factor(.col, levels = .col.names),
      .row = factor(.row, levels = rev(.row.names))
    )
  data
}



# Check if the contingency table is in the streched format
.is_streched <- function(data, x = NULL, y = NULL){

  streched <- TRUE
  if(is.null(x) | is.null(y)){

    if(.is_numeric_data(data))
      streched <- FALSE
    else{
      x <- dplyr::pull(data, 1)
      y <- dplyr::pull(data, 2)
      z <- dplyr::pull(data, 3)
      streched <- (
        (is.character(x) | is.factor(x)) &
          (is.character(y) | is.factor(y)) &
          is.numeric(z)
        )
    }
  }

  streched
}

# Check if a data matrix is numeric
.is_numeric_data <- function(x){
  all(apply(x, 2, is.numeric))
}

# For tible data. The first column should be row names
.is_correct_tbl <- function(x){
  ok <- FALSE
  if(inherits(x,"tbl_df" )) ok <- TRUE

  if(ok){
    .rows <- dplyr::pull(x, 1)
    ok <- is.character(.rows) & .is_numeric_data(x[,-1])
  }

  ok
}

