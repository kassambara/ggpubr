#' @include utilities.R ggpar.R stat_chull.R stat_conf_ellipse.R
NULL
#' Text
#' @description Add text to a plot.
#' @inheritParams ggscatter
#' @param data a data frame
#' @param x,y x and y variables for drawing.
#' @param label the name of the column containing point labels. Can be also a
#'   character vector with length = nrow(data).
#' @param color text font color.
#' @param size text font size.
#' @param face text font style. Allowed values are one of c("plain", "bold",
#'   "italic", "bold.italic").
#' @param family character vector specifying font family.
#' @param label.select can be of two formats: \itemize{
#'   \item a character vector specifying some labels to show.
#'   \item a list containting one or the combination of the following components:
#'     \itemize{
#'        \item \code{top.up} and \code{top.down}: to display the labels  of the top up/down points.
#'        For example, \code{label.select = list(top.up = 10, top.down = 4)}.
#'        \item \code{criteria}: to filter, for example, by x and y variabes values,
#'        use this: \code{label.select = list(criteria = "`y` > 2 & `y` < 5 & `x` \%in\% c('A', 'B')")}.
#'   }
#'   }
#' @param repel a logical value, whether to use ggrepel to avoid overplotting
#'   text labels or not.
#' @param label.rectangle logical value. If TRUE, add rectangle underneath the
#'   text, making it easier to read.
#' @param grouping.vars grouping variables to sort the data by, when the user
#'   wants to display the top n up/down labels.
#' @param ggp a ggplot. If not NULL, points are added to an existing plot.
#' @param ... other arguments to be passed to \code{\link{ggpar}}.
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right"  }
#' @seealso \code{\link{ggpar}}
#' @examples
#' # Load data
#' data("mtcars")
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#' df$name <- rownames(df)
#' head(df[, c("wt", "mpg", "cyl")], 3)
#'
#' # Textual annotation
#' # +++++++++++++++++
#' ggtext(df, x = "wt", y = "mpg",
#'    color = "cyl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'    label = "name", repel = TRUE)
#'
#' # Add rectangle around label
#' ggtext(df, x = "wt", y = "mpg",
#'    color = "cyl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'    label = "name", repel = TRUE,  label.rectangle = TRUE)
#'
#'
#' @export
ggtext <- function(data, x = NULL, y = NULL, label = NULL,
                  color = "black",  palette = NULL,
                  size = 11,  face = "plain", family = "",
                  label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                  grouping.vars = NULL,
                  ggp = NULL, ggtheme = theme_pubr(),
                      ...)
{

  set.seed(123)
  . <- NULL
  data <- as.data.frame(data)
  if(length(label) >1){
    if(length(label) != nrow(data))
      stop("The argument label should be a column name or a vector of length = nrow(data). ",
           "It seems that length(label) != nrow(data)")
    else data$label.xx <- label
    label <- "label.xx"
  }

  if(is.null(ggp)) p <- ggplot(data, aes_string(x, y))
  else p <- ggp

  # Add textual annotation
  # ++++++
  alpha <- 1
  if(!is.null(list(...)$alpha)) alpha <- list(...)$alpha

  if(is.null(label))
    return(p)

  lab_data <- data

  # Special case for density plot and histogram
  # y is calculated as ..count.. or ..density..
  # we should estimate label y from ggplot2 output
  .is_density_plot <- y[1] %in% c("..count..", "..density..")
  if(.is_density_plot){


   # by_panel <- hist.data %>% group_by_(.dots = "PANEL")
    lab_data <- .hist_label_data(p, grouping.vars = list(...)$facet.by)
    y <- "lab.y"

    # hist.data <- ggplot_build(p)$data[[1]][, c("x", "y", "count", "density")]
    # hist.x <- hist.data$x
    # hist.y <- hist.data$y
    # break.x <- c(0, hist.x) %>% unique()
    # label.break <- 1:(length(break.x)-1)
    # lab.y <- .select_vec(data, x) %>%
    #   cut(breaks = break.x, labels = label.break) %>%
    #   hist.y[.]
    # lab_data$lab.y <- lab.y
    # y <- "lab.y"
    # lab_data <- lab_data %>% dplyr::filter(!is.na(lab.y))
  }

  # Select some labels to show
  if(!is.null(label.select)){
    lab_data <- .get_label_data (lab_data, x, y, label = label,
                                label.select = label.select,
                                grouping.vars = grouping.vars)
  }

  if(repel){
    set.seed(123)
    ggfunc <- ggrepel::geom_text_repel
    if(label.rectangle) ggfunc <- ggrepel::geom_label_repel
      p <- p + geom_exec(ggfunc, data = lab_data, x = x, y = y,
                        label = label, fontface = face,
                        family = family,
                        size = size/3, color = color,
                        alpha = alpha,
                        box.padding = unit(0.35, "lines"),
                        point.padding = unit(0.3, "lines"),
                        force = 1, segment.size = 0.2)
  }
  else{
    ggfunc <- geom_text
    vjust  <- -0.7
    if(label.rectangle) {
      ggfunc <- geom_label
      vjust <- -0.4
    }
    p <- p + geom_exec(ggfunc, data = lab_data, x = x, y = y, color = color,
                        label = label, fontface = face, family = family,
                        size = size/3, color = color,
                        vjust = vjust, alpha = alpha)

  }

  p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)
  if(family != "")
    p <- p + theme(text = element_text(family = family))

  p
}



# data: data frame
# x, y: x and y variables
# label: label columns
# label.select: select some labels. Can be a character vector, or a list
#   with the following components (top.up, top.down)
# grouping.vars grouping variables
.get_label_data <- function(data, x, y, label = NULL,
                            label.select = NULL, grouping.vars = NULL)

  {
  if(.is_list(label.select)){
    expected.components = c("top.up", "top.down", "criteria")
    if(!any(expected.components %in% names(label.select)))
      stop("If label.select is a list, it should contain one or the combination ",
           "of the following element: ", .collapse(expected.components, sep = ", "))
  }

  data <- as.data.frame(data)

  if(is.null(label))
    lab_data <- NULL

  else if(is.null(label.select))
    lab_data <- data

  else if(.is_list(label.select)){

    lab_data <- data
    top_up <- top_down <- . <- NULL

    if(!is.null(label.select$top.up))
      top_up <- .top_up(data, x, y, n = label.select$top.up,
                        grouping.vars = grouping.vars)

    if(!is.null(label.select$top.down))
      top_down <- .top_down(data, x, y, n = label.select$top.down,
                            grouping.vars = grouping.vars)

    if(!is.null(top_up) | !is.null(top_down))
      lab_data <- rbind(top_up, top_down)

    if(!is.null(label.select$criteria)){
      criteria <- gsub("`y`", y, label.select$criteria) %>%
        gsub("`x`", x, .)
      lab_data <- dplyr::filter_(lab_data, .dots = criteria)

    }


  }

  else lab_data  <- subset(data, data[, label, drop = TRUE] %in% label.select,
                        drop = FALSE)

  return(lab_data)
}





# Get histogram/density label y coord from ggplot output
# grouping.vars : facet variables
.hist_label_data <- function(p, grouping.vars = NULL){

  .  <- NULL
  x <- .mapping(p) %>%.$x
  hist.data <- ggplot_build(p)$data[[1]]
  data <- p$data

  if(is.null(grouping.vars)){
    return(.hist_label_y(hist.data, data, x ))
  }


  data <- p$data %>%
    dplyr::group_by_(.dots = grouping.vars) %>%
    tidyr::nest()

  hist.data <- hist.data %>%
    dplyr::group_by_(.dots = "PANEL") %>%
    tidyr::nest() %>%
    .$data

  data <- data %>% mutate(hist.data = hist.data)
  lab.data <- purrr::map2(data$hist.data, data$data, .hist_label_y, x)

  data <- data %>% mutate(lab.data  = lab.data ) %>%
    dplyr::select_(.dots = c( "lab.data", grouping.vars)) %>%
    tidyr::unnest()

  data
}

# Get histogram/density label y coord from ggplot output
# hist.data: histogram data. ggplot_build(p)$data[[1]]
# data: data frame
# x: x variable name
.hist_label_y <- function(hist.data, data, x ){
  . <- NULL
  hist.x <- hist.data$x
  hist.y <- hist.data$y
  break.x <- c(0, hist.x) %>% unique()
  label.break <- 1:(length(break.x)-1)
  lab.y <- .select_vec(data, x) %>%
    cut(breaks = break.x, labels = label.break) %>%
    hist.y[.]

  data$lab.y <- lab.y
  data <- data %>% dplyr::filter(!is.na(lab.y))
  data
}




