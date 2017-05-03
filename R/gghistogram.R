#' @include utilities.R ggpar.R
NULL
#' Histogram plot
#' @description Create a histogram plot.
#' @inheritParams ggboxplot
#' @param x variable to be drawn.
#' @param y one of "..density.." or "..count..".
#' @param color,fill histogram line color and fill color.
#' @param linetype line type. See \code{\link{show_line_types}}.
#' @param alpha numeric value specifying fill color transparency. Value should
#'   be in [0, 1], where 0 is full transparency and 1 is no transparency.
#' @param bins Number of bins. Defaults to 30.
#' @param add allowed values are one of "mean" or "median" (for adding mean or
#'   median line, respectively).
#' @param add.params parameters (color, size, linetype) for the argument 'add';
#'   e.g.: add.params = list(color = "red").
#' @param rug logical value. If TRUE, add marginal rug.
#' @param add_density logical value. If TRUE, add density curves.
#' @param ... other arguments to be passed to
#'   \code{\link[ggplot2]{geom_histogram}} and \code{\link{ggpar}}.
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right" \item plot orientation : orientation
#'   = c("vertical", "horizontal", "reverse") }
#' @seealso \code{\link{ggpar}}
#'
#' @examples
#' # Create some data format
#' set.seed(1234)
#' wdata = data.frame(
#'    sex = factor(rep(c("F", "M"), each=200)),
#'    weight = c(rnorm(200, 55), rnorm(200, 58)))
#'
#' head(wdata, 4)
#'
#' # Basic density plot
#' # Add mean line and marginal rug
#' gghistogram(wdata, x = "weight", fill = "lightgray",
#'    add = "mean", rug = TRUE)
#'
#' # Change outline colors by groups ("sex")
#' # Use custom color palette
#' gghistogram(wdata, x = "weight",
#'    add = "mean", rug = TRUE,
#'    color = "sex", palette = c("#00AFBB", "#E7B800"))
#'
#' # Change outline and fill colors by groups ("sex")
#' # Use custom color palette
#' gghistogram(wdata, x = "weight",
#'    add = "mean", rug = TRUE,
#'    color = "sex", fill = "sex",
#'    palette = c("#00AFBB", "#E7B800"))
#'
#'
#'
#' # Combine histogram and density plots
#' gghistogram(wdata, x = "weight",
#'    add = "mean", rug = TRUE,
#'    fill = "sex", palette = c("#00AFBB", "#E7B800"),
#'    add_density = TRUE)
#'
#'
#' @export
gghistogram <- function(data, x, y = "..count..",
                      color = "black", fill = NA, palette = NULL,
                      size = NULL, linetype = "solid", alpha = 0.5,
                      bins = NULL,
                      add = c("none", "mean", "median"),
                      add.params = list(linetype = "dashed"),
                      rug = FALSE, add_density = FALSE,
                      ggtheme = theme_classic(),
                      ...)
{

  # Check data
  .dd <- .check_data(data, x, y)
  data <- .dd$data
  x <- .dd$x
  y <- .dd$y
  # Check bins
  if(is.null(bins)){
    bins <- 30
    warning("Using `bins = 30` by default. Pick better value with the argument `bins`.")
  }

  add <- match.arg(add)
  add.params <- .check_add.params(add, add.params, error.plot = "", data, color, fill, ...)
  if(is.null(add.params$size)) add.params$size <- size
  if(is.null(add.params$linetype)) add.params$linetype <- linetype
  # if(add_density) y <- "..density.."

  p <- ggplot(data, aes_string(x, y))

  p <- p +
      .geom_exec(geom_histogram, data = data,
                 color = color, fill = fill, size = size,
                 linetype = linetype, alpha = alpha, bins = bins,
                 position = "identity", ...)

  # Add mean/median
  #++++++++++++++++++++++
  grp <- intersect(unique(c(color, fill, linetype, size, alpha)), colnames(data))[1]
  if(!is.na(grp)) add.params$color <- grp
  if(add %in% c("mean", "median")){
    # NO grouping variable
    if(is.na(grp)) {
      m <- ifelse(add == "mean",
                  mean(data[, x], na.rm = TRUE),
                  stats::median(data[, x], na.rm = TRUE))
      p <- p + .geom_exec(geom_vline, data = data,
                          xintercept = m, color = add.params$color,
                          linetype = add.params$linetype,
                          size = add.params$size)
    }
    # Case of grouping variable
    else {
      grp_name <- grp
      if(!inherits(data[, grp_name], "factor")) data[, grp_name] <- as.factor(data[, grp_name])
      df <- data.frame(grp = data[, grp_name], x = data[, x])
      if (add == "mean") df.m <- stats::aggregate(df[, "x"], by = list(grp = df[, "grp"]), mean, na.rm = TRUE)
      else if (add == "median") df.m <- stats::aggregate(df[, "x"], by = list(grp = df[, "grp"]), stats::median, na.rm = TRUE)
      names(df.m) <- c(grp_name,'x.mean')
      p <- p + .geom_exec(geom_vline, data = df.m,
                          xintercept = "x.mean", color = add.params$color,
                          linetype = add.params$linetype, size = add.params$size)
    }
  }

  # Add marginal rug
  # +++++++++++
  if(rug) {
    .args <- .geom_exec(NULL, data = data,
                              color = add.params$color, sides = "b")
    mapping <- .args$mapping
    mapping[["y"]] <- 0
    option <- .args$option
    option[["mapping"]] <- do.call(ggplot2::aes_string, mapping)
    p <- p + do.call(geom_rug, option)
  }

  # Add density curve
  if(add_density) p <- p + .geom_exec(geom_density, data = data,
                                      color = add.params$color,
                                      linetype = linetype, alpha = alpha,
                                      size = add.params$size)


  p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)
  p
}



