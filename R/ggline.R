#' @include utilities.R ggpar.R
NULL
#' Line plot
#' @description Create a line plot.
#' @inheritParams ggboxplot
#' @inheritParams ggplot2::geom_line
#' @param x,y x and y variables for drawing.
#' @param group grouping variable to connect points by line.
#' Allowed values are 1 (for one line, one group) or a character vector specifying
#' the name of the grouping variable (case of multiple lines).
#' @param color line colors.
#' @param linetype line type.
#' @param plot_type plot type. Allowed values are one of "b" for both line and point;
#' "l" for line only; and "p" for point only. Default is "b".
#' @param shape point shapes.
#' @param ... other arguments to be passed to geom_dotplot.
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right" \item plot orientation : orientation
#'   = c("vertical", "horizontal", "reverse") }
#' @seealso \code{\link{ggpar}}, \code{\link{ggbarplot}}
#' @examples
#' # Data
#' df <- data.frame(dose=c("D0.5", "D1", "D2"),
#'    len=c(4.2, 10, 29.5))
#' print(df)
#'
#' # Basic plot
#' # +++++++++++++++++++++++++++
#' ggline(df, x = "dose", y = "len")
#'
#' # Change line type, point shape and color
#' ggline(df, "dose", "len", linetype = "dashed",
#'    shape = 18, color = "steelblue")
#'
#'
#' # Select and order items
#' # ++++++++++++++++++++++++++++++
#' # Select which items to display: "0.5" and "2"
#' ggline(df, "dose", "len",
#'    select = c("D0.5", "D2"))
#' # Change the default order of items
#' ggline(df, "dose", "len",
#'    order = c("D2", "D1", "D0.5"))
#'
#'
#'
#' # Plot with multiple groups
#' # +++++++++++++++++++++
#'
#' # Create some data
#' df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
#'    dose=rep(c("D0.5", "D1", "D2"),2),
#'    len=c(6.8, 15, 33, 4.2, 10, 29.5))
#' print(df2)
#'
#' # Plot "len" by "dose" and
#' # Change line types and point shapes by a second groups: "supp"
#' ggline(df2, "dose", "len",
#'   linetype = "supp", shape = "supp")
#'
#'
#' # Change colors
#' # +++++++++++++++++++++
#'
#' # Change color by group: "supp"
#' ggline(df2, "dose", "len",
#'    linetype = "supp", shape = "supp",
#'    color = "supp")
#'
#' # Use custom colors
#' ggline(df2, "dose", "len",
#'    linetype = "supp", shape = "supp",
#'    color = "supp", palette = c("#999999", "#E69F00"))
#'
#' # Use brewer palette
#' ggline(df2, "dose", "len",
#'    linetype = "supp", shape = "supp",
#'    color = "supp", palette = "Paired")
#'
#' # Use grey palette
#' ggline(df2, "dose", "len",
#'    linetype = "supp", shape = "supp",
#'    color = "supp", palette = "grey")
#'
#' # Add points and errors
#' # ++++++++++++++++++++++++++
#'
#' # Data: ToothGrowth data set we'll be used.
#' df3 <- ToothGrowth
#' head(df3, 10)
#'
#' # It can be seen that for each group we have
#' # different values
#' ggline(df3, x = "dose", y = "len")
#'
#' # Visualize the mean of each group
#' ggline(df3, x = "dose", y = "len",
#'  add = "mean")
#'
#' # Add error bars: mean_se
#' # (other values include: mean_sd, mean_ci, median_iqr, ....)
#' # Add labels
#' ggline(df3, x = "dose", y = "len", add = "mean_se")
#'
#' # Change error.plot to "pointrange"
#' ggline(df3, x = "dose", y = "len",
#'  add = "mean_se", error.plot = "pointrange")
#'
#' # Add jitter points and errors (mean_se)
#' ggline(df3, x = "dose", y = "len",
#'  add = c("mean_se", "jitter"))
#'
#' # Add dot and errors (mean_se)
#' ggline(df3, x = "dose", y = "len",
#'  add = c("mean_se", "dotplot"), color = "steelblue")
#'
#' # Add violin and errors (mean_se)
#' ggline(df3, x = "dose", y = "len",
#'  add = c("mean_se", "violin"), color = "steelblue")
#'
#' # Multiple groups with error bars
#' # ++++++++++++++++++++++
#'
#' ggline(df3, x = "dose", y = "len", color = "supp",
#'  add = "mean_se", palette = "Dark2")
#'
#' # Add jitter
#' ggline(df3, x = "dose", y = "len", color = "supp",
#'  add = c("mean_se", "jitter"), palette = "Dark2")
#'
#' # Add dot plot
#' ggline(df3, x = "dose", y = "len", color = "supp",
#'  add = c("mean_se", "dotplot"), palette = "Dark2")
#
#'
#'
#' @export
ggline <- function(data, x, y, group = 1,
                      color = "black", palette = NULL,
                      linetype = "solid",
                      plot_type = c("b", "l", "p"),
                      size = 1, shape = 19,
                      select = NULL, order = NULL,
                      add = "none",
                      add.params = list(),
                      error.plot = "errorbar",
                      ggtheme = theme_pubr(),
                      ...)
{

  data[, x] <- factor(data[, x])
  error.plot = error.plot[1]
  plot_type <- match.arg(plot_type)
  if("none" %in% add) add <- "none"
  position = "identity"

  # static summaries for computing mean/median and adding errors
  if(is.null(add.params$fill)) add.params$fill <- "white"
  add.params <- .check_add.params(add, add.params, error.plot, data, color, fill = "white", ...)

  errors <- c("mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range")
  if(any(errors %in% add)) {
    data_sum <- desc_statby(data, measure.var = y,
                        grps = intersect(c(x, color, linetype, group), names(data)))
    .center <- intersect(c("mean", "median"), add)
    errors <- c("mean_se", "mean_sd", "mean_ci", "mean_range", "median_iqr", "median_mad", "median_range")
    if(length(.center) == 2) stop("Use mean or mdedian, but not both at the same time.")
    else if(length(.center) == 0) .center <- unlist(strsplit(errors, "_", fixed = TRUE))[1]
    add <- setdiff(add, .center)
    names(data_sum)[which(names(data_sum) == .center)] <- y
    data_sum[, x] <- as.factor(data_sum[, x])
  }
  else data_sum <- data

  group = group
  .cols <- unique(c(color, linetype))
  if(any(.cols %in% names(data))){
    .in <- which(.cols %in% names(data))
    group <- .cols[.in]
  }


  p <- ggplot(data, aes_string(x, y))
  # Add errors
  p <- .add(p, add = add,
            add.params = add.params, error.plot = error.plot,
            position = "identity", p_geom = "geom_line")

  # Main plot
  if(plot_type %in% c("b", "l")){
    line_args <- .geom_exec(NULL, data = data_sum,
                            stat = "identity",
                            color = color, linetype = linetype,
                            position = position,
                            size = size)
    mapping <- line_args$mapping
    mapping[["group"]] <- group
    option <- line_args$option
    option[["mapping"]] <- do.call(ggplot2::aes_string, mapping)
    p <- p + do.call(geom_line, option)
  }

  if(plot_type %in% c("p", "b")){
    p <- p +
    .geom_exec(geom_point, data = data_sum,
               color = color, shape = shape,
               size = 2+size)
  }

  # Select and order
  if(is.null(select)) select <- order
  if (!is.null(select) | !is.null(order))
    p <- p + scale_x_discrete(limits = as.character(select))
   p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)

  p
}

