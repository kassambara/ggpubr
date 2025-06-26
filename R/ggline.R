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
#' @param numeric.x.axis logical. If TRUE, x axis will be treated as numeric. Default is FALSE.
#' @param color line colors.
#' @param linetype line type.
#' @param plot_type plot type. Allowed values are one of "b" for both line and point;
#' "l" for line only; and "p" for point only. Default is "b".
#' @param size line size. Deprecated in ggplot2 v >= 3.4.0, use \code{linewidth} instead.
#' @param linewidth line width. Default is 0.5. Recommended parameter for
#' ggplot2 version >= 3.4.0. If both \code{size} and \code{linewidth} are specified, an error is thrown.
#' @param shape point shapes.
#' @param stroke point stroke. Used only for shapes 21-24 to control the thickness of points border.
#' @param show.line.label logical value. If TRUE, shows line labels.
#' @param point.size point size.
#' @param point.color point color.
#' @param ... other arguments to be passed to geom_dotplot.
#'
#'
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
#' # Use custom color palette
#' ggline(df2, "dose", "len",
#'    linetype = "supp", shape = "supp",
#'    color = "supp", palette = c("#00AFBB", "#E7B800"))
#'
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
#'  add = "mean_se", palette = c("#00AFBB", "#E7B800"))
#'
#' # Add jitter
#' ggline(df3, x = "dose", y = "len", color = "supp",
#'  add = c("mean_se", "jitter"), palette = c("#00AFBB", "#E7B800"))
#'
#' # Add dot plot
#' ggline(df3, x = "dose", y = "len", color = "supp",
#'  add = c("mean_se", "dotplot"), palette = c("#00AFBB", "#E7B800"))
#
#'
#'
#' @export
ggline<- function(data, x, y, group = 1,
                  numeric.x.axis = FALSE,
                  combine = FALSE, merge = FALSE,
                  color = "black", palette = NULL,
                  linetype = "solid",
                  plot_type = c("b", "l", "p"),
                  size = NULL, linewidth = NULL, 
                  shape = 19, stroke = NULL,
                  point.size = linewidth, point.color = color,
                  title = NULL, xlab = NULL, ylab = NULL,
                  facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                  select = NULL, remove = NULL, order = NULL,
                  add = "none",
                  add.params = list(),
                  error.plot = "errorbar",
                  label = NULL, font.label = list(size = 11, color = "black"),
                  label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                  show.line.label = FALSE,
                  position = "identity",
                  ggtheme = theme_pubr(),
                  ...)
{


  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    group = group, numeric.x.axis = numeric.x.axis,
    combine = combine, merge = merge,
    color = color, palette = palette,
    linetype = linetype, plot_type = plot_type,
    size = size,  linewidth = linewidth, shape = shape, stroke = stroke,
    point.size = point.size, point.color = point.color,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    select = select , remove = remove, order = order,
    add = add, add.params = add.params, error.plot = error.plot,
    label = label, font.label = font.label, label.select = label.select,
    repel = repel, label.rectangle = label.rectangle,
    show.line.label = show.line.label, position = position, ggtheme = ggtheme, ...)
  if(!missing(data)) .opts$data <- data
  if(!missing(x)) .opts$x <- x
  if(!missing(y)) .opts$y <- y

  # User options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .user.opts <- as.list(match.call(expand.dots = TRUE))
  .user.opts[[1]] <- NULL # Remove the function name
  # keep only user arguments
  for(opt.name in names(.opts)){
    if(is.null(.user.opts[[opt.name]]))
      .opts[[opt.name]] <- NULL
  }
  .opts$fun <- ggline_core
  .opts$fun_name <- "ggline"
  if(missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- theme_pubr(border = TRUE)
  p <- do.call(.plotter, .opts)

  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)
}


ggline_core <- function(data, x, y, group = 1,
                  numeric.x.axis = FALSE,
                  color = "black", fill = "white", palette = NULL,
                  linetype = "solid",
                  plot_type = c("b", "l", "p"),
                  size = NULL, linewidth = NULL,
                  shape = 19, stroke = NULL,
                  point.size = NULL, point.color = color,
                  title = NULL, xlab = NULL, ylab = NULL,
                  select = NULL, order = NULL,
                  facet.by = NULL,
                  add = "none",
                  add.params = list(),
                  error.plot = "errorbar",
                  show.line.label = FALSE,
                  font.label = list(size = 11, color = "black"),
                  repel = FALSE, label.rectangle = FALSE,
                  position = "identity",
                  ggtheme = theme_pubr(),
                      ...)
{

  # Handle size vs linewidth parameter compatibility
  # size deprecated in ggplot2 v >= 3.4.0
  if (!is.null(size) && !is.null(linewidth)) {
    stop("Please specify either 'size' or 'linewidth', not both. Use 'linewidth' for ggplot2 3.4.0+ compatibility.")
  } else if (!is.null(size)) {
    warning("The 'size' parameter for lines is deprecated in ggplot2 3.4.0+. Please use 'linewidth' instead to avoid this warning in future versions.")
    linewidth <- size  
  } else if (is.null(linewidth)) {
    linewidth <- 0.5  # Default value
  }
  
  if(is.null(point.size)){
    point.size <- if (!is.null(size)) size else if (!is.null(linewidth)) linewidth else 0.5
  }

  xx <- .select_vec(data, x)
  if(inherits(xx, c("character", "numeric")) & !numeric.x.axis)
    data[[x]] <- .select_vec(data, x) %>% as.factor()

  error.plot = error.plot[1]
  plot_type <- match.arg(plot_type)
  if("none" %in% add) add <- "none"
  grouping.vars <- intersect(c(x, color, linetype, group, facet.by), names(data))
  . <- NULL

  # static summaries for computing mean/median and adding errors
  if(is.null(add.params$fill)) add.params$fill <- "white"
  add.params <- .check_add.params(add, add.params, error.plot, data, color, fill = "white", ...)

  if(any(.summary_functions() %in% add)) {
    data_sum <- desc_statby(data, measure.var = y, grps = grouping.vars)
    summary.funcs <- intersect(.summary_functions(), add)
    if(length(summary.funcs) > 1)
      stop("Only one summary function is allowed. ",
           "Choose one of ", .collapse(.summary_functions(), sep = ", "))

    .center <- summary.funcs %>%
      strsplit("_", fixed = TRUE) %>%
      unlist() %>% .[1]

    add <- setdiff(add, .center)
    names(data_sum)[which(names(data_sum) == .center)] <- y
    # data_sum[, x] <- as.factor(data_sum[, x])
    if(inherits(xx, c("character", "numeric")) & !numeric.x.axis)
      data_sum[, x] <- .select_vec(data_sum, x) %>% as.factor()
  }
  else data_sum <- data

  .cols <- unique(c(color, linetype, group))
  if(any(.cols %in% names(data))){
    .in <- which(.cols %in% names(data))
    group <- .cols[.in]
    if(is.null(add.params$group)) add.params$group <- group[1]
  }

  p <- ggplot(data, create_aes(list(x = x, y = y)))

  # Add other geom or summary
  #:::::::::::::::::::::::::::::::::::::::
  add.params <- add.params %>%
    .add_item(error.plot = error.plot,
              position = position, p_geom = "geom_line")

  # First add geom if any
  p <- add.params %>%
    .add_item(p = p, add = setdiff(add, .summary_functions())) %>%
    do.call(ggadd, .)
  # Then add summary statistics
  p <- add.params %>%
    .add_item(p = p, size = linewidth, linewidth = linewidth, add = intersect(add, .summary_functions())) %>%
    do.call(ggadd, .)


  # add.params <- add.params %>%
  #   .add_item(p = p, add = add, error.plot = error.plot,
  #             position = "identity", p_geom = "geom_line")
  # p <- do.call(ggadd, add.params)

  # Main plot
  if(plot_type %in% c("b", "l")){
    line_args <- geom_exec(NULL, data = data_sum,
                            stat = "identity",
                            color = color, linetype = linetype,
                            position = position,
                            linewidth = linewidth)
    mapping <- line_args$mapping
    mapping[["group"]] <- group
    option <- line_args$option
    option[["mapping"]] <- create_aes(mapping)
    p <- p + do.call(geom_line, option)
  }

  if(plot_type %in% c("p", "b")){
    p <- p +
    geom_exec(geom_point, data = data_sum,
               color = point.color, shape = shape,
               size = 1.2+point.size, stroke = stroke,
              position = position)
    # Adjust shape when ngroups > 6, to avoid ggplot warnings
    p <-.scale_point_shape(p, data_sum, shape)
  }

  # Color palette
  user.add.color <- list(...)$user.add.color
  if(is.null(user.add.color)) user.add.color <- ""
  if(.is_color(user.add.color) & !is.numeric(group)){
    ngroup <- nlevels(.select_vec(data_sum, group))
    palette <- rep(user.add.color, ngroup)
  }

  if(show.line.label & !is.numeric(group)){
    xval <- .select_vec(data_sum, x)
    last.xval <- .levels(xval) %>% utils::tail(1)
    groupval <- .select_vec(data_sum, group)
    label.data <- subset(data_sum, xval == last.xval)

    font.label <- .parse_font(font.label)
    p <- font.label %>%
      .add_item(data = label.data, x = x, y = y, label = group,
                repel = repel, label.rectangle = label.rectangle,
                ggtheme = ggtheme, ggp = p) %>%
      do.call(ggtext, .)
  }

   p <- ggpar(p, palette = palette, ggtheme = ggtheme,
              title = title, xlab = xlab, ylab = ylab,...)

  p
}

