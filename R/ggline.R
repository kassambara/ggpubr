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
#' @param show.line.label logical value. If TRUE, shows line labels.
#' @param ... other arguments to be passed to geom_dotplot.
#'
#'@section FURTHER ARGUMENTS:
#'Find below further arguments to customize the plots.
#'See also the \link{ggpubr_args} page.
#'
#'@section Plot title and axis labels:
#'\itemize{
#'  \item \strong{title}: main title.
#'  \item \strong{xlab, ylab}: x and y axis labels, respectively.
#'}
#'
#'@section Legend title and position:
#'\itemize{
#'  \item \strong{legend}: character specifying legend position. Allowed values are one of
#'  c("top", "bottom", "left", "right", "none"). Default is "top" side position.
#'  to remove the legend use legend = "none". Legend position can be also
#'  specified using a numeric vector c(x, y). In this case it is
#'  possible to position the legend inside the plotting area. x and y are the
#'  coordinates of the legend box. Their values should be between 0 and 1.
#'  c(0,0) corresponds to the "bottom left" and c(1,1) corresponds to the "top
#'  right" position. For instance use legend = c(0.8, 0.2).
#'  \item \strong{legend.title}: legend title.
#'}
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
                  combine = FALSE, merge = FALSE,
                  color = "black", palette = NULL,
                  linetype = "solid",
                  plot_type = c("b", "l", "p"),
                  size = 0.5, shape = 19,
                  title = NULL, xlab = NULL, ylab = NULL,
                  facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                  select = NULL, remove = NULL, order = NULL,
                  add = "none",
                  add.params = list(),
                  error.plot = "errorbar",
                  label = NULL, font.label = list(size = 11, color = "black"),
                  label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                  show.line.label = FALSE,
                  ggtheme = theme_pubr(),
                  ...)
{


  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    group = group,
    combine = combine, merge = merge,
    color = color, palette = palette,
    linetype = linetype, plot_type = plot_type,
    size = size, shape = shape,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    select = select , remove = remove, order = order,
    add = add, add.params = add.params, error.plot = error.plot,
    label = label, font.label = font.label, label.select = label.select,
    repel = repel, label.rectangle = label.rectangle,
    show.line.label = show.line.label, ggtheme = ggtheme, ...)

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

  #.opts <- match.call(expand.dots = TRUE)
  #.opts <- as.list(.opts)
  #.opts[[1]] <- NULL
  .opts$fun <- ggline_core
  .opts$fun_name <- "ggline"
  if(missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- theme_pubr(border = TRUE)
  p <- do.call(.plotter, .opts)

  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)
}


ggline_core <- function(data, x, y, group = 1,
                  color = "black", fill = "white", palette = NULL,
                  linetype = "solid",
                  plot_type = c("b", "l", "p"),
                  size = 0.6, shape = 19,
                  select = NULL, order = NULL,
                  facet.by = NULL,
                  add = "none",
                  add.params = list(),
                  error.plot = "errorbar",
                  show.line.label = FALSE,
                  font.label = list(size = 11, color = "black"),
                  repel = FALSE, label.rectangle = FALSE,
                  ggtheme = theme_pubr(),
                      ...)
{
  data[, x] <- .select_vec(data, x) %>% as.factor()
  error.plot = error.plot[1]
  plot_type <- match.arg(plot_type)
  if("none" %in% add) add <- "none"
  position = "identity"
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
    data_sum[, x] <- as.factor(data_sum[, x])
  }
  else data_sum <- data

  .cols <- unique(c(color, linetype, group))
  if(any(.cols %in% names(data))){
    .in <- which(.cols %in% names(data))
    group <- .cols[.in]
  }


  p <- ggplot(data, aes_string(x, y))

  # Add other geom or summary
  #:::::::::::::::::::::::::::::::::::::::
  add.params <- add.params %>%
    .add_item(error.plot = error.plot,
              position = "identity", p_geom = "geom_line")

  # First add geom if any
  p <- add.params %>%
    .add_item(p = p, add = setdiff(add, .summary_functions())) %>%
    do.call(ggadd, .)
  # Then add summary statistics
  p <- add.params %>%
    .add_item(p = p, size = size, add = intersect(add, .summary_functions())) %>%
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
                            size = size)
    mapping <- line_args$mapping
    mapping[["group"]] <- group
    option <- line_args$option
    option[["mapping"]] <- do.call(ggplot2::aes_string, mapping)
    p <- p + do.call(geom_line, option)
  }

  if(plot_type %in% c("p", "b")){
    p <- p +
    geom_exec(geom_point, data = data_sum,
               color = add.params$color, shape = shape,
               size = 1.2+size)
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

   p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)

  p
}

