#' @include utilities.R ggpar.R
NULL
#' Bar plot
#' @description Create a bar plot.
#' @inheritParams ggboxplot
#' @inheritParams ggplot2::geom_bar
#' @param x,y x and y variables for drawing.
#' @param color,fill outline and fill colors.
#' @param sort.val a string specifying whether the value should be sorted.
#' Allowed values are "none" (no sorting), "asc" (for ascending) or "desc" (for descending).
#' @param sort.by.groups logical value. If TRUE the data are sorted by groups.
#' Used only when sort.val != "none".
#' @param top a numeric value specifying the number of top elements to be shown.
#' @param label specify whether to add labels on the bar plot. Allowed values
#'   are: \itemize{ \item \strong{logical value}: If TRUE, y values is added as
#'   labels on the bar plot \item \strong{character vector}: Used as text
#'   labels; must be the same length as y. }
#' @param lab.col,lab.size text color and size for labels.
#' @param lab.pos character specifying the position for labels. Allowed values
#'   are "out" (for outside) or "in" (for inside). Ignored when lab.vjust !=
#'   NULL.
#' @param lab.vjust numeric, vertical justification of labels. Provide negative
#'   value (e.g.: -0.4) to put labels outside the bars or positive value to put
#'   labels inside (e.g.: 2).
#' @param lab.hjust numeric, horizontal justification of labels.
#' @param lab.nb.digits integer indicating the number of decimal places (round) to be used.
#' @param ... other arguments to be passed to be passed to ggpar().
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right" \item plot orientation : orientation
#'   = c("vertical", "horizontal", "reverse") }
#' @seealso \code{\link{ggpar}}, \code{\link{ggline}}
#' @examples
#' # Data
#' df <- data.frame(dose=c("D0.5", "D1", "D2"),
#'    len=c(4.2, 10, 29.5))
#' print(df)
#'
#' # Basic plot with label outsite
#' # +++++++++++++++++++++++++++
#' ggbarplot(df, x = "dose", y = "len",
#'   label = TRUE, label.pos = "out")
#'
#' # Change width
#' ggbarplot(df, x = "dose", y = "len", width = 0.5)
#'
#' # Change the plot orientation: horizontal
#' ggbarplot(df, "dose", "len", orientation = "horiz")
#'
#' # Change the default order of items
#' ggbarplot(df, "dose", "len",
#'    order = c("D2", "D1", "D0.5"))
#'
#'
#' # Change colors
#' # +++++++++++++++++++++++++++
#'
#' # Change fill and outline color
#' # add labels inside bars
#' ggbarplot(df, "dose", "len",
#'  fill = "steelblue", color = "steelblue",
#'  label = TRUE, lab.pos = "in", lab.col = "white")
#'
#' # Change colors by groups: dose
#' # Use custom color palette
#'  ggbarplot(df, "dose", "len", color = "dose",
#'    palette = c("#00AFBB", "#E7B800", "#FC4E07"))
#'
#' # Change fill and outline colors by groups
#'  ggbarplot(df, "dose", "len",
#'    fill = "dose", color = "dose",
#'    palette = c("#00AFBB", "#E7B800", "#FC4E07"))
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
#' # Plot "len" by "dose" and change color by a second group: "supp"
#' # Add labels inside bars
#' ggbarplot(df2, "dose", "len",
#'   fill = "supp", color = "supp", palette = "Paired",
#'   label = TRUE, lab.col = "white", lab.pos = "in")
#'
#' # Change position: Interleaved (dodged) bar plot
#' ggbarplot(df2, "dose", "len",
#'   fill = "supp", color = "supp", palette = "Paired",
#'   label = TRUE,
#'   position = position_dodge(0.9))
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
#' ggbarplot(df3, x = "dose", y = "len")
#'
#' # Visualize the mean of each group
#' ggbarplot(df3, x = "dose", y = "len",
#'  add = "mean")
#'
#' # Add error bars: mean_se
#' # (other values include: mean_sd, mean_ci, median_iqr, ....)
#' # Add labels
#' ggbarplot(df3, x = "dose", y = "len",
#'  add = "mean_se", label = TRUE, lab.vjust = -1.6)
#'
#' # Use only "upper_errorbar"
#' ggbarplot(df3, x = "dose", y = "len",
#'  add = "mean_se", error.plot = "upper_errorbar")
#'
#' # Change error.plot to "pointrange"
#' ggbarplot(df3, x = "dose", y = "len",
#'  add = "mean_se", error.plot = "pointrange")
#'
#' # Add jitter points and errors (mean_se)
#' ggbarplot(df3, x = "dose", y = "len",
#'  add = c("mean_se", "jitter"))
#'
#' # Add dot and errors (mean_se)
#' ggbarplot(df3, x = "dose", y = "len",
#'  add = c("mean_se", "dotplot"))
#'
#' # Multiple groups with error bars and jitter point
#' ggbarplot(df3, x = "dose", y = "len", color = "supp",
#'  add = "mean_se", palette = c("#00AFBB", "#E7B800"),
#'  position = position_dodge())
#
#'
#'
#' @export
ggbarplot <- function(data, x, y, combine = FALSE, merge = FALSE,
                      color = "black", fill = "white", palette = NULL,
                      size = NULL, width = NULL,
                      title = NULL, xlab = NULL, ylab = NULL,
                      facet.by = NULL, panel.labs = NULL, short.panel.labs = TRUE,
                      select = NULL, remove = NULL, order = NULL,
                      add = "none", add.params = list(), error.plot = "errorbar",
                      label = FALSE, lab.col = "black", lab.size = 4,
                      lab.pos = c("out", "in"), lab.vjust = NULL, lab.hjust = NULL,
                      lab.nb.digits = NULL,
                      sort.val = c("none", "desc", "asc"), sort.by.groups = TRUE,
                      top = Inf,
                      position = position_stack(),
                      ggtheme = theme_pubr(),
                      ...)
{


  # Default options
  #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  .opts <- list(
    combine = combine, merge = merge,
    color = color, fill = fill, palette = palette,
    size = size, width = width,
    title = title, xlab = xlab, ylab = ylab,
    facet.by = facet.by, panel.labs = panel.labs, short.panel.labs = short.panel.labs,
    select = select , remove = remove, order = order,
    add = add, add.params = add.params, error.plot = error.plot,
    label = label, lab.col = lab.col, lab.size = lab.size,
    lab.pos = lab.pos, lab.vjust = lab.vjust, lab.hjust = lab.hjust,
    lab.nb.digits = lab.nb.digits,
    sort.val = sort.val, sort.by.groups = sort.by.groups, top = top,
    position = position, ggtheme = ggtheme, ...)

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

  if(is.logical(merge)){
    if(merge & missing(position))
      .opts$position <- position_dodge(0.8)
    if(merge & missing(lab.col))
      .opts$lab.col <- ".y."
  }
  else if(is.character(merge)){
    .opts$position <- position_dodge(0.8)
  }

  .opts$fun <- ggbarplot_core
  .opts$fun_name <- "barplot"
  if(missing(ggtheme) & (!is.null(facet.by) | combine))
    .opts$ggtheme <- theme_pubr(border = TRUE)
  p <- do.call(.plotter, .opts)

  if(.is_list(p) & length(p) == 1) p <- p[[1]]
  return(p)

}

ggbarplot_core <- function(data, x, y,
                      color = "black", fill = "white", palette = NULL,
                      size = NULL, width = 0.7,
                      title = NULL, xlab = NULL, ylab = NULL,
                      label = FALSE, lab.col = "black", lab.size = 4,
                      lab.pos = c("out", "in"), lab.vjust = NULL, lab.hjust = NULL,
                      lab.nb.digits = NULL,
                      select = NULL, order = NULL, facet.by = NULL,
                      sort.val = c("none", "desc", "asc"), sort.by.groups = TRUE,
                      merge = FALSE,
                      top = Inf,
                      add = "none",
                      add.params = list(),
                      error.plot = "errorbar",
                      position = position_stack(),
                      ggtheme = theme_pubr(),
                      ...)
{

  sort.val <- match.arg(sort.val)
  if(!is.null(order)) data[, x] <- factor(data[, x], levels = order)
  else {
    xx <- .select_vec(data, x)
    if(inherits(xx, c("character", "numeric")))
      data[, x] <- .select_vec(data, x) %>% as.factor()
  }
  error.plot = error.plot[1]
  lab.pos <- match.arg(lab.pos)
  label <- as.vector(label)
  if("none" %in% add) add <- "none"

  grouping.vars <- intersect(c(x, color, fill, facet.by), names(data))
  . <- NULL

  # static summaries for computing mean/median and adding errors
  if(is.null(add.params$fill)) add.params$fill <- "white"
  if(is.null(add.params$group)){
    if(fill %in% names(data)) add.params$group <- fill
  }
  add.params <- .check_add.params(add, add.params, error.plot, data, color, fill, ...)

  if(any(.summary_functions() %in% add)) {
    data_sum <- desc_statby(data, measure.var = y, grps = grouping.vars)
    summary.funcs <- intersect(.summary_functions(), add)
    if(length(summary.funcs) > 1)
      stop("Only one summary function is allowed. ",
           "Choose one of ", .collapse(.summary_functions(), sep = ", "))
    .center <- .get_errorbar_center_func(summary.funcs)

    add <- setdiff(add, .center)
    names(data_sum)[which(names(data_sum) == .center)] <- y
    if(inherits(xx, c("character", "numeric")))
      data_sum[, x] <- .select_vec(data_sum, x) %>% as.factor()
  }
  else data_sum <- data

  # Sorting
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(top !=Inf & sort.val == "none") sort.val = "desc"
  if(top !=Inf) {
    data_sum <- data_sum[order(-data_sum[, y]), ]
    data_sum <- utils::head(data_sum, n = top)
  }
  grps <- unique(intersect(c(color, fill), names(data)))
  if(length(grps) > 0) grps <- .get_not_numeric_vars(data[, grps, drop = FALSE])
  ngrps <- length(grps)
  if(!sort.by.groups) ngrps <- 0
  # Variables for ordering
  if(ngrps > 0) dd <- data_sum[, c(grps, y)]
  else dd <- data_sum[, y, drop = FALSE]
  if(sort.val == "desc") dd[, y] <- -dd[, y]
  # Sorting
  if(sort.val != "none") {
    if(ngrps == 0) data_sum <- data_sum[order(dd[, y]),]
    else if(ngrps == 1) data_sum <- data_sum[order(dd[, 1], dd[, y]),]
    else if(ngrps == 2) data_sum <- data_sum[order(dd[, 1], dd[, 2], dd[, y]),]
    data_sum[, x] <- factor(data_sum[, x], levels = data_sum[, x])
  }

  # Main plot
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  if(inherits(position, "PositionDodge") & is.null(position$width)) position$width = 0.95
  p <- ggplot(data, aes_string(x, y))
  p <- p +
      geom_exec(geom_bar, data = data_sum,
                stat = "identity",
                color = color, fill = fill,
                position = position,
                size = size, width = width, ...)

  # Add errors
  add.params <- add.params %>% .add_item(p = p, error.plot = error.plot)
  is.stacked.position <- inherits(position, "PositionStack")
  stack.groups <- unique(c(x, facet.by))
  nb.bars.by.xposition <- data_sum %>%
    group_by(!!!syms(stack.groups)) %>%
    dplyr::count() %>%
    dplyr::pull(.data$n) %>%
    max()
  if(is.stacked.position) add.position <- "identity"
  else add.position <- position
  if(is.stacked.position & nb.bars.by.xposition >=2) {
    p <- add.params %>%
      .add_item(add = .remove_errorbar_func(add), position = add.position) %>%
      do.call(ggadd, .)
    if(any(.errorbar_functions() %in% add)){
      p <- p + .geom_stacked_errorbar(
        data_sum, x, y,
        color = add.params$color, fill = add.params$fill,
        group = add.params$group, facet.by = facet.by,
        func = .get_summary_func(add), error.plot = error.plot
      )
    }
  }
  else {
    p <- add.params %>%
      .add_item(add = add, position = add.position) %>%
      do.call(ggadd, .)
  }


   # Add labels
   add.label <- FALSE
   if(is.logical(label)){
     .lab <- y
     add.label <- label
   } else {
     # Add user specified labels as data column
     data_sum$.ulabel. <- label
     .lab <- ".ulabel."
     add.label <- TRUE
   }

   if(add.label) {
     if(is.null(lab.vjust)) lab.vjust <- ifelse(lab.pos == "out", -0.4, 2 )
     if(is.null(lab.hjust)) lab.hjust <- 0.5
     if(!is.null(lab.nb.digits)){
       if(is.numeric(.lab)) .lab <- round(.lab, digits = lab.nb.digits)
       else if(.lab[1] %in% colnames(data_sum))
         data_sum[, .lab] <- dplyr::pull(data_sum, .lab) %>%
           round(digits = lab.nb.digits)
     }

      # pos <- "identity"
      # if color or fill by groups
     .cols <- unique(c(color, fill))
     if(any(.cols %in% names(data))){
       .in <- which(.cols %in% names(data))
       lab.fill <- color.var <- .cols[.in]
       data_sum <- data_sum %>%
         dplyr::arrange(!!!syms(x), desc(!!!syms(color.var)))

       group <- intersect(.cols, names(data))[1]# You should specify group for dodging text

       p <- p + geom_exec(geom_text, data = data_sum, label = .lab,  #fill = lab.fill
                           vjust = lab.vjust, hjust = lab.hjust, size = lab.size, color = lab.col,
                           fontface = "plain", position = position, group = group)
     }
     else{
     p <- p + geom_exec(geom_text, data = data_sum, label = .lab,
                         vjust = lab.vjust,  hjust = lab.hjust, size = lab.size, color = lab.col,
                         fontface = "plain", position = position)
     }
   }
   # To do
   # top10, visualizing error
   p <- ggpar(p, palette = palette, ggtheme = ggtheme,
              title = title, xlab = xlab, ylab = ylab,...)

  p
}

# Stacked error bar ----------------------------
.geom_stacked_errorbar <- function(data_sum, x, y, color = NULL, fill = NULL, facet.by = NULL, group = NULL,
                                  func = "mean_se", error.plot = "errorbar"){
  stack.groups <- unique(c(x, facet.by))
  legend.var <- intersect(unique(c(color, fill, group)), colnames(data_sum))
  error <- .get_errorbar_error_func(func)
  error.value <- data_sum %>% dplyr::pull(!!error)
  desc <- dplyr::desc
  errorbar.position <- data_sum %>%
    group_by(!!!syms(stack.groups)) %>%
    dplyr::arrange(!!sym(x), desc(!!sym(legend.var))) %>%
    dplyr::mutate(
      y = cumsum(!!sym(y)),
      ymin = .data$y - !!sym(error),
      ymax = .data$y + !!sym(error)
      ) %>%
    dplyr::ungroup()
  geom_error <- .get_geom_error_function(error.plot)

  args <- geom_exec(
    data = errorbar.position, color = color,
    group = group,
    x = x, ymin = "ymin", ymax = "ymax"
  )
  mapping <- args$mapping
  option <- args$option
  if(error.plot == "errorbar") option$width <- 0.15
  option[["mapping"]] <- do.call(ggplot2::aes_string, mapping)
  do.call(geom_error, option)
}


.get_geom_error_function <- function(error.plot = "errorbar"){
  error.plot <- error.plot[1]
  geom_func <- ggplot2::geom_errorbar
  if(error.plot %in% c("pointrange", "lower_pointrange", "upper_pointrange"))
    geom_func <- ggplot2::geom_pointrange
  else if(error.plot %in% c("linerange", "lower_linerange", "upper_linerange"))
    geom_func <- ggplot2::geom_linerange
  else if(error.plot %in% c("errorbar", "lower_errorbar", "upper_errorbar"))
    geom_func <- ggplot2::geom_errorbar
  geom_func
}

.is_stacked <- function(p){
  inherits(p$layers[[1]]$position, "PositionStack")
}

# remove "mean_se", "mean_sd", etc
.remove_errorbar_func <- function(add){
  setdiff(add, .errorbar_functions())
}
# return "mean_se"
.get_summary_func <- function(add){
  intersect(.errorbar_functions(), add)
}


# Returns: mean or median
.get_errorbar_center_func <- function(func = "mean_se"){
  . <- NULL
  func %>%
    strsplit("_", fixed = TRUE) %>%
    unlist() %>% .[1]
}

# Returns se, sd, iqr
.get_errorbar_error_func <- function(func = "mean_se"){
  res <- func %>%
    strsplit("_", fixed = TRUE) %>%
    unlist()
  if(length(res) >= 2){
    res <- res[2]
  }
  else res <- NULL
  res
}
