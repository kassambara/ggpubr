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
ggbarplot <- function(data, x, y,
                      color = "black", fill = "white", palette = NULL,
                      size = NULL, width = NULL,
                      label = FALSE, lab.col = "black", lab.size = 4,
                      lab.pos = c("out", "in"), lab.vjust = NULL, lab.hjust = NULL,
                      select = NULL, order = NULL,
                      sort.val = c("none", "desc", "asc"), sort.by.groups = TRUE,
                      top = Inf,
                      add = "none",
                      add.params = list(),
                      error.plot = "errorbar",
                      position = position_stack(),
                      ggtheme = theme_classic2(),
                      ...)
{

  sort.val <- match.arg(sort.val)
  data[, x] <- factor(data[, x])
  error.plot = error.plot[1]
  lab.pos <- match.arg(lab.pos)
  label <- as.vector(label)
  if("none" %in% add) add <- "none"

  # static summaries for computing mean/median and adding errors
  if(is.null(add.params$fill)) add.params$fill <- "white"
  add.params <- .check_add.params(add, add.params, error.plot, data, color, fill, ...)

  errors <- c("mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range")
  if(any(errors %in% add)) {
    data_sum <- desc_statby(data, measure.var = y,
                        grps = intersect(c(x, color, fill), names(data)))
    .center <- intersect(c("mean", "median"), add)
    errors <- c("mean_se", "mean_sd", "mean_ci", "mean_range", "median_iqr", "median_mad", "median_range")
    if(length(.center) == 2) stop("Use mean or mdedian, but not both at the same time.")
    else if(length(.center) == 0) .center <- unlist(strsplit(errors, "_", fixed = TRUE))[1]
    add <- setdiff(add, .center)
    names(data_sum)[which(names(data_sum) == .center)] <- y
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
      .geom_exec(geom_bar, data = data_sum,
                stat = "identity",
                color = color, fill = fill,
                position = position,
                size = size, width = width, ...)

  # Add errors
   p <- .add(p, add = add,
            add.params = add.params, error.plot = error.plot)

   # Add labels
   add.label <- FALSE
   if(is.logical(label)){
     .lab <- y
     add.label <- label
   } else {
     .lab <- label
     add.label <- TRUE
   }

   if(add.label) {
     if(is.null(lab.vjust)) lab.vjust <- ifelse(lab.pos == "out", -0.4, 2 )
     if(is.null(lab.hjust)) lab.hjust <- 0.5
      # pos <- "identity"
      # if color or fill by groups
     .cols <- unique(c(color, fill))
     if(any(.cols %in% names(data))){
       .in <- which(.cols %in% names(data))
       lab.fill <- .cols[.in]
       p <- p + .geom_exec(geom_text, data = data_sum, label = .lab,  fill = lab.fill,
                           vjust = lab.vjust, hjust = lab.hjust, size = lab.size, color = lab.col,
                           fontface = "plain", position = position)
     }
     else{
     p <- p + .geom_exec(geom_text, data = data_sum, label = .lab,
                         vjust = lab.vjust,  hjust = lab.hjust, size = lab.size, color = lab.col,
                         fontface = "plain", position = position)
     }
   }

   # To do
   # top10, visualizing error

  # Select and order
  if(is.null(select)) select <- order
  if (!is.null(select) | !is.null(order))
    p <- p + scale_x_discrete(limits = as.character(select))
   p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)

  p
}

