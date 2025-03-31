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
  if(!is.null(order)) data[[x]] <- factor(data[[x]], levels = order)
  else {
    xx <- .select_vec(data, x)
    if(inherits(xx, c("character", "numeric")))
      data[[x]] <-  as.factor(data[[x]])
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
    else if(color %in% names(data)) add.params$group <- color
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
  p <- ggplot(data, create_aes(list(x = x, y = y)))
  p <- p +
    geom_exec(geom_bar, data = data_sum,
              stat = "identity",
              color = color, fill = fill,
              position = position,
              size = size, width = width, ...)

  # Add errors
  if(any(.errorbar_functions() %in% add)) {
    # Get the error function (e.g., "se", "sd") from the add parameter
    error_func <- .get_errorbar_error_func(add)

    # Determine grouping variable - use specified group or fall back to x variable
    group_var <- if(!is.null(add.params$group)) add.params$group else x

    # Handle stacked bars differently from dodged bars
    if(inherits(position, "PositionStack")) {
      # For stacked bars, we need manual position calculation

      # Get all possible groups and x levels (including missing combinations)
      all_groups <- levels(factor(data[[add.params$group]]))
      all_x_levels <- levels(factor(data[[x]]))

      # Calculate precise positions for error bars
      data_sum <- data_sum %>%
        mutate(
          # Convert x to numeric position
          x_num = as.numeric(factor(!!sym(x), levels = all_x_levels)),
          # Convert groups to numeric position
          group_num = as.numeric(factor(!!sym(add.params$group), levels = all_groups)),
          # Count total number of groups
          n_groups = length(all_groups),
          # Calculate centered x position:
          # - Starts from x_num (base position)
          # - Adjusts by (group_num - center) * (width/n_groups)
          x_pos = x_num + (group_num - (n_groups + 1)/2) * (width/n_groups),
          # Calculate error bar limits
          ymin = !!sym(y) - !!sym(error_func),
          ymax = !!sym(y) + !!sym(error_func)
        )

      # Add error bars using calculated positions
      p <- p + geom_errorbar(
        data = data_sum,
        aes(x = x_pos, ymin = ymin, ymax = ymax),
        width = 0.1,  # Width of error bar ends
        size = 0.2,    # Line thickness
        color = "black",
        inherit.aes = FALSE  # Don't inherit aesthetics from main plot
      )
    } else {
      # For dodged bars, use ggplot's native position_dodge
      p <- p + geom_errorbar(
        data = data_sum,
        aes(x = !!sym(x),          # Use original x position
            ymin = !!sym(y) - !!sym(error_func),  # Lower error
            ymax = !!sym(y) + !!sym(error_func),  # Upper error
            group = !!sym(group_var)),  # Grouping variable
        position = position_dodge(width = width),  # Match bar width
        width = 0.1,    # Width of error bar ends
        size = 0.2,      # Line thickness
        color = "black"
      )
    }
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
  option[["mapping"]] <- create_aes(mapping)
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
