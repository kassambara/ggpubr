#' @include desc_statby.R
NULL
#' @import ggplot2

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Execute a geom_* function from ggplot2
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# geomfunc : gem_*() functions
# data data for mapping
# ... argument accepeted by the function
# return a plot if geomfunc!=Null or a list(option, mapping) if  geomfunc = NULL
.geom_exec <- function (geomfunc = NULL, data = NULL,
                        position = NULL, ...) {
  params <- list(...)

  mapping <-
    list() # option to pass to mapping aes() or aes_string()
  option <- list() # option to the geom_*()

  allowed_options <- c(
    # general
    "color", "colour", "linetype", "fill", "size", "shape", "width",
    "alpha", "na.rm", "lwd", "pch", "cex", "position", "stat", "geom",
    "show.legend", "inherit.aes", "fun.args", "fontface",
    # boxplot
    "outlier.colour", "outlier.shape", "outlier.size",
    "outlier.stroke", "notch", "notchwidth", "varwidth",
    # dot plot
    "binwidth", "binaxis", "method", "binpositions",
    "stackdir", "stackratio", "dotsize",
    # Violin
    "trim", "draw_quantiles", "scale",
    # error
    "ymin", "ymax", "xmin", "xmax",
    # text
    "label", "hjust", "vjust", "fontface",
    # smooth
    "se", "level", "fullrange"

  )

  columns <- colnames(data)
  for (key in names(params)) {
    value <- params[[key]]
    if (is.null(value)) {

    }
    else if (value[1] %in% columns) {
      mapping[[key]] <- value

    }
  else if (key %in% allowed_options) {
      option[[key]] <- value
    }
    # else warnings("Don't know '", key, "'")
  }
  if (!is.null(position))
  option[["position"]] <- position
  option[["data"]] <- data
  if(is.null(geomfunc)){
    return(list(option = option, mapping = mapping))
  }
  else{
  option[["mapping"]] <- do.call(ggplot2::aes_string, mapping)
  return(do.call(geomfunc, option))
  }
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Official argument from ggplot2
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# bar plot arguments
.barplot_params <- function(...){
  x <- list(...)
  res <- list()
  res$width  <- x$width
  res$binwidth  <- x$binwidth
  res$na.rm  <- ifelse(!is.null(x$na.rm), x$na.rm, FALSE)
  res$show.legend <- ifelse(!is.null(x$show.legend), x$show.legend, NA)
  res$inherit.aes <- ifelse(!is.null(x$inherit.aes), x$inherit.aes, TRUE)
  return(res)
}

# box plot arguments
.boxplot_params <- function(...){
  x <- list(...)
  res <- list()
  res$outlier.colour  <- x$outlier.colour
  res$outlier.shape  <- ifelse(!is.null(x$outlier.shape), x$outlier.shape, 19)
  res$outlier.size  <- ifelse(!is.null(x$outlier.size), x$outlier.size, 1.5)
  res$outlier.stroke  <- ifelse(!is.null(x$outlier.stroke), x$outlier.stroke, 0.5)
  res$notch  <- ifelse(!is.null(x$notch), x$notch, FALSE)
  res$notchwidth  <- ifelse(!is.null(x$notchwidth), x$notchwidth, 0.5)
  res$varwidth  <- ifelse(!is.null(x$varwidth), x$varwidth, FALSE)
  res$na.rm  <- ifelse(!is.null(x$na.rm), x$na.rm, FALSE)
  res$show.legend <- ifelse(!is.null(x$show.legend), x$show.legend, NA)
  res$inherit.aes <- ifelse(!is.null(x$inherit.aes), x$inherit.aes, TRUE)
  return(res)
}

.dotplot_params <- function(...){
  x <- list(...)
  res <- list()
  res$stackratio  <- ifelse(!is.null(x$stackratio ), x$stackratio, 1)
  res$width <- ifelse(!is.null(x$width), x$width, 0.9)
  return(res)
}

.violin_params <- function(...){
  x <- list(...)
  res <- list()
  res$stat  <- ifelse(!is.null(x$stat ), x$stat, "ydensity")
  res$draw_quantiles  <- x$draw_quantiles
  res$scale <- ifelse(!is.null(x$scale), x$scale, "area")
  res$trim <- ifelse(!is.null(x$trim), x$trim, TRUE)
  return(res)
}

.hist_params <- function(...){
  x <- list(...)
  res <- list()
  res$binwidth <- x$binwidth
  res$bins <- x$bins
  return(res)
}

.standard_params <- function(...){
  x <- list(...)
  res <- list()
  res$color <- ifelse(!is.null(x$color), x$color, "black")
  res$color <- ifelse(!is.null(x$colour), x$colour, res$color)
  res$linetype <- ifelse(!is.null(x$linetype), x$linetype, "solid")
  res$size <- ifelse(!is.null(x$size), x$size, 1)
  res$fill <- ifelse(!is.null(x$fill), x$fill, "black")
  res$shape <- ifelse(!is.null(x$shape), x$shape, 19)
  res
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Graphical parameters
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Change color manually
# possible value for palette: brewer palette, "grey" or a vector of colors
.ggcolor <- function(palette = NULL, ...) {
  brewerpal <- c(
    # sequential
    'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
    'YlGn', 'YlGnBu YlOrBr', 'YlOrRd',
    #Divergent
    'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral',
    # Qualitative
    'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3'
  )

  # Scientific Journal and Sci-Fi Themed Color Palettes for ggplot2
  # ggsci package: https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html
  ggscipal <- c("npg", "aaas", "lancet", "jco",
                "ucscgb", "uchicago", "simpsons", "rickandmorty")

  res <- NULL
  if (is.null(palette))
    palette <- ""
  if (length(palette) == 1) {
    if (palette %in% brewerpal)
      ggplot2::scale_color_brewer(..., palette = palette)
    else if (palette %in% ggscipal)
      .scale_color_ggsci(palette = palette)
    else if (palette == "grey")
       ggplot2::scale_color_grey(..., start = 0.8, end = 0.2)
    else if (palette == "hue")
      ggplot2::scale_color_hue(...)
  }
  else if (palette[1] != "")
    ggplot2::scale_color_manual(..., values = palette)
}

# Change fill color manually
# possible value for palette: brewer palette, "grey" or a vector of colors
.ggfill <- function(palette = NULL, ...) {
  brewerpal <- c(
    # sequential
    'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
    'YlGn', 'YlGnBu YlOrBr', 'YlOrRd',
    #Divergent
    'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral',
    # Qualitative
    'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3'
  )

  # Scientific Journal and Sci-Fi Themed Color Palettes for ggplot2
  # ggsci package: https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html
  ggscipal <- c("npg", "aaas", "lancet", "jco",
                "ucscgb", "uchicago", "simpsons", "rickandmorty")

  res <- NULL
  if (is.null(palette))
    palette <- ""
  if (length(palette) == 1) {
    if (palette %in% brewerpal)
      ggplot2::scale_fill_brewer(..., palette = palette)
    else if (palette %in% ggscipal)
      .scale_fill_ggsci(palette = palette)
    else if (palette == "grey")
      ggplot2::scale_fill_grey(..., start = 0.8, end = 0.2)
    else if (palette == "hue")
      ggplot2::scale_fill_hue(...)
  }
  else if (palette[1] != "")
    ggplot2::scale_fill_manual(..., values = palette)
}



# Helper function to use palette from ggsci package
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.scale_color_ggsci <- function(palette = c("npg", "aaas", "lancet", "jco",
                                           "ucscgb", "uchicago", "simpsons", "rickandmorty"))
{

 pal <- match.arg(palette)

  functs <- list(
    npg = ggsci::scale_color_npg(),
    aaas = ggsci::scale_color_aaas(),
    lancet = ggsci::scale_color_lancet(),
    jco = ggsci::scale_color_jco(),
    ucscgb = ggsci::scale_color_ucscgb(),
    uchicago = ggsci::scale_color_uchicago(),
    simpsons = ggsci::scale_color_simpsons(),
    rickandmorty = ggsci::scale_color_rickandmorty()
  )
  functs[[pal]]
}

.scale_fill_ggsci <- function(palette = c("npg", "aaas", "lancet", "jco",
                                           "ucscgb", "uchicago", "simpsons", "rickandmorty"))
{

  pal <- match.arg(palette)

  functs <- list(
    npg = ggsci::scale_fill_npg(),
    aaas = ggsci::scale_fill_aaas(),
    lancet = ggsci::scale_fill_lancet(),
    jco = ggsci::scale_fill_jco(),
    ucscgb = ggsci::scale_fill_ucscgb(),
    uchicago = ggsci::scale_fill_uchicago(),
    simpsons = ggsci::scale_fill_simpsons(),
    rickandmorty = ggsci::scale_fill_rickandmorty()
  )
  functs[[pal]]
}


# Set plot orientation
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.set_orientation <-
  function(p, orientation = c("vertical", "horizontal", "reverse")) {
    ori <- match.arg(orientation)
    if (ori == "horizontal") p + coord_flip()
    else if (ori == "reverse")
      p + scale_y_reverse()
    else p
  }


# Change title and labels
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.labs <- function(p, main = NULL, xlab = NULL, ylab = NULL,
                  font.main = NULL, font.x = NULL, font.y = NULL)
{

  font.main <- .parse_font(font.main)
  font.x <- .parse_font(font.x)
  font.y <- .parse_font(font.y)


  if (!is.null(main)) {
    if (main != FALSE)
      p <- p + labs(title = main)
  }

  if (!is.null(xlab)) {
    if (xlab == FALSE)
      p <- p + theme(axis.title.x = element_blank())
    else
      p <- p + labs(x = xlab)
  }

  if (!is.null(ylab)) {
    if (ylab == FALSE)
      p <- p + theme(axis.title.y = element_blank())
    else
      p <- p + labs(y = ylab)
  }

  if (!is.null(font.main))
    p <-
    p + theme(
      plot.title = element_text(
        size = font.main$size,
        lineheight = 1.0, face = font.main$face, colour = font.main$color
      )
    )
  if (!is.null(font.x))
    p <-
    p + theme(axis.title.x = element_text(
      size = font.x$size,
      face = font.x$face, colour = font.x$color
    ))
  if (!is.null(font.y))
    p <-
    p + theme(axis.title.y = element_text(
      size = font.y$size,
      face = font.y$face, colour = font.y$color
    ))



  p
}


# ticks
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.set_ticks <-
  function(ticks = TRUE, tickslab = TRUE, font.tickslab = NULL,
           xtickslab.rt = 0, ytickslab.rt = 0)
  {
    if (ticks)
      ticks <-
        element_line(colour = "black")
    else
      ticks <- element_blank()
    if (is.null(font.tickslab))
      font <- list(size = 12, face = "bold", color = "black")
    else
      font <- .parse_font(font.tickslab)
    if (tickslab) {
      xtickslab <-
        element_text(
          size = font$size, face = font$face,
          colour = font$color, angle = xtickslab.rt
        )
      ytickslab <-
        element_text(
          size = font$size, face = font$face,
          colour = font$color, angle = ytickslab.rt
        )
    }
    else {
      xtickslab <- element_blank()
      ytickslab <- element_blank()
    }
    theme(
      axis.ticks = ticks, axis.text.x = xtickslab, axis.text.y = ytickslab
    )
  }


# Change Axis limits
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.set_axis_limits <- function(xlim = NULL, ylim = NULL){
  coord_cartesian(xlim, ylim)
}


# Axis scales
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.set_scale <- function (p, xscale = c("none", "log2", "log10", "sqrt"),
                        yscale = c("none", "log2", "log10", "sqrt"),
                        format.scale = FALSE)
{

  xscale <- match.arg(xscale)
  yscale <- match.arg(yscale)
  .x <- ".x"

  if(format.scale){
    if(!requireNamespace("scales")) stop("The R package 'scales' is required.")

      if(yscale == "log2"){
        p <- p + scale_y_continuous(trans = scales::log2_trans(),
                           breaks = scales::trans_breaks("log2", function(x) 2^x),
                           labels = scales::trans_format("log2", scales::math_format(2^.x)))
      }
    else if(yscale == "log10"){
      p <- p + scale_y_continuous(trans = scales::log10_trans(),
                                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                                  labels = scales::trans_format("log10", scales::math_format(10^.x)))
    }

    if(xscale == "log2"){
      p <- p + scale_x_continuous(trans = scales::log2_trans(),
                                  breaks = scales::trans_breaks("log2", function(x) 2^x),
                                  labels = scales::trans_format("log2", scales::math_format(2^.x)))
    }
    else if(xscale == "log10"){
      p <- p + scale_x_continuous(trans = scales::log10_trans(),
                                  breaks = scales::trans_breaks("log10", function(x) 10^x),
                                  labels = scales::trans_format("log10", scales::math_format(10^.x)))
    }

  }

  else{
    if(xscale != "none")  p <- p + scale_x_continuous(trans = xscale)
    else if(yscale != "none") p <- p + scale_y_continuous(trans = yscale)
  }
p
}

# Legends
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.set_legend <- function(p, legend = c("bottom", "top", "left", "right", "none"),
                        legend.title = NULL, font.legend = NULL)
{
  if(!inherits(legend, "numeric")) legend <- match.arg(legend)
  if(is.null(legend.title)) legend.title = waiver()
  font <- .parse_font(font.legend)

   p <- p + theme(legend.position = legend) +
     labs(color = legend.title, fill = legend.title, linetype = legend.title, shape = legend.title)

   if(!is.null(font)){
     p <- p + theme(
       legend.text = element_text(size = font$size,
                                  face = font$face, colour = font$color),
       legend.title = element_text(size = font$size,
                                   face = font$face, colour = font$color)
     )
   }

   p
}


# Set ticks by
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.set_ticksby <- function(p, xticks.by = NULL, yticks.by = NULL)
  {
    .data <- p$data
    .mapping <- as.character(p$mapping)

    if(!is.null(yticks.by)) {
      y <- .data[, .mapping["y"]]
      ybreaks <- seq(0, max(y), by = yticks.by)
      p <- p + scale_y_continuous(breaks = ybreaks)
    }
    else if(!is.null(xticks.by)) {
      x <- .data[, .mapping["x"]]
      xbreaks <- seq(0, max(x), by = xticks.by)
      p <- p + scale_x_continuous(breaks = xbreaks)
    }
    p
}



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Add stat
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

.check_add.params <- function(add, add.params, error.plot, data, color, fill,  ...){
  if(color %in% names(data) & is.null(add.params$color))  add.params$color <- color
  if(fill %in% names(data) & is.null(add.params$fill))  add.params$fill <- fill
  if(is.null(add.params$color)) add.params$color <- color
  if(is.null(add.params$fill) & ("crossbar" %in% error.plot | "boxplot" %in% add | "violin" %in% add)) add.params$fill <- fill
  if(is.null(add.params$fill)) add.params$fill <- add.params$color
  #else add.params$fill <- add.params$color
  if(!is.null(list(...)$shape) & is.null(add.params$shape)) add.params$shape <- list(...)$shape
  add.params
}

# Allowed values for add are one or the combination of: "none",
#   "dotplot", "jitter", "boxplot", "mean", "mean_se", "mean_sd", "mean_ci", "mean_range",
#  "median", "median_iqr", "median_mad", "median_range"
# p_geom character, e.g "geom_line"
.add <- function(p,
                 add = NULL,
                 add.params = list(color = "black", fill = "white", shape = 19, width = 1),
                 data = NULL, position = position_dodge(0.8),
                 error.plot = c("pointrange", "linerange", "crossbar", "errorbar",
                                "upper_errorbar", "lower_errorbar", "upper_pointrange", "lower_pointrange",
                                "upper_linerange", "lower_linerange"),
                 p_geom = ""
                 )
{

  if(is.null(data)) data <- p$data
  pms <- add.params
  if("none" %in% add) add <- "none"
  error.plot = match.arg(error.plot)


  color <- ifelse(is.null(pms$color), "black",pms$color)
  fill <-  ifelse(is.null(pms$fill), "white", pms$fill)
  shape <- ifelse(is.null(pms$shape), 19, pms$shape)
  width <- ifelse(is.null(pms$width), 1, pms$width)
  shape <- ifelse(is.null(add.params$shape), 19, add.params$shape)

 # size <- ifelse(is.null(add.params$size), 1, add.params$size)


  # stat summary
  .mapping <- as.character(p$mapping)
  x <- .mapping["x"]
  y <- .mapping["y"]

  errors <- c("mean", "mean_se", "mean_sd", "mean_ci", "mean_range", "median", "median_iqr", "median_mad", "median_range")
  if(any(errors %in% add)) stat_sum <- desc_statby(data, measure.var = .mapping["y"],
                                                   grps = intersect(c(.mapping["x"], color, fill), names(data)))



  if ("boxplot" %in% add) {
    size <- ifelse(is.null(add.params$size), 1, add.params$size)
    p <- p + .geom_exec(geom_boxplot, data = data,
                        color = color, fill = fill,
                        position = position, width = width, size = size)
  }

  if ("violin" %in% add) {
    size <- ifelse(is.null(add.params$size), 1, add.params$size)
    p <- p + .geom_exec(geom_violin, data = data, trim = FALSE,
                        color = color, fill = fill,
                        position = position, width = width, size = size)
  }


  if ( "dotplot" %in% add ) {
    dotsize <- ifelse(is.null(add.params$size), 0.9, add.params$size)
    p <- p + .geom_exec(geom_dotplot, data = data, binaxis = 'y', stackdir = 'center',
                        color = color, fill = fill, dotsize = dotsize,
                        position = position, stackratio = 1.2, binwidth = add.params$binwidth)

  }
  if ( "jitter" %in% add ){
    set.seed(123)
    jitter.size <- ifelse(is.null(add.params$size), 3, add.params$size)
    ngrps <- length(intersect(names(data), c(.mapping["x"], fill, color)))
    if(p_geom == "geom_line" | ngrps == 1) .jitter = position_jitter(0.4)
    else if(ngrps > 1) .jitter <- position_dodge(0.8)

    if(!is.null(add.params$jitter)) .jitter = position_jitter(0.4)
    p <- p + .geom_exec(geom_jitter, data = data,
                        color = color, fill = fill, shape = shape, size = jitter.size,
                        position = .jitter )

  }


  # Add mean or median
  center <- intersect(c("mean", "median"), add)
  if(length(center) == 2)
    stop("Use mean or mdedian, but not both at the same time.")
    if(length(center) == 1){
      center.size <- ifelse(is.null(add.params$size), 6, add.params$size)
      names(stat_sum)[which(names(stat_sum) == center)] <- y
      p <- p + .geom_exec(geom_point, data = stat_sum, x = x, y = y,
                          color = color,  shape = shape,
                          position = position, size = center.size)
    }

  # Add errors
  errors <- c("mean_se", "mean_sd", "mean_ci", "mean_range",  "median_iqr", "median_mad", "median_range")
  errors <- intersect(errors, add)
  if(length(errors) >= 2)
    stop("Choose one these: ", paste(errors, collapse =", "))
  if(length(errors) == 1){
    errors <- strsplit(errors, "_", fixed = TRUE)[[1]]
   .center <- errors[1]
   .errors <- errors[2]
    stat_sum$ymin <- stat_sum[, .center] - stat_sum[, .errors]
    stat_sum$ymax <- stat_sum[, .center] + stat_sum[, .errors]
    names(stat_sum)[which(names(stat_sum) == .center)] <- y
    size <- ifelse(is.null(add.params$size), 1, add.params$size)


    if(error.plot %in% c("upper_errorbar", "upper_pointrange", "upper_linerange")) {
      ymin <- y
      ymax <- "ymax"
    }
    else if(error.plot %in% c("lower_errorbar", "lower_pointrange", "lower_linerange")){
      ymin <- "ymin"
      ymax <- y
    }
    else {
      ymin <- "ymin"
      ymax <- "ymax"
    }

    if(error.plot %in% c("pointrange", "lower_pointrange", "upper_pointrange"))
      p <- p + .geom_exec(geom_pointrange, data = stat_sum,
                        color = color, shape = shape, ymin = ymin, ymax = ymax,
                        position = position, size = size)
   else if(error.plot %in% c("linerange", "lower_linerange", "upper_linerange"))
      p <- p + .geom_exec(geom_linerange, data = stat_sum,
                          color = color,  ymin = ymin, ymax = ymax,
                          position = position, size = size)
    else if(error.plot %in% c("errorbar", "lower_errorbar", "upper_errorbar"))
      p <- p + .geom_exec(geom_errorbar, data = stat_sum,
                          color = color,  ymin = ymin, ymax = ymax,
                          position = position, size = size, width = 0.2)

    else if(error.plot == "crossbar")
      p <- p + .geom_exec(geom_crossbar, data = stat_sum, fill = fill,
                          color = color, ymin = "ymin", ymax = "ymax",
                          position = position, width = width, size = size)
  }

  p

}


# Calculate the mean and the SD in each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of the variable to be summariezed
# grps : column names to be used as grouping variables
.mean_sd <- function(data, varname, grps){
  summary_func <- function(x, col){
    c(mean = base::mean(x[[col]], na.rm=TRUE),
      sd = stats::sd(x[[col]], na.rm=TRUE))
  }
  data_sum <- plyr::ddply(data, grps, .fun=summary_func, varname)
  data_sum$ymin <- data_sum$mean-data_sum$sd
  data_sum$ymax <- data_sum$mean+data_sum$sd
  names(data_sum)[ncol(data_sum)-3] <- varname
  # data_sum <- plyr::rename(data_sum, c("mean" = varname))
  return(data_sum)
}


# parse font
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.parse_font <- function(font){
  if(is.null(font)) res <- NULL
  else{
    # matching size and face
    size <- grep("^[0-9]+$", font, perl = TRUE)
    face <- grep("plain|bold|italic|bold.italic", font, perl = TRUE)
    if(length(size) == 0) size <- NULL else size <- as.numeric(font[size])
    if(length(face) == 0) face <- NULL else face <- font[face]
    color <- setdiff(font, c(size, face))
    if(length(color) == 0) color <- NULL
    res <- list(size=size, face = face, color = color)
  }
  res
}







