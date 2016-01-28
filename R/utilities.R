#' @import ggplot2

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Execute a geom_* function from ggplot2
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# geomfunc : gem_*() functions
# data data for mapping
# ... argument accepeted by the function
.geom_exec <- function (geomfunc, data = NULL,
                        position = NULL,  ...) {
  params <- list(...)

  mapping <-
    list() # option to pass to mapping aes() or aes_string()
  option <- list() # option to the geom_*()

  allowed_options <- c(
    # general
    "color", "colour", "linetype", "fill", "size", "shape", "width",
    "alpha", "na.rm", "lwd", "pch", "cex", "position", "stat", "geom",
    "show.legend", "inherit.aes", "fun.args",
    # boxplot
    "outlier.colour", "outlier.shape", "outlier.size",
    "outlier.stroke", "notch", "notchwidth", "varwidth",
    # dot plot
    "binwidth", "binaxis", "method", "binpositions",
    "stackdir", "stackratio", "dotsize",
    # Violin
    "trim",
    # error
    "ymin", "ymax", "xmin", "xmax"
  )

  columns <- colnames(data)
  for (key in names(params)) {
    value <- params[[key]]
    if (is.null(value)) {

    }
    else if (value %in% columns) {
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
  option[["mapping"]] <- do.call(ggplot2::aes_string, mapping)
  return(do.call(geomfunc, option))
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

  res <- NULL
  if (is.null(palette))
    palette <- ""
  if (length(palette) == 1) {
    if (palette %in% brewerpal)
      ggplot2::scale_color_brewer(palette = palette)
    else if (palette == "grey")
       ggplot2::scale_color_grey()
    else if (palette == "hue")
      ggplot2::scale_color_hue(...)
  }
  else if (palette[1] != "")
    ggplot2::scale_color_manual(values = palette, ...)
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

  res <- NULL
  if (is.null(palette))
    palette <- ""
  if (length(palette) == 1) {
    if (palette %in% brewerpal)
      ggplot2::scale_fill_brewer(palette = palette)
    else if (palette == "grey")
      ggplot2::scale_fill_grey()
    else if (palette == "hue")
      ggplot2::scale_fill_hue(...)
  }
  else if (palette[1] != "")
    ggplot2::scale_fill_manual(values = palette, ...)
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


# Add mean point to a plot
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.add_mean <- function(shape = 18, size = 5, color = "red") {
  stat_summary(
    fun.y = base::mean,
    geom = 'point', shape = shape,
    size = size, colour = color
  )
}

# Change title and labels
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.labs <- function(p, main = NULL, xlab = NULL, ylab = NULL,
                  font.main = NULL, font.x = NULL, font.y = NULL)
{
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
          size = as.numeric(font.main[1]),
          lineheight = 1.0, face = font.main[2], colour = font.main[3]
        )
      )
  if (!is.null(font.x))
    p <-
      p + theme(axis.title.x = element_text(
        size = as.numeric(font.x[1]),
        face = font.x[2], colour = font.x[3]
      ))
  if (!is.null(font.y))
    p <-
      p + theme(axis.title.y = element_text(
        size = as.numeric(font.y[1]),
        face = font.y[2], colour = font.y[3]
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
      font <- c(12, "bold", "black")
    else
      font <- font.tickslab
    if (tickslab) {
      xtickslab <-
        element_text(
          size = as.numeric(font[1]), face = font[2],
          colour = font[3], angle = xtickslab.rt
        )
      ytickslab <-
        element_text(
          size = as.numeric(font[1]), face = font[2],
          colour = font[3], angle = ytickslab.rt
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

   p <- p + theme(legend.position = legend) +
     labs(color = legend.title, fill = legend.title, linetype = legend.title)
   if(!is.null(font.legend))
     p <- p + theme(legend.text=element_text(size = as.numeric(font.legend[1]),
                                             face = font.legend[2], colour = font.legend[3]))

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
.add_dot <- function (p, add = c("none", "dotplot", "jitter"),
                      add.params = list(color = "black", shape = 19, size = 1) )
{
   data <- p$data
   add  <-  match.arg(add)
   color <- ifelse(is.null(add.params$color), "black", add.params$color)
   fill <- ifelse(is.null(add.params$fill), color, add.params$fill)
   shape <- ifelse(is.null(add.params$shape), 19, add.params$shape)
   .jitter <- ifelse(is.null(add.params$jitter), 0.3, add.params$jitter)
   if(is.null(add.params$size)){
     if(add == "dotplot") size = 0.8
     else if(add == "jitter") size = 4
   }
   size <- ifelse(is.null(add.params$size), 1, add.params$size)

   if ( add == "dotplot") {
     p <- p + .geom_exec(geom_dotplot, data = data, binaxis = 'y', stackdir = 'center',
                         color = color, fill = fill, dotsize = size,
                         position = position_dodge(0.8), stackratio = 1.2, binwidth = add.params$binwidth)

   }
   if (add == "jitter") {
     p <- p + .geom_exec(geom_jitter, data = data,
                         color = color, fill = fill, shape = shape, size = size,
                         position = position_jitter(.jitter) )

   }
   p

}


.add <- function(p,
                 add = c("none", "pointrange", "crossbar", "boxplot", "dotplot"),
                 add.params = list(color = "black", fill = "white", shape = 19, width = 1),
                 data = NULL, position = position_dodge(0.8)
                 )
{

  if(is.null(data)) data <- p$data
  pms <- add.params
  if("none" %in% add) add <- "none"

  color <- ifelse(is.null(pms$color), "black",pms$color)
  fill <-  ifelse(is.null(pms$fill), "white", pms$fill)
  shape <- ifelse(is.null(pms$shape), 19, pms$shape)
  width <- ifelse(is.null(pms$width), 1, pms$width)
  shape <- ifelse(is.null(add.params$shape), 19, add.params$shape)

  if(is.null(add.params$size)){
    if("dotplot" %in% add) add.params$size = 0.9
    else if("jitter" %in% add) add.params$size = 3
  }
  size <- ifelse(is.null(add.params$size), 1, add.params$size)


  # stat summary
  .mapping <- as.character(p$mapping)
  if( any( c("pointrange", "crossbar") %in% add))
    stat_sum <- .mean_sd(data, varname = .mapping["y"],
                         grps = intersect(names(data), c(.mapping["x"], color, fill)))

  if ( "dotplot" %in% add ) {
    p <- p + .geom_exec(geom_dotplot, data = data, binaxis = 'y', stackdir = 'center',
                        color = color, fill = fill, dotsize = size,
                        position = position_dodge(0.8), stackratio = 1.2, binwidth = add.params$binwidth)

  }
  if ( "jitter" %in% add ){
    ngrps <- length(intersect(names(data), c(.mapping["x"], fill, color)))
    if(ngrps > 1) .jitter <- position_dodge(0.8) else .jitter <- position_jitter(0.4)
    if(!is.null(add.params$jitter)) .jitter = position_jitter(0.4)
    p <- p + .geom_exec(geom_jitter, data = data,
                        color = color, fill = fill, shape = shape, size = size,
                        position = .jitter )

  }

  if ("boxplot" %in% add) {
    p <- p + .geom_exec(geom_boxplot, data = data,
                        color = color, fill = fill,
                        position = position_dodge(0.8), width = width, size = size)
  }

  if ("violin" %in% add) {
    p <- p + .geom_exec(geom_violin, data = data, trim = FALSE,
                        color = color, fill = fill,
                        position = position, width = width, size = size)
  }

  if("crossbar" %in% add){
    p <- p + .geom_exec(geom_crossbar, data = stat_sum, fill = fill,
                        color = color, ymin = "ymin", ymax = "ymax",
                        position = position, width = width, size = size)
  }
  if("pointrange" %in% add){
    p <- p + .geom_exec(geom_pointrange, data = stat_sum,
                        color = color, ymin = "ymin", ymax = "ymax",
                        position = position, size = size)
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



