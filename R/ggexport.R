#' @include utilities.R
NULL
#'Export ggplots
#'@description Export ggplots
#'@inheritParams ggarrange
#'@param ... list of plots to be arranged into the grid. The plots can be either
#'  ggplot2 plot objects, arbitrary gtables or an object of class
#'  \code{\link{ggarrange}}.
#' @param filename File name to create on disk.
#'@param width,height plot width and height, respectively (example, width = 800,
#'  height = 800). Applied only to raster plots: "png", "jpeg", "jpg", "bmp" and
#'  "tiff".
#'@param pointsize the default pointsize of plotted text (example, pointsize =
#'  8). Used only for raster plots.
#'@param res the resolution in ppi (example, res = 250). Used only for raster
#'  plots.
#'@param verbose logical. If TRUE, show message.
#'@author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @examples
#' \dontrun{
#' require("magrittr")
#' # Load data
#' data("ToothGrowth")
#' df <- ToothGrowth
#' df$dose <- as.factor(df$dose)
#'
#' # Box plot
#' bxp <- ggboxplot(df, x = "dose", y = "len",
#'     color = "dose", palette = "jco")
#' # Dot plot
#' dp <- ggdotplot(df, x = "dose", y = "len",
#'     color = "dose", palette = "jco")
#' # Density plot
#' dens <- ggdensity(df, x = "len", fill = "dose", palette = "jco")
#'
#'# Export to pdf
#' ggarrange(bxp, dp, dens, ncol = 2) %>%
#'   ggexport(filename = "test.pdf")
#'
#' # Export to png
#' ggarrange(bxp, dp, dens, ncol = 2) %>%
#'   ggexport(filename = "test.png")
#'  }
#'
#'@export
ggexport <- function(..., plotlist = NULL, filename = NULL, ncol = NULL, nrow = NULL,
                     width = 480, height = 480, pointsize = 12, res = NA, verbose = TRUE)
{

  # File name and extension
  if(is.null(filename))
    filename <- .collapse(.random_string(), ".pdf", sep = "")
  file.ext <- .file_ext(filename)


  # Device
  dev <- .device(filename)
  dev.opts <- list(file = filename)
  if(file.ext %in% c("ps", "eps"))
    dev.opts <- dev.opts %>%
    .add_item(onefile = FALSE, horizontal = FALSE)
  else if(file.ext %in% c("png", "jpeg", "jpg", "bmp", "tiff"))
    dev.opts <- dev.opts %>%
    .add_item(width = width, height = height, pointsize = pointsize, res = res)

  if(file.ext %in% c("pdf")){
    if(!missing(width)) dev.opts <- dev.opts %>% .add_item(width = width)
    if(!missing(height)) dev.opts <- dev.opts %>% .add_item(height = height)
    if(!missing(pointsize)) dev.opts <- dev.opts %>% .add_item(pointsize = pointsize)
  }

  #width=800, height=800, pointsize=8, res=250

  # Plots
  plots <- c(list(...), plotlist)
  nb.plots <- length(plots)

  if(nb.plots == 1)
    plots <- plots[[1]]
  else if(!is.null(ncol) | !is.null(nrow)){
    plots <- ggarrange(plotlist = plots, ncol = ncol, nrow = nrow)
  }
  if(inherits(plots, "ggarrange") & .is_list(plots))
    nb.plots <- length(plots)

  if(nb.plots > 1 & file.ext %in% c("eps", "ps", "png", "jpeg", "jpg", "tiff", "bmp", "svg")){
    filename <- gsub(paste0(".", file.ext), paste0("%03d.",file.ext), filename)
    dev.opts$file <- filename
    print(filename)
  }

  do.call(dev, dev.opts)
  utils::capture.output(print(plots))
  utils::capture.output(grDevices::dev.off())

  message("file saved to ", filename)
}

