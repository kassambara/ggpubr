#' @include utilities.R
NULL
#'Add Background Image to ggplot2
#'@description Add background image to ggplot2.
#'@param raster.img raster object to display, as returned by the function
#'  \code{readPNG()}[in \code{png} package] and \code{readJPEG()} [in \code{jpeg} package].
#'@author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @examples
#' \dontrun{
#'install.packages("png")
#'
#'# Import the image
#'img.file <- system.file(file.path("images", "background-image.png"),
#'                        package = "ggpubr")
#'img <- png::readPNG(img.file)
#'
#'# Plot with background image
#'ggplot(iris, aes(Species, Sepal.Length))+
#'  background_image(img)+
#'  geom_boxplot(aes(fill = Species), color = "white")+
#'  fill_palette("jco")
#'  }
#'
#'@export
background_image <- function(raster.img){
  annotation_raster(img,
                    xmin = -Inf, xmax = Inf,
                    ymin = -Inf, ymax = Inf)
}
