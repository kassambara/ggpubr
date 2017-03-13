#'Generate Color Palettes
#'
#'@description Generate a palette of k colors from ggsci palettes, RColorbrewer
#'  palettes and custom color palettes. Useful to extend RColorBrewer and ggsci to support more colors.
#'@param palette Color palette. Allowed values include: \itemize{ \item
#'  \strong{Grey color palettes}: "grey" or "gray"; \item \strong{RColorBrewer
#'  palettes}, see \code{\link[RColorBrewer]{brewer.pal}} and details section.
#'  Examples of palette names include: "RdBu", "Blues", "Dark2", "Set2", ...;
#'  \item \strong{Custom color palettes}. For example, palette = c("#00AFBB",
#'  "#E7B800", "#FC4E07"); \item \strong{ggsci scientific journal palettes},
#'  e.g.: "npg", "aaas", "lancet", "jco", "ucscgb", "uchicago", "simpsons" and
#'  "rickandmorty". }
#'@param k the number of colors to generate.
#'@details
#'\strong{RColorBrewer palettes}: To display all available color
#'  palettes, type this in R:RColorBrewer::display.brewer.all(). Color palette
#'  names include:
#'
#'  \itemize{ \item \strong{Sequential palettes}, suited to ordered data that
#'  progress from low to high. Palette names include: Blues BuGn BuPu GnBu
#'  Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu
#'  YlOrBr YlOrRd. \item \strong{Diverging palettes}:Gradient colors. Names
#'  include: BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral. \item
#'  \strong{Qualitative palettes}:  Best suited to representing nominal or
#'  categorical data. Names include: Accent, Dark2, Paired, Pastel1, Pastel2,
#'  Set1, Set2, Set3.
#'  }
#'
#'@return Returns a vector of color palettes.
#'
#'@examples
#'data("iris")
#' iris$Species2 <- factor(rep(c(1:10), each = 15))
#'
#'# Generate a gradient of 10 colors
#'ggscatter(iris, x = "Sepal.Length", y = "Petal.Length",
#'  color = "Species2",
#'  palette = get_palette(c("#00AFBB", "#E7B800", "#FC4E07"), 10))
#'
#'# Scatter plot with default color palette
#'ggscatter(iris, x = "Sepal.Length", y = "Petal.Length",
#'  color = "Species")
#'
#'# RColorBrewer color palettes
#'ggscatter(iris, x = "Sepal.Length", y = "Petal.Length",
#'  color = "Species", palette = get_palette("Dark2", 3))
#'
#' # ggsci color palettes
#'ggscatter(iris, x = "Sepal.Length", y = "Petal.Length",
#'  color = "Species", palette = get_palette("npg", 3))
#'
#' # Custom color palette
#'ggscatter(iris, x = "Sepal.Length", y = "Petal.Length",
#'  color = "Species",
#'  palette = c("#00AFBB", "#E7B800", "#FC4E07"))
#'
#' # Or use this
#'ggscatter(iris, x = "Sepal.Length", y = "Petal.Length",
#'  color = "Species",
#'  palette = get_palette(c("#00AFBB", "#FC4E07"), 3))
#'
#'
#'
#'
#'
#'@export
get_palette <- function(palette = "default", k)
  {

   # Check if RColorBrewer, ggsci, hue or grey/gray color palettes
  if(.is_col_palette(palette)){
    if(palette %in% .brewerpal()) .get_brewer_pal(palette, k)
    else if(palette %in% .ggscipal()) .get_ggsci_pal(palette, k)
    else if(palette %in% c("default", "hue")){
      hues <- seq(15, 375, length = k + 1)
      grDevices::hcl(h = hues, l = 65, c = 100, alpha = 1)[1:k]
    }
    # Grey color palette
    else if(palette %in% c("grey", "gray")){
      grDevices::grey.colors(k, start = 0.2, end = 0.8)
    }
  }
  else grDevices::colorRampPalette(palette)(k)
}
