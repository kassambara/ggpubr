#' Arrange Multiple ggplots
#' @description Arrange multiple ggplots on the same page.
#' @inheritParams cowplot::plot_grid
#' @return a ggplot
#' @author Alboukadel Kassambara <alboukadel.kassambara@@gmail.com>
#' @examples
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
#' ggarrange(bxp, dp, dens, ncol = 2)
#'
#' @export
ggarrange <- function(..., plotlist = NULL, ncol = NULL, nrow = NULL, labels = NULL)
  {
  plots <- c(list(...), plotlist)
  nb.plots <- length(plots)
  nb.plots.per.page <- .nbplots_per_page(ncol, nrow)

  # One unique page
  if(nb.plots.per.page >= nb.plots)
    cowplot::plot_grid(plotlist = plots, ncol = ncol, nrow = nrow,
                     labels = labels)
  # Multiple page
  else{
    plots.split <- split(plots, ceiling(seq_along(plots)/nb.plots.per.page))

    purrr::map(plots.split, .plot_grid,
                ncol = ncol, nrow = nrow)
  }
}




# Compute number of plots per page
.nbplots_per_page <- function(ncol = NULL, nrow = NULL){

  if(!is.null(ncol) & !is.null(nrow))
    ncol * nrow
  else if(!is.null(ncol))
    ncol
  else if(!is.null(nrow))
    nrow
  else Inf
}


.plot_grid <- function(plotlist, ... ){
  cowplot::plot_grid(plotlist = plotlist, ...)
}
