#' geom_signif exported from ggsignif package
#'
#' See \code{ggsignif::\link[ggsignif:stat_signif]{geom_signif}} for details.
#'
#' @name geom_signif
#' @rdname geom_signif
#' @examples
#' ggplot2::ggplot(iris, ggplot2::aes(Species, Sepal.Length)) +
#'   ggplot2::geom_boxplot() +
#'   geom_signif(
#'     comparisons = list(c("versicolor", "virginica")),
#'     map_signif_level = TRUE
#'   )
#' @keywords internal
#' @export
#' @importFrom ggsignif geom_signif
NULL
