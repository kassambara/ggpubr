#' @keywords internal
#'
#' @details
#'
#' General resources:
#'
#'\itemize{
#' \item \href{https://rpkgs.datanovia.com/ggpubr/}{ggpubr documentation}
#' \item \href{https://www.datanovia.com/en/blog/tag/ggpubr/}{ggpubr tutorials}
#'}
#'
#' @section Package options:
#'
#' \describe{
#' \item{ggpubr.parse_aes}{logical indicating whether to parse or not aesthetics variables names.
#' Default is \code{TRUE}. For example, if you want \code{ggpubr} to handle non-standard column names, like \code{A-A},
#' without parsing, then set this option to \code{FALSE} using \code{options(ggpubr.parse_aes = FALSE)}.}
#' \item{ggpubr.null_device}{A function that creates an appropriate null device.
#' These include: \code{\link[cowplot::pdf_null_device]{cowplot::pdf_null_device}},
#' \code{\link[cowplot::png_null_device]{cowplot::png_null_device}},
#' \code{\link[cowplot::cairo_null_device]{cowplot::cairo_null_device}} and
#' \code{\link[cowplot::agg_null_device]{cowplot::agg_null_device}}. Default is
#' \code{\link[cowplot::pdf_null_device]{cowplot::pdf_null_device}}. This is used in
#' function like \code{\link{as_ggplot}()}, which needs to open a graphics
#' device to render ggplot objects into grid graphics objects. This function is
#' used to open null device for avoiding the display of unnecessary blank page
#' when calling \code{\link{ggarrange}()} or \code{\link{as_ggplot}()}} }
#'
"_PACKAGE"
