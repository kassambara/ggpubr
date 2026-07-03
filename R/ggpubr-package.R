#' @keywords internal
#'
#' @details
#'
#' General resources:
#'
#' \itemize{
#' \item \href{https://rpkgs.datanovia.com/ggpubr/}{ggpubr documentation}
#' \item \href{https://www.datanovia.com/en/blog/tag/ggpubr/}{ggpubr tutorials}
#' }
#'
#' @section P-Value Formatting:
#'
#' ggpubr provides customizable p-value formatting with predefined style presets
#' to match different journal and publication requirements. Use \code{\link{format_p_value}()}
#' for direct formatting, or set \code{p.format.style} in statistical functions.
#'
#' Available styles: \code{"default"}, \code{"apa"}, \code{"nejm"}, \code{"lancet"},
#' \code{"ama"}, \code{"graphpad"}, \code{"scientific"}.
#'
#' See \code{\link{list_p_format_styles}()} for details on each style.
#'
#' @section Package options:
#'
#' \describe{
#' \item{ggpubr.parse_aes}{logical indicating whether to parse aesthetic variable names.
#' Default is \code{TRUE}. For example, if you want \code{ggpubr} to handle non-standard column names, like \code{A-A},
#' without parsing, then set this option to \code{FALSE} using \code{options(ggpubr.parse_aes = FALSE)}.}
#' \item{ggpubr.null_device}{A function that creates an appropriate null device.
#' These include: \code{\link[cowplot:pdf_null_device]{cowplot::pdf_null_device}},
#' \code{\link[cowplot:png_null_device]{cowplot::png_null_device}},
#' \code{\link[cowplot:cairo_null_device]{cowplot::cairo_null_device}} and
#' \code{\link[cowplot:agg_null_device]{cowplot::agg_null_device}}. Default is
#' \code{\link[cowplot:pdf_null_device]{cowplot::pdf_null_device}}. This is used in
#' functions like \code{\link{as_ggplot}()}, which need to open a graphics
#' device to render ggplot objects into grid graphics objects. This function is
#' used to open a null device to avoid displaying an unnecessary blank page
#' when calling \code{\link{ggarrange}()} or \code{\link{as_ggplot}()}} }
#'
"_PACKAGE"
