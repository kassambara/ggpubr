#' Global Options for GGPubr
#' @description Displays allowed global options in ggpubr.
#' @examples
#'
#' ggpubr_options()
#'
#' @export
ggpubr_options <- function(){
  list(
    ggpubr.parse_aes = TRUE,
    ggpubr.null_device = cowplot::pdf_null_device
  )
}
