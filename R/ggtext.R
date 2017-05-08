#' @include utilities.R ggpar.R stat_chull.R stat_conf_ellipse.R
NULL
#' Text
#' @description Add text to a plot.
#' @inheritParams ggscatter
#' @param data a data frame
#' @param x,y x and y variables for drawing.
#' @param label the name of the column containing point labels. Can be also a
#'   character vector with length = nrow(data).
#' @param color text font color.
#' @param size text font size.
#' @param face text font style. Allowed values are one of c("plain", "bold",
#'   "italic", "bold.italic").
#' @param family character vector specifying font family.
#' @param label.select character vector specifying some labels to show.
#' @param repel a logical value, whether to use ggrepel to avoid overplotting
#'   text labels or not.
#' @param label.rectangle logical value. If TRUE, add rectangle underneath the
#'   text, making it easier to read.
#' @param ggp a ggplot. If not NULL, points are added to an existing plot.
#' @param ... other arguments to be passed to \code{\link{ggpar}}.
#' @details The plot can be easily customized using the function ggpar(). Read
#'   ?ggpar for changing: \itemize{ \item main title and axis labels: main,
#'   xlab, ylab \item axis limits: xlim, ylim (e.g.: ylim = c(0, 30)) \item axis
#'   scales: xscale, yscale (e.g.: yscale = "log2") \item color palettes:
#'   palette = "Dark2" or palette = c("gray", "blue", "red") \item legend title,
#'   labels and position: legend = "right"  }
#' @seealso \code{\link{ggpar}}
#' @examples
#' # Load data
#' data("mtcars")
#' df <- mtcars
#' df$cyl <- as.factor(df$cyl)
#' df$name <- rownames(df)
#' head(df[, c("wt", "mpg", "cyl")], 3)
#'
#' # Textual annotation
#' # +++++++++++++++++
#' ggtext(df, x = "wt", y = "mpg",
#'    color = "cyl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'    label = "name", repel = TRUE)
#'
#' # Add rectangle around label
#' ggtext(df, x = "wt", y = "mpg",
#'    color = "cyl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#'    label = "name", repel = TRUE,  label.rectangle = TRUE)
#'
#'
#' @export
ggtext <- function(data, x, y, label = NULL,
                  color = "black",  palette = NULL,
                  size = 12,  face = "plain", family = "",
                  label.select = NULL, repel = FALSE, label.rectangle = FALSE,
                  ggp = NULL, ggtheme = theme_pubr(),
                      ...)
{

  if(length(label) >1){
    if(length(label) != nrow(data))
      stop("The argument label should be a column name or a vector of length = nrow(data). ",
           "It seems that length(label) != nrow(data)")
    else data$label.xx <- label
    label <- "label.xx"
  }

  if(is.null(ggp)) p <- ggplot(data, aes_string(x, y))
  else p <- ggp

  # Add textual annotation
  # ++++++
  alpha <- 1
  if(!is.null(list(...)$alpha)) alpha <- list(...)$alpha

  if(!is.null(label)) {
    lab_data <- data
    # Select some labels to show
    if(!is.null(label.select))
      lab_data  <- subset(lab_data, lab_data[, label, drop = TRUE] %in% label.select,
                          drop = FALSE)

    if(repel){
      ggfunc <- ggrepel::geom_text_repel
      if(label.rectangle) ggfunc <- ggrepel::geom_label_repel
        p <- p + geom_exec(ggfunc, data = lab_data, x = x, y = y,
                          label = label, fontface = face,
                          family = family,
                          size = size/3, color = color,
                          alpha = alpha,
                          box.padding = unit(0.35, "lines"),
                          point.padding = unit(0.3, "lines"),
                          force = 1)
    }
    else{
      ggfunc <- geom_text
      vjust  <- -0.7
      if(label.rectangle) {
        ggfunc <- geom_label
        vjust <- -0.4
        }
      p <- p + geom_exec(ggfunc, data = lab_data, x = x, y = y, color = color,
                          label = label, fontface = face, family = family,
                          size = size/3, color = color,
                          vjust = vjust, alpha = alpha)
    }
  }
  p <- ggpar(p, palette = palette, ggtheme = ggtheme, ...)
  if(family != "")
    p <- p + theme(text = element_text(family = family))
  p
}




