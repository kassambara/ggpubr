#' @include utilities.R
NULL
#' Draw a Textual Table
#'
#' @description Draw a textual table. \itemize{ \item \code{ggtexttable()}: draw
#'   a textual table. \item \code{ttheme()}: customize table theme. \item
#'   \code{rownames_style(), colnames_style(), tbody_style()}: helper functions
#'   to customize the table row names, column names and body. }
#' @inheritParams gridExtra::tableGrob
#' @param x a \code{data.frame} or \code{matrix}.
#' @param theme a list, as returned by the function \code{ttheme()}, defining
#'   the parameters of the table theme. Allowed values include one of
#'   \code{ttheme()} and \code{ttheme_clean()}.
#' @param base_style charcter string the table style/theme. The available themes
#'   are illustrated in the
#'   \href{http://www.sthda.com/english/rpkgs/ggpubr/files/ggtexttable-theme.pdf}{ggtexttable-theme.pdf}
#'   file. Allowed values include one of \code{c("default", "blank", "lBlack",
#'   "lBlue", "lRed", "lGreen", "lViolet", "lCyan", "lOrange", "lBlackWhite",
#'   "lBlueWhite", "lRedWhite", "lGreenWhite", "lVioletWhite", "lCyanWhite",
#'   "lOrangeWhite", "mBlack", "mBlue", "mRed", "mGreen", "mViolet", "mCyan",
#'   "mOrange", "mBlackWhite", "mBlueWhite", "mRedWhite", "mGreenWhite",
#'   "mVioletWhite", "mCyanWhite", "mOrangeWhite" )}. Note that, l = "light"; m
#'   = "medium".
#' @param colnames.style a list, as returned by the function
#'   \code{colnames_style()}, defining the style of the table column names.
#' @param rownames.style a list, as returned by the function
#'   \code{rownames_style()}, defining the style of the table row names.
#' @param tbody.style a list, as returned by the function \code{tbody_style()},
#'   defining the style of the table body.
#' @param color,face,size text font color, face and size, respectively. Allowed
#'   values for face include c("plain", "bold", "italic", "bold.italic").
#' @param fill background color.
#' @param linewidth,linecolor line width and color, respectively.
#' @return an object of class ggplot.
#'
#'
#' @examples
#' # data
#' df <- head(ToothGrowth)
#'
#' # Default table
#' # Remove row names using rows = NULL
#' ggtexttable(df, rows = NULL)
#'
#' # Medium blue (mBlue) theme
#' ggtexttable(df, rows = NULL, theme = ttheme("mBlue"))
#'
#' # Blank theme
#' ggtexttable(df, rows = NULL, theme = ttheme("mBlue"))
#'
#' # Customize the table as you want
#' ggtexttable(df, rows = NULL,
#'            theme = ttheme(
#'              colnames.style = colnames_style(color = "white", fill = "#8cc257"),
#'              tbody.style = tbody_style(color = "black", fill = c("#e8f3de", "#d3e8bb"))
#'            )
#' )
#'
#' @rdname ggtexttable
#' @export
ggtexttable <- function(x, rows = rownames(x), cols = colnames(x), vp = NULL,
                       theme = ttheme(), ...)
{
  gridExtra::tableGrob(x, rows = rows, cols = cols, vp = vp,
                       theme = theme, ...) %>%
    as_ggplot()
}

#' @export
#' @rdname ggtexttable
ttheme <- function(base_style = "default", base_size = 11, base_colour = "black", padding = unit(c(4, 4), "mm"),
                    colnames.style = colnames_style(size = base_size),
                    rownames.style = rownames_style(size = base_size),
                    tbody.style = tbody_style(size = base_size)
                      )
{

  style <- tstyle(base_style, size = base_size)

  if(!is.null(style)){
    colnames.style <- style$colnames.style
    rownames.style  <- style$rownames.style
    tbody.style <- style$tbody.style
  }


  .ttheme <- gridExtra::ttheme_default(base_size = base_size,
                                      base_colour = base_colour,
                                       padding = padding)
  .ttheme$colhead <- do.call(.add_item, c(list(.list = .ttheme$colhead), colnames.style))
  .ttheme$rowhead <- do.call(.add_item, c(list(.list = .ttheme$rowhead), rownames.style))
  .ttheme$core <- do.call(.add_item, c(list(.list = .ttheme$core), tbody.style))

  .ttheme
}




#' @export
#' @rdname ggtexttable
colnames_style <- function(color = "black", face = "bold", size = 12,
                        fill = "grey80", linewidth = 1, linecolor = "white",
                        parse = FALSE)
{
  list(
    fg_params = list(parse = parse, col = color,
                     fontface = face, fontsize = size),
    bg_params = list(fill = fill, lwd = linewidth, col = linecolor))
}

#' @export
#' @rdname ggtexttable
rownames_style <- function(color = "black", face = "italic", size = 12,
                        fill = NA, linewidth = 1, linecolor = "white",
                        parse = FALSE)
{
  list(
    fg_params = list(parse = parse, col = color,
                     fontface = face, fontsize = size, hjust = 1, x = 0.95),
    bg_params = list(fill = fill, lwd = linewidth, col = linecolor))
}


#' @export
#' @rdname ggtexttable
tbody_style <- function(color = "black", face = "plain", size = 12,
                        fill = c("grey95", "grey90"),
                        linewidth = 1, linecolor = "white",
                        parse = FALSE)
{
  list(
    fg_params = list(parse = parse, col = color,
                     fontface = face, fontsize = size),
    bg_params = list(fill = fill, lwd = linewidth, col = linecolor))
}



#::::::::::::::::::::::::::::::::::::::::
# Helper function
#::::::::::::::::::::::::::::::::::::::::

# Define table style
tstyle <- function(pal, size = 12){

  allowed.palettes = c("default", "blank",
                      "lBlack", "lBlue", "lRed", "lGreen", "lViolet", "lCyan", "lOrange",
                      "lBlackWhite", "lBlueWhite", "lRedWhite", "lGreenWhite", "lVioletWhite", "lCyanWhite", "lOrangeWhite",
                      "mBlack", "mBlue", "mRed", "mGreen", "mViolet", "mCyan", "mOrange",
                      "mBlackWhite", "mBlueWhite", "mRedWhite", "mGreenWhite", "mVioletWhite", "mCyanWhite", "mOrangeWhite"
                      )
  if(!(pal %in% allowed.palettes ))
    stop(pal, " is not a supported palette")

  style <- switch(pal,

                  blank = list(
                    colnames.style = colnames_style(fill = NA, linecolor = NA, size = size),
                    rownames.style = rownames_style(fill = NA, linecolor = NA, size = size),
                    tbody.style = tbody_style(fill = NA, linecolor = NA, size = size)
                  ),

                  lBlack = list(
                    colnames.style = colnames_style( color = "black", fill = "white", size = size,
                                                     linecolor = NA, linewidth = 1),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#D9D9D9", "white"),
                                              linecolor = NA, size = size)
                  ),

                  lBlue = list(
                    colnames.style = colnames_style( color = "#2F5D94", fill = "white",
                                                     linecolor = NA, linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "#2F5D94", fill = c("#DAE6F2", "white"),
                                              linecolor = NA, size = size)
                  ),

                  lRed = list(
                    colnames.style = colnames_style( color = "#A3262A", fill = "white",
                                                     linecolor = NA, linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "#A3262A", fill = c("#F7DBDA", "white"),
                                              linecolor = NA, size = size)
                  ),

                  lGreen = list(
                    colnames.style = colnames_style( color = "#7F993A", fill = "white",
                                                     linecolor = NA, linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "#7F993A", fill = c("#E8F3DE", "white"),
                                              linecolor = NA, size = size)
                  ),

                  lViolet = list(
                    colnames.style = colnames_style( color = "#67417B", fill = "white",
                                                     linecolor = NA, linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "#67417B", fill = c("#E6DEEC", "white"),
                                              linecolor = NA, size = size)
                  ),

                  lCyan = list(
                    colnames.style = colnames_style( color = "#00889E", fill = "white",
                                                     linecolor = NA, linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "#00889E", fill = c("#D5EFF4", "white"),
                                              linecolor = NA, size = size)
                  ),

                  lOrange = list(
                    colnames.style = colnames_style( color = "#F15F00", fill = "white",
                                                     linecolor = NA, linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "#F15F00", fill = c("#FBE5D4", "white"),
                                              linecolor = NA, size = size)
                  ),

                  lBlackWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "black", size = size,
                                                     linecolor = "black", linewidth = 1),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = "white",
                                              linecolor = "black", size = size)
                  ),

                  lBlueWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "#477DC0", size = size,
                                                     linecolor = "#477DC0", linewidth = 1),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = "white",
                                              linecolor = "#477DC0", size = size)
                  ),

                  lRedWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "#D04042", size = size,
                                                     linecolor = "#D04042", linewidth = 1),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = "white",
                                              linecolor = "#D04042", size = size)
                  ),

                  lGreenWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "#8CC257", size = size,
                                                     linecolor = "#8CC257", linewidth = 1),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = "white",
                                              linecolor = "#8CC257", size = size)
                  ),

                  lVioletWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "#895AA3", size = size,
                                                     linecolor = "#895AA3", linewidth = 1),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = "white",
                                              linecolor = "#895AA3", size = size)
                  ),

                  lCyanWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "#00AEC9", size = size,
                                                     linecolor = "#00AEC9", linewidth = 1),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = "white",
                                              linecolor = "#00AEC9", size = size)
                  ),

                  lOrangeWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "#FB8F2D", size = size,
                                                     linecolor = "#FB8F2D", linewidth = 1),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = "white",
                                              linecolor = "#FB8F2D", size = size)
                  ),

                  mBlack = list(
                    colnames.style = colnames_style( color = "white", fill = "black", size = size,
                                                     linecolor = "white", linewidth = 1),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#A6A6A6", "#D9D9D9"),
                                              linecolor = "white", size = size)
                  ),

                  mBlue = list(
                    colnames.style = colnames_style( color = "white", fill = "#477DC0",
                                                     linecolor = "white", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#B5CBE5", "#DAE6F2"),
                                              linecolor = "white", size = size)
                  ),

                  mRed = list(
                    colnames.style = colnames_style( color = "white", fill = "#D04042",
                                                     linecolor = "white", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#EFB4B5", "#F7DBDA"),
                                              linecolor = "white", size = size)
                  ),

                  mGreen = list(
                    colnames.style = colnames_style( color = "white", fill = "#8CC257",
                                                     linecolor = "white", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#D3E8BB", "#E8F3DE"),
                                              linecolor = "white", size = size)
                  ),

                  mViolet = list(
                    colnames.style = colnames_style( color = "white", fill = "#895AA3",
                                                     linecolor = "white", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#D0BDDA", "#E6DEEC"),
                                              linecolor = "white", size = size)
                  ),

                  mCyan = list(
                    colnames.style = colnames_style( color = "white", fill = "#00AEC9",
                                                     linecolor = "white", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#ADDFEA", "#D5EFF4"),
                                              linecolor = "white", size = size)
                  ),

                  mOrange = list(
                    colnames.style = colnames_style( color = "white", fill = "#EC8C2C",
                                                     linecolor = "white", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#FDD2AF", "#FFE9D7"),
                                              linecolor = "white", size = size)
                  ),
                  mBlackWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "black", size = size,
                                                     linecolor = "black", linewidth = 1),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#A6A6A6", "white"),
                                              linecolor = "black", size = size)
                  ),

                  mBlueWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "#477DC0",
                                                     linecolor = "#477DC0", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#B5CBE5", "white"),
                                              linecolor = "#477DC0", size = size)
                  ),

                  mRedWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "#D04042",
                                                     linecolor = "#D04042", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#EFB4B5", "white"),
                                              linecolor = "#D04042", size = size)
                  ),

                  mGreenWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "#8CC257",
                                                     linecolor = "#8CC257", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#D3E8BB", "white"),
                                              linecolor = "#8CC257", size = size)
                  ),

                  mVioletWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "#895AA3",
                                                     linecolor = "#895AA3", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#D0BDDA", "white"),
                                              linecolor = "#895AA3", size = size)
                  ),

                  mCyanWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "#00AEC9",
                                                     linecolor = "#00AEC9", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#ADDFEA", "white"),
                                              linecolor = "#00AEC9", size = size)
                  ),

                  mOrangeWhite = list(
                    colnames.style = colnames_style( color = "white", fill = "#EC8C2C",
                                                     linecolor = "#EC8C2C", linewidth = 1, size = size),
                    rownames.style = rownames_style(size = size),
                    tbody.style = tbody_style(color = "black", fill = c("#FDD2AF", "white"),
                                              linecolor = "#EC8C2C", size = size)
                  )
                  ,
                  NULL
  )

  style

}


rgb(71, 125, 192, maxColorValue=255)



