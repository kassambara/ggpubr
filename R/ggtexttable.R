#' @include utilities.R
NULL
#' Draw a Textual Table
#' @description Draw a textual table. \itemize{ \item \code{ggtexttable()}: draw
#'  a textual table. \item \code{ttheme()}: customize table theme. \item
#'  \code{rownames_style(), colnames_style(), tbody_style()}: helper functions
#'  to customize the table row names, column names and body.
#'  \item \code{table_cell_font()}: access to a table cell for changing the text font (size and face).
#'  \item \code{table_cell_bg()}: access to a table cell for changing the background (fill, color, linewidth).
#'  \item \code{tab_cell_crossout()}: cross out a table cell.
#'  \item \code{tab_ncol(), tab_nrow()}: returns, respectively, the number of columns and rows in a ggtexttable.
#'  \item \code{tab_add_hline()}: Creates horizontal lines or separators at the top or the bottom side of a given specified row.
#'  \item \code{tab_add_vline()}: Creates vertical lines or separators at the right or the left side of a given specified column.
#'  \item \code{tab_add_border(), tbody_add_border(), thead_add_border()}: Add borders to table; tbody is for table body and thead is for table head.
#'   }
#'@inheritParams gridExtra::tableGrob
#'@param x a \code{data.frame} or \code{matrix}.
#'@param theme a list, as returned by the function \code{ttheme()}, defining the
#'  parameters of the table theme. Allowed values include one of \code{ttheme()}
#'  and \code{ttheme_clean()}.
#'@param base_style character string the table style/theme. The available themes
#'  are illustrated in the
#'  \href{https://rpkgs.datanovia.com/ggpubr/files/ggtexttable-theme.pdf}{ggtexttable-theme.pdf}
#'   file. Allowed values include one of \code{c("default", "blank", "classic",
#'  "minimal", "light", "lBlack", "lBlue", "lRed", "lGreen", "lViolet", "lCyan",
#'  "lOrange", "lBlackWhite", "lBlueWhite", "lRedWhite", "lGreenWhite",
#'  "lVioletWhite", "lCyanWhite", "lOrangeWhite", "mBlack", "mBlue", "mRed",
#'  "mGreen", "mViolet", "mCyan", "mOrange", "mBlackWhite", "mBlueWhite",
#'  "mRedWhite", "mGreenWhite", "mVioletWhite", "mCyanWhite", "mOrangeWhite" )}.
#'  Note that, l = "light"; m = "medium".
#'@param colnames.style a list, as returned by the function
#'  \code{colnames_style()}, defining the style of the table column names.
#'  Considered only when \code{base_size = "default"}.
#'@param rownames.style a list, as returned by the function
#'  \code{rownames_style()}, defining the style of the table row names.
#'  Considered only when \code{base_size = "default"}.
#'@param tbody.style a list, as returned by the function \code{tbody_style()},
#'  defining the style of the table body. Considered only when \code{base_size =
#'  "default"}.
#'@param color,face,size text font color, face and size, respectively. Allowed
#'  values for face include c("plain", "bold", "italic", "bold.italic").
#'@param fill background color.
#'@param linewidth,linecolor line width and color, respectively.
#'@param alpha numeric value specifying fill color transparency.
#' Value should be in [0, 1], where 0 is full transparency and 1 is no transparency.
#' @param at.row a numeric vector of row indexes; for example \code{at.row = c(1, 2)}.
#' @param row.side row side to which the horinzotal line should be added. Can be one of \code{c("bottom", "top")}.
#' @param from.column integer indicating the column from which to start drawing the horizontal line.
#' @param to.column integer indicating the column to which the horizontal line should end.
#' @param linetype line type
#' @param at.column a numeric vector of column indexes; for example \code{at.column = c(1, 2)}.
#' @param col.side column side to which the vertical line should be added. Can be one of \code{c("left", "right")}.
#' @param from.row integer indicating the row from which to start drawing the horizontal line.
#' @param to.row integer indicating the row to which the vertical line should end.
#'@param ... extra parameters for text justification, e.g.: hjust and x. Default
#'  is "centre" for the body and header, and "right" for the row names. Left
#'  justification: \code{hjust = 0, x = 0.1}. Right justification: \code{hjust = 1, x = 0.9}.
#'@return an object of class ggplot.
#'
#'
#' @examples
#' # data
#' df <- head(iris)
#'
#' # Default table
#' # Remove row names using rows = NULL
#' ggtexttable(df, rows = NULL)
#'
#' # Blank theme
#' ggtexttable(df, rows = NULL, theme = ttheme("blank"))
#'
#' # light theme
#' ggtexttable(df, rows = NULL, theme = ttheme("light"))
#'
#' # classic theme
#' ggtexttable(df, rows = NULL, theme = ttheme("classic"))
#'
#' # minimal theme
#' ggtexttable(df, rows = NULL, theme = ttheme("minimal"))
#'
#' # Medium blue (mBlue) theme
#' ggtexttable(df, rows = NULL, theme = ttheme("mBlue"))
#'
#'
#' # Customize the table as you want
#' ggtexttable(df, rows = NULL,
#'            theme = ttheme(
#'              colnames.style = colnames_style(color = "white", fill = "#8cc257"),
#'              tbody.style = tbody_style(color = "black", fill = c("#e8f3de", "#d3e8bb"))
#'            )
#' )
#'
#' # Use RColorBrewer palette
#' # Provide as many fill color as there are rows in the table body, here nrow = 6
#'ggtexttable(df,
#'            theme = ttheme(
#'              colnames.style = colnames_style(fill = "white"),
#'              tbody.style = tbody_style(fill = get_palette("RdBu", 6))
#'            )
#')
#'
#'# Text justification
#'#::::::::::::::::::::::::::::::::::::::::::::::
#'# Default is "centre" for the body and header, and "right" for the row names.
#'# Left justification: hjust=0, x=0.1
#'# Right justification: hjust=1, x=0.9
#'tbody.style = tbody_style(color = "black",
#'    fill = c("#e8f3de", "#d3e8bb"), hjust=1, x=0.9)
#'ggtexttable(head(iris), rows = NULL,
#'            theme = ttheme(
#'              colnames.style = colnames_style(color = "white", fill = "#8cc257"),
#'              tbody.style = tbody.style
#'            )
#')
#'
#'# Access and modify the font and
#'# the background of table cells
#'# :::::::::::::::::::::::::::::::::::::::::::::
#'tab <- ggtexttable(head(iris), rows = NULL,
#'                   theme = ttheme("classic"))
#'tab <- table_cell_font(tab, row = 3, column = 2,
#'                       face = "bold")
#'tab <- table_cell_bg(tab, row = 4, column = 3, linewidth = 5,
#'                     fill="darkolivegreen1", color = "darkolivegreen4")
#'tab
#'
#'
#' # Combine density plot and summary table
#'#:::::::::::::::::::::::::::::::::::::
#'# Density plot of "Sepal.Length"
#'density.p <- ggdensity(iris, x = "Sepal.Length",
#'                       fill = "Species", palette = "jco")
#'
#'# Draw the summary table of Sepal.Length
#'# Descriptive statistics by groups
#'stable <- desc_statby(iris, measure.var = "Sepal.Length",
#'                      grps = "Species")
#'stable <- stable[, c("Species", "length", "mean", "sd")]
#'stable.p <- ggtexttable(stable, rows = NULL,
#'                        theme = ttheme("mOrange"))
#'
#'# Arrange the plots on the same page
#'ggarrange(density.p, stable.p,
#'          ncol = 1, nrow = 2,
#'          heights = c(1, 0.5))
#'
#'@rdname ggtexttable
#'@export
ggtexttable <- function(x, rows = rownames(x), cols = colnames(x), vp = NULL,
                       theme = ttheme(), ...)
{
  style <- attr(theme, "style")
  res <- gridExtra::tableGrob(x, rows = rows, cols = cols, vp = vp,
                                theme = theme, ...)
  if(style == "minimal"){
    # minimal = blank theme + left vertical line
    res <- tab_add_vline(
      res, at.column = 2:tab_ncol(res), col.side = "left",
      from.row = 2, to.row = tab_nrow(res)
      )
  }
  else if(style == "light"){
    # light = blank theme + horizontal line
    res <- tab_add_hline(
      res, at.row = c(1, tab_nrow(res)), row.side = "bottom",
      linewidth = 1.5, linetype = 1, from.column = 1
    )
  }
  .grob <- res
  res <- as_ggplot(res)
  attr(res, "ggtexttableGrob") <- .grob
  return(res)
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
    if(missing(colnames.style)) colnames.style <- style$colnames.style
    if(missing(rownames.style)) rownames.style  <- style$rownames.style
    if(missing(tbody.style)) tbody.style <- style$tbody.style
  }

  .ttheme <- gridExtra::ttheme_default(base_size = base_size,
                                      base_colour = base_colour,
                                       padding = padding)

  .ttheme$colhead <- do.call(.add_item, c(list(.list = .ttheme$colhead), colnames.style))
  .ttheme$rowhead <- do.call(.add_item, c(list(.list = .ttheme$rowhead), rownames.style))
  .ttheme$core <- do.call(.add_item, c(list(.list = .ttheme$core), tbody.style))
  attr(.ttheme, "style") <- base_style

  .ttheme
}




#' @export
#' @rdname ggtexttable
colnames_style <- function(color = "black", face = "bold", size = 12,
                        fill = "grey80", linewidth = 1, linecolor = "white",
                        parse = FALSE, ...)
{

  list(
    fg_params = list(parse = parse, col = color,
                     fontface = face, fontsize = size) %>%
      .add_item(...), # Accept extra parameters
    bg_params = list(fill = fill, lwd = linewidth, col = linecolor))
}

#' @export
#' @rdname ggtexttable
rownames_style <- function(color = "black", face = "italic", size = 12,
                        fill = NA, linewidth = 1, linecolor = "white",
                        parse = FALSE, ...)
{
  list(
    fg_params = list(parse = parse, col = color,
                     fontface = face, fontsize = size, hjust = 1, x = 0.95) %>%
      .add_item(...), # Accept extra parameters
    bg_params = list(fill = fill, lwd = linewidth, col = linecolor))
}


#' @export
#' @rdname ggtexttable
tbody_style <- function(color = "black", face = "plain", size = 12,
                        fill = c("grey95", "grey90"),
                        linewidth = 1, linecolor = "white",
                        parse = FALSE, ...)
{
  list(
    fg_params = list(parse = parse, col = color,
                     fontface = face, fontsize = size)%>%
      .add_item(...), # Accept extra parameters
    bg_params = list(fill = fill, lwd = linewidth, col = linecolor))
}


#' @export
#' @rdname ggtexttable
#' @param tab an object from \code{ggtexttable} or from \code{gridExtra::tableGrob()}.
#' @param row,column an integer specifying the row and the column numbers for the cell of interest.
table_cell_font <- function(tab, row, column, face = NULL, size = NULL)
{
  tabGrob <- get_tablegrob(tab)
  cells <- expand.grid(row = row, column = column)
  for(i in 1:nrow(cells)){
    tc <- .find_cell(tabGrob, cells$row[i], cells$column[i], "core-fg")
    tabGrob$grobs[tc][[1]][["gp"]] <- grid::gpar(fontface = face, fontsize = size)
  }
  tab_return_same_class_as_input(tabGrob, input = tab)
}

#' @export
#' @rdname ggtexttable
table_cell_bg <- function(tab, row, column, fill = NULL, color = NULL, linewidth = NULL, alpha = NULL)
{
  tabGrob <- get_tablegrob(tab)
  cells <- expand.grid(row = row, column = column)
  for(i in 1:nrow(cells)){
    tc <- .find_cell(tabGrob, cells$row[i], cells$column[i], "core-bg")
    tabGrob$grobs[tc][[1]][["gp"]] <- grid::gpar(
      fill = fill, col = color, lwd = linewidth, alpha = alpha
      )
  }
  tab_return_same_class_as_input(tabGrob, input = tab)
}

.find_cell <- function(tab, row, column, name="core-fg"){
  l <- tab$layout
  which(l$t==row & l$l==column & l$name==name)
}

#' @export
#' @rdname ggtexttable
#' @param reduce.size.by Numeric value in [0, 1] to reduce the size by.
tab_cell_crossout <- function(tab, row, column, linetype = 1, linewidth = 1, linecolor = "black", reduce.size.by = 0){
  required_package("gtable")
  tabgrob <- get_tablegrob(tab)
  crosses <- replicate(
    n = length(row),
    tab_cross(linetype = linetype, linewidth = linewidth, linecolor = linecolor, reduce.size.by = reduce.size.by),
    simplify = FALSE
  )
  tabgrob <- gtable::gtable_add_grob(
    tabgrob, grobs = crosses,
    t = row, b = row,
    l = column, r = column
  )
  tab_return_same_class_as_input(tabgrob, input = tab)
}

tab_cross <- function(linetype = 1, linewidth = 1, linecolor = "black", reduce.size.by = 0){
  reduce <- reduce.size.by/2
  grid::grobTree(
    grid::segmentsGrob( # diagonal line ul -> lr
      x0 = unit(0+reduce,"npc"),
      y0 = unit(1-reduce,"npc"),
      x1 = unit(1-reduce,"npc"),
      y1 = unit(0+reduce,"npc"),
      gp = grid::gpar( lty = linetype, lwd = linewidth, col = linecolor, reduce = reduce)
    ),
    grid::segmentsGrob( # diagonal line ll -> ur
      x0 = unit(0+reduce,"npc"),
      y0 = unit(0+reduce,"npc"),
      x1 = unit(1-reduce,"npc"),
      y1 = unit(1-reduce,"npc"),
      gp = grid::gpar( lty = linetype, lwd = linewidth, col = linecolor)
    )
  )
}


#' @export
#' @rdname  ggtexttable
tab_ncol <- function(tab){
  ncol(get_tablegrob(tab))
}

#' @export
#' @rdname  ggtexttable
tab_nrow <- function(tab){
  nrow(get_tablegrob(tab))
}

#' @export
#' @rdname ggtexttable
tab_add_hline <- function(tab, at.row = 2:tab_nrow(tab), row.side = c("bottom", "top"),
                          from.column = 1, to.column = tab_ncol(tab),
                          linetype = 1, linewidth = 1, linecolor = "black"){
  required_package("gtable")
  row.side <- match.arg(row.side)
  tabgrob <- get_tablegrob(tab)
  separators <- replicate(
    n = length(at.row),
    tab_hline(row.side = row.side, linetype = linetype, linewidth = linewidth, linecolor = linecolor),
    simplify = FALSE
  )
  tabgrob <- gtable::gtable_add_grob(
    tabgrob, grobs = separators,
    t = at.row, b = at.row,
    l = from.column, r = to.column
  )
  tab_return_same_class_as_input(tabgrob, input = tab)
}

# Create hline at the top or the bottom side of a given row
tab_hline <- function(row.side = c("bottom", "top"), linetype = 1, linewidth = 1, linecolor = "black"){
  row.side <- match.arg(row.side)
  y0 <- y1 <- unit(0, "npc")
  if(row.side == "top") y0 <- y1 <- unit(1, "npc")
  grid::segmentsGrob(
    x0 = unit(0, "npc"), x1 = unit(1,"npc"),
    y0 = y0, y1 = y1,
    gp = grid::gpar( lty = linetype, lwd = linewidth, col = linecolor)
  )
}


#' @export
#' @rdname ggtexttable
tab_add_vline <- function(tab, at.column = 2:tab_ncol(tab), col.side = c("left", "right"),
                          from.row = 1, to.row = tab_nrow(tab),
                          linetype = 1, linewidth = 1, linecolor = "black"){
  required_package("gtable")
  col.side <- match.arg(col.side)
  tabgrob <- get_tablegrob(tab)
  separators <- replicate(
    n = length(at.column),
    tab_vline(col.side = col.side, linetype = linetype, linewidth = linewidth, linecolor = linecolor),
    simplify = FALSE
  )
  tabgrob <- gtable::gtable_add_grob(
    tabgrob, grobs = separators,
    t = from.row, b = to.row,
    l = at.column, r = at.column
  )
  tab_return_same_class_as_input(tabgrob, input = tab)
}
# Create vline at the left or the right side of a given column
tab_vline <- function(col.side = c("left", "right"), linetype = 1, linewidth = 1, linecolor = "black"){
  col.side <- match.arg(col.side)
  x0 <- x1 <- unit(0, "npc")
  if(col.side == "right") x0 <- x1 <- unit(1, "npc")
  grid::segmentsGrob(
    x0 = x0, x1 = x1,
    y0 = unit(0, "npc"), y1 = unit(1, "npc"),
    gp = grid::gpar( lty = linetype, lwd = linewidth, col = linecolor)
  )
}

#' @export
#' @rdname ggtexttable
tab_add_border <- function(tab, from.row = 2, to.row = tab_nrow(tab),
                           from.column = 1, to.column = tab_ncol(tab),
                           linetype = 1, linewidth = 1, linecolor = "black"){
  required_package("gtable")
  tabgrob <- get_tablegrob(tab)
  border <- grid::rectGrob(
    gp = grid::gpar(fill = NA, lty = linetype, lwd = linewidth, col = linecolor)
    )
  tabgrob <- gtable::gtable_add_grob(
    tabgrob, grobs = border,
    t = from.row, b = to.row,
    l = from.column, r = to.column
  )
  tab_return_same_class_as_input(tabgrob, input = tab)
}

#' @export
#' @rdname ggtexttable
tbody_add_border <- function(tab, from.row = 2, to.row = tab_nrow(tab),
                             from.column = 1, to.column = tab_ncol(tab),
                             linetype = 1, linewidth = 1, linecolor = "black"){
  tab_add_border(
    tab, from.row = from.row, to.row = to.row,
    from.column = from.column, to.column = to.column,
    linetype = linetype, linewidth = linewidth, linecolor = linecolor
  )
}

#' @export
#' @rdname ggtexttable
thead_add_border <- function(tab, from.row = 1, to.row = 1,
                             from.column = 1, to.column = tab_ncol(tab),
                             linetype = 1, linewidth = 1, linecolor = "black"){
  tab_add_border(
    tab, from.row = from.row, to.row = to.row,
    from.column = from.column, to.column = to.column,
    linetype = linetype, linewidth = linewidth, linecolor = linecolor
  )
}



#::::::::::::::::::::::::::::::::::::::::
# Helper function
#::::::::::::::::::::::::::::::::::::::::

is_ggtexttable <- function(tab){
  !is.null(attr(tab, "ggtexttableGrob"))
}
is_tablegrob <- function(tab){
  inherits(tab, "gtable") & inherits(tab, "grob")
}

# Transform a table grob in ggtexttable like object
as_ggtexttable <- function(tabgrob){
  res <- as_ggplot(tabgrob)
  attr(res, "ggtexttableGrob") <- tabgrob
  res
}

# Extract tableGrob from ggtexttable()
get_tablegrob <- function(tab){
  if(is_ggtexttable(tab)){
    tabgrob <- attr(tab, "ggtexttableGrob")
  }
  else if(is_tablegrob(tab)){
    tabgrob <- tab
  }
  else{
    stop("tab should be an object from either ggpubr::ggtexttable() or gridExtra::tableGrob().")
  }
  tabgrob
}

# Return the same class as the input data,
# which can be either ggtextable or a gridExtra::tableGrob
tab_return_same_class_as_input <- function(tabgrob, input){
  if(is_ggtexttable(input)){
    return(as_ggtexttable(tabgrob))
  }
  else if(is_tablegrob(input)){
    return(tabgrob)
  }
  tabgrob
}



# Define table style
tstyle <- function(pal, size = 12){

  allowed.palettes = c("default", "blank", "classic", "minimal", "light",
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

                  classic = list(
                    colnames.style = colnames_style(fill = NA, linecolor = "black", size = size),
                    rownames.style = rownames_style(fill = NA, linecolor = NA, size = size),
                    tbody.style = tbody_style(fill = NA, linecolor = "black", size = size)
                  ),

                  minimal = list(
                    colnames.style = colnames_style(fill = NA, linecolor = NA, size = size),
                    rownames.style = rownames_style(fill = NA, linecolor = NA, size = size),
                    tbody.style = tbody_style(fill = NA, linecolor = NA, size = size)
                  ),

                  light = list(
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






