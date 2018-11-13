
.brewerpal <- function(){
  c(
    # sequential
    'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
    'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds',
    'YlGn', 'YlGnBu YlOrBr', 'YlOrRd',
    #Divergent
    'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral',
    # Qualitative
    'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1', 'Set2', 'Set3'
  )
}
.ggscipal <- function(){
  # Scientific Journal and Sci-Fi Themed Color Palettes for ggplot2
  # ggsci package: https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html
  c("npg", "aaas", "nejm", "lancet", "jama", "jco", "ucscgb", "d3", "locuszoom",
    "igv", "uchicago", "startrek", "tron", "futurama", "rickandmorty", "simpsons")
}

# Check if color palette or default hue
.is_col_palette <- function(pal){
  if(is.null(pal)) return(FALSE)
  else return(length(pal)==1 & pal[1] %in% c(.brewerpal(), .ggscipal(),
                                             "default", "hue", "grey_pal", "gray_pal"))
}
.is_color_palette <- .is_col_palette # alias

# Change color manually
# possible value for palette: brewer palette, "grey" or a vector of colors
.ggcolor <- function(palette = NULL, ...) {
  color_palette(palette = palette, ...)
}

# Change fill color manually
# possible value for palette: brewer palette, "grey" or a vector of colors
.ggfill <- function(palette = NULL, ...) {
 fill_palette(palette = palette, ...)
}



# Helper function to use palette from ggsci package
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.scale_color_ggsci <- function(palette = c("npg", "aaas", "nejm", "lancet", "jama",
					   "jco", "ucscgb", "d3", "locuszoom",
					   "igv", "uchicago", "startrek", "tron",
					   "futurama", "rickandmorty", "simpsons"))
{

  pal <- match.arg(palette)

  functs <- list(
    npg = ggsci::scale_color_npg(),
    aaas = ggsci::scale_color_aaas(),
    nejm = ggsci::scale_color_nejm(),
    lancet = ggsci::scale_color_lancet(),
    jama = ggsci::scale_color_jama(),
    jco = ggsci::scale_color_jco(),
    ucscgb = ggsci::scale_color_ucscgb(),
    d3 = ggsci::scale_color_d3(),
    locuszoom = ggsci::scale_color_locuszoom(),
    igv = ggsci::scale_color_igv(),
    uchicago = ggsci::scale_color_uchicago(),
    startrek = ggsci::scale_color_startrek(),
    tron = ggsci::scale_color_tron(),
    futurama = ggsci::scale_color_futurama(),
    rickandmorty = ggsci::scale_color_rickandmorty(),
    simpsons = ggsci::scale_color_simpsons()
  )
  functs[[pal]]
}

.scale_fill_ggsci <- function(palette = c("npg", "aaas", "nejm", "lancet", "jama",
					  "jco", "ucscgb", "d3", "locuszoom",
					  "igv", "uchicago", "startrek", "tron",
					  "futurama", "rickandmorty", "simpsons"))
{

  pal <- match.arg(palette)

  functs <- list(
    npg = ggsci::scale_fill_npg(),
    aaas = ggsci::scale_fill_aaas(),
    nejm = ggsci::scale_fill_nejm(),
    lancet = ggsci::scale_fill_lancet(),
    jama = ggsci::scale_fill_jama(),
    jco = ggsci::scale_fill_jco(),
    ucscgb = ggsci::scale_fill_ucscgb(),
    d3 = ggsci::scale_fill_d3(),
    locuszoom = ggsci::scale_fill_locuszoom(),
    igv = ggsci::scale_fill_igv(),
    uchicago = ggsci::scale_fill_uchicago(),
    startrek = ggsci::scale_fill_startrek(),
    tron = ggsci::scale_fill_tron(),
    futurama = ggsci::scale_fill_futurama(),
    rickandmorty = ggsci::scale_fill_rickandmorty(),
    simpsons = ggsci::scale_fill_simpsons()
  )
  functs[[pal]]
}

# Generate color palette from ggsci or Rcolorbrewer
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# pal could be a brewer or ggsci palette
.get_pal <- function(pal = "default", k){
  if(pal %in% .brewerpal()) .get_brewer_pal(pal, k)
  else if(pal %in% .ggscipal()) .get_ggsci_pal(pal, k)
  else if(pal %in% c("default", "hue")){
    hues <- seq(15, 375, length = k + 1)
    grDevices::hcl(h = hues, l = 65, c = 100, alpha = 1)[1:k]
  }
}
.get_palette <- .get_pal # alias

# Generate color palette from ggsci
# k the number of color
.get_ggsci_pal <- function(palette = c("npg", "aaas", "nejm", "lancet", "jama",
				       "jco", "ucscgb", "d3", "locuszoom",
				       "igv", "uchicago", "startrek", "tron",
				       "futurama", "rickandmorty", "simpsons"), k)
{

  pal <- match.arg(palette)

  if(pal %in% c("npg", "aaas", "jco", "d3")) max_k <- 10
  else if (pal %in% c("nejm")) max_k <- 8
  else if (pal %in% c("jama", "locuszoom", "startrek", "tron")) max_k <- 7
  else if (pal %in% c("igv")) max_k <- 51
  else if (pal %in% c("lancet", "uchicago")) max_k <- 9
  else if (pal %in% c("ucscgb")) max_k <- 26
  else if (pal %in% c("futurama", "rickandmorty")) max_k <- 12
  else if (pal %in% c("simpsons")) max_k <- 16
  else stop("Don't support palette name: ", pal)

  functs <- list(
    npg = ggsci::pal_npg(),
    aaas = ggsci::pal_aaas(),
    nejm = ggsci::pal_nejm(),
    lancet = ggsci::pal_lancet(),
    jama = ggsci::pal_jama(),
    jco = ggsci::pal_jco(),
    ucscgb = ggsci::pal_ucscgb(),
    d3 = ggsci::pal_d3(),
    locuszoom = ggsci::pal_locuszoom(),
    igv = ggsci::pal_igv(),
    uchicago = ggsci::pal_uchicago(),
    startrek = ggsci::pal_startrek(),
    tron = ggsci::pal_tron(),
    futurama = ggsci::pal_futurama(),
    rickandmorty = ggsci::pal_rickandmorty(),
    simpsons = ggsci::pal_simpsons()
  )

  if(k <= max_k) functs[[pal]](k)
  else grDevices::colorRampPalette(functs[[pal]](max_k))(k)
}

# Generate a color palette from brewer
.get_brewer_pal <- function(palette, k){
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("RColorBrewer package needed. Please install it using install.packages('RColorBrewer').")
  }
  initial.k <- k
  k <- max(c(k, 3)) # Kshoud be at least 3
  pal <- palette[1]
  sequential <- c('Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
                  'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds','YlGn', 'YlGnBu YlOrBr', 'YlOrRd')
  divergent <- c('BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu', 'RdYlGn', 'Spectral')

  if(pal %in% sequential) max_k <- 9
  else if(pal %in% divergent) max_k <- 11
  else if(pal %in% c('Accent','Dark2','Pastel2', 'Set2')) max_k <- 8
  else if(pal %in% c('Pastel1',  'Set1')) max_k <- 9
  else if(pal %in% c('Paired', 'Set3')) max_k <- 12
  else stop("Don't support palette name: ", pal)

  if(k <= max_k) {
    cols <- RColorBrewer::brewer.pal(k, palette)
    if(initial.k == 2) cols <- cols[c(1,3)]
    else if(initial.k == 1) cols <- cols[1]
    cols
  }
  else grDevices::colorRampPalette(RColorBrewer::brewer.pal(max_k, palette))(k)
}

# Set gradient colors
# cols a vector of colors
.gradient_col <- function (cols){
  gradient_color(cols)
}
