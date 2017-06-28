# ggpubr 0.1.4

## New features

- New helper functions:
    - `gradient_color()` and `gradient_color()`: change gradient color and fill palettes.
    - `clean_theme()`: remove axis lines, ticks, texts and titles.
    - `get_legend()`: to extract the legend labels from a ggplot object.
    - `as_ggplot()`: Transform the output of `gridExtra::arrangeGrob()` and `gridExtra::grid.arrange()` to a an object of class ggplot.
    - `ggtexttable()`: to draw a textual table.
    - `ggparagraph()`: to draw a paragraph of text.
    - fill_palette() and color_palette() to change the fill and color palette, respectively.
    - `annotate_figure()` to annotate (arranged) ggplots.
    - `text_grob()` to create easily a customized text graphical object. 
    - `background_image()` to add a background image to a ggplot.
    
- New theme function `theme_transparent()` to create a ggplot with transparent background.
 
## Minor changes

- In `gghistogram()`, density curve and rug react to the fill color.
- `ggarrange()`:
    - New  argument `àlign` to specify whether graphs in the grid should be horizontally ("h") or vertically ("v") aligned. 
    - New argument `legend` to remove or specify the legend position when arranging multiple plots.
    - New argument `common.legend` to create a common unique legend for multiple plots.
     

# ggpubr 0.1.3
   
## New features
   
- New functions:
    - `ggarrange()` to arrange multiple ggplots on the same page.
    - `ggexport()` to export one or multiple ggplots to a file (pdf, eps, png, jpeg).
    - `ggpaired()` to plot paired data.
    - `compare_means()` to compare the means of two or multiple groups. Returns a data frame.
    - `stat_compare_means()` to add p-values and significance levels to plots.
    - `stat_cor()` to add correlation coefficients with p-values to a scatter plot.
    - `stat_stars()` to add stars to a scatter plot.
      
      
      
- Now, the argument `y` can be a character vector of multiple variables to plot at once. This might be useful in genomic fields to plot the gene expression levels of multiple genes at once. see `ggboxplot()`, `ggdotplot()`, `ggstripchart()`, `ggviolin()`, `ggbarplot()` and `ggline`.
   
- The argument `x` can be a vector of multiple variables in `gghistogram()`, `ggdensity()`, `ggecdf()` and `ggqqplot()`.
     
     
- New functions to edit ggplot graphical parameters:
    - `font()` to change the appearance of titles and labels.
    - `rotate_x_text()` and `rotate_y_text()` to rotate x and y axis texts.
    - `rotate()` to rotate a ggplot for creating horizontal plot.
    - `set_palette()` or `change_palette()` to change a ggplot color palette.
    - `border()` to add/change border lines around a ggplot.
    - `bgcolor()` to change ggplot panel background color.
    - `rremove()` to remove a specific component from a ggplot.
    - `grids()` to add grid lines.
    - `xscale()` and `yscale()` to change axis scale.
       
       
- New helper functions:
    - `facet()` added to create multi-panel plots ([#5](https://github.com/kassambara/ggpubr/issues/5)).
    - `add_summary()` to add summary statistics.
    - `ggadd()` to add summary statistics or a geometry onto a ggplot.
      
      
- New data set added: `gene_citation`    
     
     
- New arguments in `ggpar()`: `x.text.angle` and `y.text.angle`

      
      
## Major changes
   
- New arguments in ggpubr functions, see `ggboxplot()`, `ggdotplot()`, `ggstripchart()`, `ggviolin()`, `ggbarplot()` and `ggline`:
    - `combine` added to combine multiple y variables on the same graph.
    - `merge` to merge multiple y variables in the same ploting area.
    - `select` to select which item to display.
    - `remove` to remove a specific item from a plot.
    - `order` to order plot items.
    - `label, font.label, label.select, repel, label.rectangle` to add and customize labels
    - `facet.by, panel.labs and short.panel.labs`: support for faceting and customization of plot panels
        
        
- New argument `grouping.vars`  in `ggtext()`. Grouping variables to sort the data by, when the user wants to display the top n up/down labels.
      
      
- New arguments in `theme_pubr()`: 
    - border,
    - margin, 
    - legend,
    - x.text.angle

   
## Minor changes


- Now, the argument `palette` Can be also a numeric vector of length(groups); in this case a basic color palette is created using the function `grDevices::palette()`.
   
# Bug fixes
   
- Now, `ggpar()` reacts to palette when length(palette) = 1 and palette is a color name [#3](https://github.com/kassambara/ggpubr/issues/3).

- `ggmaplot()` now handles situations, wehre there is only upregulated, or downlegulated gnes.
  

# ggpubr 0.1.2
   
    
## New features
   
- New function `get_palette()` to generate a palette of k colors from ggsci palettes, RColorbrewer palettes and custom color palettes. Useful to extend RColorBrewer and ggsci to support more colors.
  
## Minor changes
   
- Now the `ggpar()` function can handle a list of ggplots.
- Now the default legend position is `right`.
- New argument `show.legend.text` in the `ggscatter()` function. Use show.legend.text = FALSE to hide text in the legend.
- New arguments `title, submain, subtitle, caption, font.submain, font.subtitle, font.caption` in the `ggpar()` function.
- New argument `font.family` in `ggscatter()`.
   
## Bug fixed
   
- The mean within group for `ggdensity` (`gghistogram`) are now shown if data have NA values [@chunkaowang, #1](https://github.com/kassambara/ggpubr/issues/1)
   
   
# ggpubr 0.1.1
   
  
## New features
   
- New function `ggtext()` for textual annotation.
- New argument star.plot in `ggscatter()`. A logical value. If TRUE, a star plot is generated.
- New helper function `geom_exec()`. A helper function used by ggpubr functions to execute any geom_xx functions in ggplot2. Useful only when you want to call a geom_xx function without carrying about the arguments to put in `ggplot2::aes()`.
- New arguments sort.val and top in `ggbarplot()`. 
    - sort.val: a string specifying whether the value should be sorted. Allowed values are "none" (no sorting), "asc" (for ascending) or "desc" (for descending).
    - top: a numeric value specifying the number of top elements to be shown.
- New function `theme_classic2()` added. Classic theme with axis lines.
    
    
## Minor changes

- `ggboxplot()`, `ggviolin()`, `ggdotplot()`, `ggstripchart()`, `gghistogram()`, `ggdensity()`, `ggecdf()` and `ggqqplot()` can now handle one single numeric vector.

```
# Example
ggboxplot(iris$Sepal.Length)
```

- Now, in `gghistogram()`, when add_density = TRUE, y scale remains = "..count..".
- Now, default theme changed to theme_classic2()
- Default point size and line size set to NULL

   

# ggpubr 0.1.0


## Plot one variable - X: Continuous

- ggdensity(): Density plot
- gghistogram(): Histogram plot
- ggecdf(): Empirical cumulative density function
- ggqqplot(): QQ plots


## Plot two variables - X & Y: Discrete X and Continuous Y

- ggboxplot(): Box plot
- ggviolin(): Violin plot
- ggdotplot(): Dot plot
- ggstripchart(): Stripchart (jitter)
- ggbarplot(): Bar plot
- ggline(): Line plot
- ggerrorplot(): Error plot
- ggpie(): Pie chart
- ggdotchart(): Cleveland's dot plots


## Plot two continuous variables

- ggscatter(): Scatter plot
  
  
## Graphical paramters
   
- ggpar(): Change graphical parameters
- show_line_type(): Line types available in R
- show_point_shapes(): Point shapes available in R
- theme_pubr(): Create a publication ready theme
- labs_pubr(): Format only plot labels to a publication ready style
   
   
## Genomics
    
- ggmaplot(): MA-plot from means and log fold changes
   
   
## Data
   
- diff_express: Differential gene expression analysis results
   
   
## Other
   
- desc_statby(): Descriptive statistics by groups
- stat_chull(): Plot convex hull of a set of points
- stat_conf_ellipse(): Plot confidence ellipses
- stat_mean(): Draw group mean points
