# ggpubr 0.1.2.999
   
## New features
   
- New function `facet()` added to create multi-panel plots ([#5](https://github.com/kassambara/ggpubr/issues/5))
     
- New arguments - `facet.by, panel.labs and short.panel.labs`,  added in many ggpubr functions for faceting. These functions include:  
   - ggboxplot()
   - ggdotplot()
   - ggstripchart()
   - ggviolin()
   - ggline()
   
- Now, in some ggpubr functions, the argument `y` can be a character vector of multiple variables to plot at once. This might be useful in genomic fields to plot the gene expression levels of multiple genes at once. see ggboxplot(), ggdotplot(), ggstripchart() and ggviolin().
    
    
- New arguments `combine` added to combine multiple y variables on the same graph
- New argument `merge` to merge multiple y variables in the same ploting area.
- New argument `remove` to remove a specific item from a plot
- New arguments added to show point labels
     
     
- New function `add_summary()` to add summary statistics.
- New function `ggadd()` to Add summary statistics or a geometry onto a ggplot.

- New arguments `grouping.vars`  in `ggtext()`. Grouping variables to sort the data by, when the user wants to display the top n up/down labels.

- New arguments in `theme_pubr()`: 
    - border,
    - margin, 
    - legend,
    - x.text.angle
      
      
## Major changes
   

   
## Minor changes


- Now, the argument `palette` Can be also a numeric vector of length(groups); in this case a basic color palette is created using the function `grDevices::palette()`.
   
# Bug fixes
   
- Now, `ggpar()` reacts to palette when length(palette) = 1 and palette is a color name [#3](https://github.com/kassambara/ggpubr/issues/3).

- `ggmaplot()` now handles situations, wehre there is only upregulated, or downlegulated gnes.
   
## To check
add labels on boxplot:
  label = "name", 
  label.show = c(nam1, name2), 
  label.show = list(top_up = 10, top_down = 10)
  label.font = list(size, color, family)
  
https://cran.r-project.org/web/packages/ggsignif/vignettes/intro.html
https://cran.r-project.org/web/packages/ggpmisc/vignettes/user-guide.html
http://techqa.info/programming/question/37536950/ggplot2:-add-p-values-to-the-plot

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

