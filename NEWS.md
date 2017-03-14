
# ggpubr 0.1.2
   
    
## New features
   
- New function `get_palette()` to generate a palette of k colors from ggsci palettes, RColorbrewer palettes and custom color palettes. Useful to extend RColorBrewer and ggsci to support more colors.
  
## Minor changes
   
- Now the `ggpar()` function can handle a list of ggplots.
- Now the default legend position is `right`.
- New argument `show.legend.text` in the `ggscatter()` function. Use show.legend.text = FALSE to hide text in the legend.
- New arguments `title, submain, subtitle, caption, font.submain, font.subtitle, font.caption` in the `ggpar()` function.
   
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

