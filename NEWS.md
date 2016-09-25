
# ggpubr 0.1.0.999
   
  
## New features
   
- New function `theme_classic2()` added. Classic theme with axis lines.


## Minor changes

- `ggboxplot()`, `ggviolin()`, `ggdotplot()`, `ggstripchart()`, `gghistogram()`, `ggdensity()`, `ggecdf()` and `ggqqplot()` can now handle one single numeric vector.

```
# Example
ggboxplot(iris$Sepal.Length)
```

- Now, in `gghistogram()`, when add_density = TRUE, y scale remains = "..count..".
- Now, default theme changed to theme_classic2()

   
## Bug fixes
    

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

