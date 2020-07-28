% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_pwc.R
\name{stat_pwc}
\alias{stat_pwc}
\alias{geom_pwc}
\title{Add Pairwise Comparisons P-values to a GGPlot}
\usage{
stat_pwc(
  mapping = NULL,
  data = NULL,
  method = "wilcox_test",
  method.args = list(),
  label = "{method}, p = {p.format}",
  y.position = NULL,
  group.by = NULL,
  dodge = 0.8,
  bracket.nudge.y = 0.05,
  bracket.shorten = 0,
  bracket.group.by = c("x.var", "legend.var"),
  step.increase = 0.12,
  tip.length = 0.03,
  size = 0.3,
  label.size = 3.88,
  family = "",
  vjust = 0,
  hjust = 0.5,
  p.adjust.method = "holm",
  p.adjust.by = c("group", "panel"),
  symnum.args = list(),
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  parse = FALSE,
  ...
)

geom_pwc(
  mapping = NULL,
  data = NULL,
  stat = "pwc",
  method = "wilcox_test",
  method.args = list(),
  label = "{method}, p = {p.format}",
  y.position = NULL,
  group.by = NULL,
  dodge = 0.8,
  step.increase = 0.12,
  tip.length = 0.03,
  bracket.nudge.y = 0.05,
  bracket.shorten = 0,
  bracket.group.by = c("x.var", "legend.var"),
  size = 0.3,
  label.size = 3.88,
  family = "",
  vjust = 0,
  hjust = 0.5,
  coord.flip = FALSE,
  p.adjust.method = "holm",
  p.adjust.by = c("group", "panel"),
  symnum.args = list(),
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  parse = FALSE,
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}} or
\code{\link[ggplot2:aes_]{aes_()}}. If specified and \code{inherit.aes = TRUE} (the
default), it is combined with the default mapping at the top level of the
plot. You must supply \code{mapping} if there is no plot mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{method}{a character string indicating which method to be used for
pairwise comparisons. Default is \code{"wilcox_test"}. Allowed methods
include pairwise comparisons methods implemented in the \code{rstatix} R
package. These methods are: \code{"wilcox_test", "wilcoxon", "t_test",
"sign_test", "dunn_test", "emmeans_test", "emmeans", "tukey_hsd",
"games_howell_test" and "games_howell"}.}

\item{method.args}{a list of additional arguments used for the test method.
For example one might use \code{method.args = list(alternative = "greater")}
for wilcoxon test.}

\item{label}{character string specifying label. Can be: \itemize{ \item the
column containing the label (e.g.: \code{label = "p"} or \code{label =
"p.adj"}), where \code{p} is the p-value. Other possible values are
\code{"p.signif", "p.adj.signif", "p.format", "p.adj.format"}. \item an
expression that can be formatted by the \code{\link[glue]{glue}()} package.
For example, when specifying \code{label = "Wilcoxon, p = \{p\}"}, the
expression \{p\} will be replaced by its value. \item a combination of
plotmath expressions and glue expressions. You may want some of the
statistical parameter in italic; for example:\code{label = "Wilcoxon,
italic(p)= {p}"}}.}

\item{y.position}{numeric vector with the y positions of the brackets}

\item{group.by}{(optional) character vector specifying the grouping variable;
it should be used only for grouped plots. Possible values are : \itemize{
\item \code{"x.var"}: Group by the x-axis variable and perform the test
between legend groups. In other words, the p-value is compute between legend
groups at each x position \item \code{"legend.var"}: Group by the legend
variable and perform the test between x-axis groups. In other words, the
test is performed between the x-groups for each legend level. }}

\item{dodge}{dodge width for grouped ggplot/test. Default is 0.8. It's used to
dodge the brackets position when \code{group.by = "legend.var"}.}

\item{bracket.nudge.y}{Vertical adjustment to nudge brackets by. Useful to
move up or move down the bracket. If positive value, brackets will be moved
up; if negative value, brackets are moved down.}

\item{bracket.shorten}{a small numeric value in [0-1] for shortening the width
of bracket.}

\item{bracket.group.by}{(optional); a variable name for grouping brackets
before adding step.increase. Useful for grouped plots. Possible values
include \code{"x.var"} and \code{"legend.var"}.}

\item{step.increase}{numeric vector with the increase in fraction of total
height for every additional comparison to minimize overlap.}

\item{tip.length}{numeric vector with the fraction of total height that the
bar goes down to indicate the precise column/}

\item{size}{change the width of the lines of the bracket}

\item{label.size}{change the size of the label text}

\item{family}{change the font used for the text}

\item{vjust}{move the text up or down relative to the bracket.}

\item{hjust}{move the text left or right relative to the bracket.}

\item{p.adjust.method}{method for adjusting p values (see
\code{\link[stats]{p.adjust}}).  Has impact only in a situation, where
multiple pairwise tests are performed; or when there are multiple grouping
variables. Allowed values include "holm", "hochberg", "hommel",
"bonferroni", "BH", "BY", "fdr", "none". If you don't want to adjust the p
value (not recommended), use p.adjust.method = "none".}

\item{p.adjust.by}{possible value is one of \code{c("group", "panel")}.
Default is \code{"group"}: for a grouped data, if pairwise test is
performed, then the p-values are adjusted for each group level
independently. P-values are adjusted by panel when \code{p.adjust.by = "panel"}.}

\item{symnum.args}{a list of arguments to pass to the function
 \code{\link[stats]{symnum}} for symbolic number coding of p-values. For
 example, \code{symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01,
 0.05, Inf), symbols = c("****", "***", "**", "*",  "ns"))}.

 In other words, we use the following convention for symbols indicating
 statistical significance: \itemize{ \item \code{ns}: p > 0.05 \item
 \code{*}: p <= 0.05 \item \code{**}: p <= 0.01 \item \code{***}: p <= 0.001
 \item \code{****}:  p <= 0.0001 }}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

\item{na.rm}{If \code{FALSE} (the default), removes missing values with a
warning.  If \code{TRUE} silently removes missing values.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{parse}{logical for parsing plotmath expression.}

\item{...}{other arguments passed on to \code{\link{layer}}. These are often
aesthetics, used to set an aesthetic to a fixed value, like \code{color =
"red"} or \code{size = 3}. They may also be parameters to the paired
geom/stat.}

\item{stat}{The statistical transformation to use on the data for this
layer, as a string.}

\item{coord.flip}{logical. If \code{TRUE}, flip x and y coordinates so that
horizontal becomes vertical, and vertical, horizontal. When adding the
p-values to a horizontal ggplot (generated using
\code{\link[ggplot2]{coord_flip}()}), you need to specify the option
\code{coord.flip = TRUE}.}
}
\description{
add pairwise comparison p-values to a ggplot such as box plots,
 dot plots and stripcharts.
}
\examples{
df <- ToothGrowth
df$dose <- factor(df$dose)

}