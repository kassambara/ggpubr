% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_anova_test.R
\name{stat_anova_test}
\alias{stat_anova_test}
\title{Add Anova Test P-values to a GGPlot}
\usage{
stat_anova_test(
  mapping = NULL,
  data = NULL,
  between = NULL,
  within = NULL,
  type = NULL,
  effect.size = "ges",
  error = NULL,
  correction = c("auto", "GG", "HF", "none"),
  label = "{method}, p = {p.format}",
  label.x.npc = "left",
  label.y.npc = "top",
  label.x = NULL,
  label.y = NULL,
  step.increase = 0.1,
  p.adjust.method = "holm",
  symnum.args = list(),
  geom = "text",
  position = "identity",
  na.rm = FALSE,
  show.legend = FALSE,
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

\item{between}{(optional) between-subject factor variables. Can be specified
for \strong{independent measures test}. Possible values include: \itemize{
\item \code{"x"}: default for comparing the x-axis variable groups. \item
\code{"group"}: for grouped plots. Can be used to compare legend variable
groups at each x-position. \item \code{c("x", "group")}: for grouped plots.
Can be used to compute the significance of a two-way interaction model. }}

\item{within}{(optional) within-subject factor variables. Can be specified for
\strong{repeated-measures test}. Possible values include: \itemize{ \item
\code{"x"}: default for comparing the x-axis variable groups. \item
\code{"group"}: for grouped plots. Can be used to compare legend variable
groups at each x-position. \item \code{c("x", "group")}: for grouped plots.
Can be used to compute the significance of a two-way interaction model. }}

\item{type}{the type of sums of squares for ANOVA. Allowed values are either
1, 2 or 3. \code{type = 2} is the default because this will yield identical
ANOVA results as type = 1 when data are balanced but type = 2 will
additionally yield various assumption tests where appropriate. When the data
are unbalanced the \code{type = 3} is used by popular commercial softwares
including SPSS.}

\item{effect.size}{the effect size to compute and to show in the ANOVA
results. Allowed values can be either "ges" (generalized eta squared) or
"pes" (partial eta squared) or both. Default is "ges".}

\item{error}{(optional) for a linear model, an lm model object from which the
overall error sum of squares and degrees of freedom are to be calculated.
Read more in \code{\link[car]{Anova}()} documentation.}

\item{correction}{character. Used only in repeated measures ANOVA test to
specify which correction of the degrees of freedom should be reported for
the within-subject factors. Possible values are: \itemize{ \item{"GG"}:
applies Greenhouse-Geisser correction to all within-subjects factors even if
the assumption of sphericity is met (i.e., Mauchly's test is not
significant, p > 0.05). \item{"HF"}: applies Hyunh-Feldt correction to all
within-subjects factors even if the assumption of sphericity is met,
\item{"none"}: returns the ANOVA table without any correction and
\item{"auto"}: apply automatically GG correction to only within-subjects
factors violating the sphericity assumption (i.e., Mauchly's test p-value is
significant, p <= 0.05). }}

\item{label}{character string specifying label. Can be: \itemize{ \item the
column containing the label (e.g.: \code{label = "p"} or \code{label =
"p.adj"}), where \code{p} is the p-value. Other possible values are
\code{"p.signif", "p.adj.signif", "p.format", "p.adj.format"}. \item an
expression that can be formatted by the \code{\link[glue]{glue}()} package.
For example, when specifying \code{label = "Anova, p = \{p\}"}, the
expression \{p\} will be replaced by its value. \item a combination of
plotmath expressions and glue expressions. You may want some of the
statistical parameter in italic; for example:\code{label = "Anova, italic(p)
= {p}"}. \item a constant: \code{label = "as_italic"}: display statistical
parameters in italic; \code{label = "as_detailed"}: detailed plain text;
\code{label = "as_detailed_expression"} or \code{label =
"as_detailed_italic"}: detailed plotmath expression. Statistical parameters
will be displayed in italic.}.}

\item{label.x.npc, label.y.npc}{can be \code{numeric} or \code{character}
vector of the same length as the number of groups and/or panels. If too
short they will be recycled. \itemize{ \item If \code{numeric}, value should
be between 0 and 1. Coordinates to be used for positioning the label,
expressed in "normalized parent coordinates". \item If \code{character},
allowed values include: i) one of c('right', 'left', 'center', 'centre',
'middle') for x-axis; ii) and one of c( 'bottom', 'top', 'center', 'centre',
'middle') for y-axis.}}

\item{label.x, label.y}{\code{numeric} Coordinates (in data units) to be used
for absolute positioning of the label. If too short they will be recycled.}

\item{step.increase}{numeric value in with the increase in fraction of total
height for every additional comparison to minimize overlap. The step value
can be negative to reverse the order of groups.}

\item{p.adjust.method}{method for adjusting p values (see
\code{\link[stats]{p.adjust}}).  Has impact only in a situation, where
multiple pairwise tests are performed; or when there are multiple grouping
variables. Allowed values include "holm", "hochberg",
"hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't want to
adjust the p value (not recommended), use p.adjust.method = "none".}

\item{symnum.args}{a list of arguments to pass to the function
 \code{\link[stats]{symnum}} for symbolic number coding of p-values. For
 example, \code{symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01,
 0.05, Inf), symbols = c("****", "***", "**", "*",  "ns"))}.

 In other words, we use the following convention for symbols indicating
 statistical significance: \itemize{ \item \code{ns}: p > 0.05 \item
 \code{*}: p <= 0.05 \item \code{**}: p <= 0.01 \item \code{***}: p <= 0.001
 \item \code{****}:  p <= 0.0001 }}

\item{geom}{The geometric object to use display the data}

\item{position}{Position adjustment, either as a string, or the result of
a call to a position adjustment function.}

\item{na.rm}{If FALSE (the default), removes missing values with a warning. If
TRUE silently removes missing values.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{parse}{If TRUE, the labels will be parsed into expressions and displayed
as described in \code{?plotmath}.}

\item{...}{other arguments to pass to
\code{\link[ggplot2:geom_text]{geom_text}}, such as:\itemize{ \item
\code{hjust}: horizontal justification of the text. Move the text left or
right and \item \code{vjust}: vertical justification of the text. Move the
text up or down. }}
}
\description{
Add one-way and two-way ANOVA test p-values to a ggplot, such as
 box blots, dot plots and stripcharts.
}