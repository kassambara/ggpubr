% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_central_tendency.R
\name{stat_central_tendency}
\alias{stat_central_tendency}
\title{Add Central Tendency Measures to a GGPLot}
\usage{
stat_central_tendency(
  mapping = NULL,
  data = NULL,
  geom = c("line", "point"),
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  type = c("mean", "median", "mode"),
  ...
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

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

\item{geom}{The geometric object to use to display the data, either as a
\code{ggproto} \code{Geom} subclass or as a string naming the geom stripped of the
\code{geom_} prefix (e.g. \code{"point"} rather than \code{"geom_point"})}

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{na.rm}{If FALSE (the default), removes missing values with a warning.
If TRUE silently removes missing values.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}

\item{type}{the type of central tendency measure to be used. Possible values
include: \code{"mean", "median", "mode"}.}

\item{...}{other arguments to pass to \code{\link[ggplot2:geom_path]{geom_line}}.}
}
\description{
Add central tendency measures (mean, median, mode) to density
  and histogram plots created using ggplots.

  Note that, normally, the mode is used for categorical data where we wish to
  know which is the most common category. Therefore, we can have have two or
  more values that share the highest frequency. This might be problematic for
  continuous variable.

  For continuous variable, we can consider using mean or median as the
  measures of the central tendency.
}
\examples{
# Simple density plot
data("mtcars")
ggdensity(mtcars, x = "mpg", fill = "red") +
  scale_x_continuous(limits = c(-1, 50)) +
  stat_central_tendency(type = "mean", linetype = "dashed")

# Color by groups
data(iris)
ggdensity(iris, "Sepal.Length", color = "Species") +
  stat_central_tendency(aes(color = Species), type = "median", linetype = 2)

# Use geom = "point" for central tendency
data(iris)
ggdensity(iris, "Sepal.Length", color = "Species") +
  stat_central_tendency(
     aes(color = Species), type = "median",
     geom = "point", size = 4
     )

# Facet
ggdensity(iris, "Sepal.Length", facet.by = "Species") +
  stat_central_tendency(type = "mean", color = "red", linetype = 2) +
  stat_central_tendency(type = "median", color = "blue", linetype = 2)

}
\seealso{
\code{\link{ggdensity}}
}
