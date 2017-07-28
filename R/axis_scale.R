#'Change Axis Scale: log2, log10 and more
#'
#'@description Change axis scale.
#'\itemize{
#'\item \code{xscale}: change x axis scale.
#'\item \code{yscale}: change y axis scale.
#'}
#'@param .scale axis scale. Allowed values are one of c("none", "log2", "log10",
#'  "sqrt", "percent", "dollar", "scientific"); e.g.: .scale="log2".
#'@param .format ogical value. If TRUE, axis tick mark labels will be formatted
#'  when .scale  = "log2" or "log10".
#'
#'
#'@examples
#'# Basic scatter plots
#'data(cars)
#'p <- ggscatter(cars, x = "speed", y = "dist")
#'p
#'
#'# Set log scale
#'p + yscale("log2", .format = TRUE)

#'@name axis_scale
#'@rdname axis_scale
#'@export
xscale <- function(.scale, .format = FALSE)
{

  .x <- NULL

  if(.format & .scale %in% c("percent", "dollar", "scientific"))
    .format <- FALSE


  if(.format){

    if(.scale == "log2"){
      scale_x_continuous(trans = scales::log2_trans(),
                         breaks = scales::trans_breaks("log2", function(x) 2^x),
                         labels = scales::trans_format("log2", scales::math_format(2^.x)))
    }
    else if(.scale == "log10"){
      scale_x_continuous(trans = scales::log10_trans(),
                         breaks = scales::trans_breaks("log10", function(x) 10^x),
                         labels = scales::trans_format("log10", scales::math_format(10^.x)))
    }

  }

  else if(.scale %in% c("log2", "log10")){
    scale_x_continuous(trans = .scale)
  }

  else{
    switch(.scale,
           percent = scale_x_continuous(labels = scales::percent),
           dollar = scale_x_continuous(labels = scales::dollar),
           scientific = scale_x_continuous(labels = scales::scientific)
           )

  }

}

#'@rdname axis_scale
#'@export
yscale <- function(.scale, .format = FALSE)
{

  .x <- NULL

  if(.format & .scale %in% c("percent", "dollar", "scientific"))
    .format <- FALSE

  if(.format){

    if(.scale == "log2"){
      scale_y_continuous(trans = scales::log2_trans(),
                         breaks = scales::trans_breaks("log2", function(x) 2^x),
                         labels = scales::trans_format("log2", scales::math_format(2^.x)))
    }
    else if(.scale == "log10"){
      scale_y_continuous(trans = scales::log10_trans(),
                         breaks = scales::trans_breaks("log10", function(x) 10^x),
                         labels = scales::trans_format("log10", scales::math_format(10^.x)))
    }

  }
  else if(.scale %in% c("log2", "log10")){
    scale_y_continuous(trans = .scale)
  }

  else{
    switch(.scale,
           percent = scale_y_continuous(labels = scales::percent),
           dollar = scale_y_continuous(labels = scales::dollar),
           scientific = scale_y_continuous(labels = scales::scientific)
    )

  }
}
