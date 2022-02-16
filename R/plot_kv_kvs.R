#' Plot the Valve flow coefficient
#'
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param cylindertyp Name of the control type
#'
#' @import ggplot2
#' @import latex2exp
#'
#' @return ggplot graphic. Valve flow coefficient
#' @export
#'
plot_kv_kvs <- function( b, d, e, cylindertyp ){
  x <- data.frame(x = 0:100)
  ggplot( data = x, mapping = aes(x = x)) +
    stat_function( fun = function(x) {x}, aes(), size = 0.5,
                   color = "black", linetype = 2) +
    stat_function( fun = function(x) {drm_LL3( x, b, d, e)*100},
                   size = 1, color = "blue") +
    scale_x_continuous( breaks = seq(0, 100, 10)) +
    scale_y_continuous( breaks = seq(0, 100, 10)) +
    labs( title    = TeX("Flow Characteristics $(k_{v}/k_{vs})$"),
          subtitle = paste("Flow characteristics: ", cylindertyp),
          caption  = "Dr.Trujillo",
          x        = "Opening degree (%)",
          y        = TeX('$k_{v}/k_{vs}$')) +
    theme_bw()
}