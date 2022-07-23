#' Plot Zeta Value Curve
#'
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param zvs Resistance Coefficient Zeta full open
#' @param cylindertyp Name of the control characteristic
#'
#' @import ggplot2
#' @import latex2exp
#' @import scales
#'
#' @return ggplot graphic. Zeta Value Curve
#' @export
#'
plot_zv <- function(b, d, e, zvs, cylindertyp){
  x   <- data.frame(x = 0:100)
  ggplot( data = x, mapping = aes(x = x)) +
    stat_function( fun = function(x) {zv_function( x, b, d, e, zvs )},
                   size = 1, color = "green") +
    scale_y_log10() +
    scale_x_continuous( breaks = seq(0, 100, 10)) +
    annotation_logticks(sides = "lr") +
    labs( title    = latex2exp::TeX('Zeta Value Curve $\\zeta_{v}$'),
          # subtitle = paste("Flow characteristics :", cylindertyp),
          # caption  = "Dr.Trujillo",
          x        = "Opening degree (%)",
          y        = latex2exp::TeX('$\\zeta_{v}$')) +
    theme_bw()
}