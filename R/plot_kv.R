#' Plot Flow coefficient Kv Value
#'
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param dn dn diameter in meter (m)
#' @param zvs Resistance Coefficient Zeta full open
#' @param cylindertyp Name of the control characteristic
#'
#'
#' @import ggplot2
#' @import latex2exp
#'
#' @return ggplot graphic. Flow coefficient Kv Value
#'
#' @export
#'
plot_kv <- function(b, d, e, dn, zvs, cylindertyp){
  x   <- data.frame(x = 0:100)
  kvs <- kv_value(dn, zvs)/3.6
  y.max <- ceiling(kvs/50)*50 # Round kvs up to the nearest 50
  ggplot( data = x, mapping = aes(x = x)) +
    stat_function( fun = function(x) {drm_LL3( x, b, d, e)*kvs},
                   size = 1, color = "blue") +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_y_continuous(breaks = seq(0, y.max, 50)) +
    labs( title    = TeX("Flow Coefficient $k_{v}$ in (Liter/second)"),
          # subtitle = paste("Flow characteristics: ", cylindertyp),
          # caption  = "Dr.Trujillo",
          x        = "Opening degree (%)",
          y        = TeX('$k_{v} \\; (lps)$')) +
    theme_bw()
}

