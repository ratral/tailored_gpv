#' Plot liquid pressure recovery factor
#'
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param fls liquid pressure recovery full open (max between fl and Flp/Fp)
#' @param cylindertyp  Name of the control characteristic
#'
#' @return ggplot graphic. of the liquid pressure recovery factor
#' @export
#'
plot_fl <- function( b, d, e, fls, cylindertyp){
  x   <- data.frame(x = 1:100)
  ggplot( data = x, mapping = aes(x = x)) +
    stat_function( fun = function(x) {fl_function( x, b, d, e, fls)},
                   size = 1, color = "black") +
    scale_x_continuous( breaks = seq(0, 100, 10)) +
    scale_y_continuous( breaks = seq(0, 1, 0.05)) +
    labs( title    = TeX('Liquid Pressure Recovery Factor ($F_{L}$, or
                            respectively $F_{LP} / F_{P}$)'),
          subtitle = paste("Flow characteristics :", cylindertyp),
          caption  = "Dr.Trujillo",
          x        = "Opening degree (%)",
          y        = TeX('$F_{L}$')) +
    theme_bw()
}


