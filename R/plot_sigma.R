#' Plot sigma for Incipient, Constant and Maximum cavitation
#'
#' @param b steepness
#' @param d upper value
#' @param e the effective dose
#' @param fls liquid pressure recovery full open (max between fl and Flp/Fp)
#' @param cylindertyp  Name of the control characteristic
#'
#' @return ggplot graphic. of the sigma for Incipient, Constant and Maximum cavitation
#' @export
#'
plot_sigma <- function( b, d, e, fls, cylindertyp){
  x   <- data.frame(x = 1:100)
  ggplot( data = x, mapping = aes(x = x)) +
    stat_function( fun = function(x) {Sigma_mv(x, b, d, e, fls)}, size = 1, aes(colour = "3.-Maximum")) +
    stat_function( fun = function(x) {Sigma_c(x, b, d, e, fls)}, size = 1,  aes(colour = "2.-Constant")) +
    stat_function( fun = function(x) {Sigma_i(x, b, d, e, fls)}, size = 1,  aes(colour = "1.-Incipient")) +
    scale_colour_manual("Borders of the cavitation: ", values = c("green", "orange", "red")) +
    scale_x_continuous( breaks = seq(0, 100, 10)) +
    labs( title    = TeX('Sigma values ($\\sigma$)'),
          subtitle = paste("Flow characteristics :", cylindertyp),
          caption  = "Dr.Trujillo",
          x        = "Opening degree (%)",
          y        = TeX('Sigma values ($\\sigma$)')) +
    theme_bw() + theme(legend.position = "bottom")
}