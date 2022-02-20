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
plotly_zv <- function(b, d, e, dn, zvs, cylindertyp){
  
  data.zeta <- tibble(position = 10:100)%>%
    mutate(zeta = map_dbl(.x = .data$position, ~zv_function( .x, b, d, e, zvs)))
  
  h.template <- paste( "Loss Coeficiente: %{y:.1f}<br>",
                       "%Valve Position: %{x:.1f}%",
                       "<extra></extra>")
  
  p <-  plot_ly() %>%
    add_trace( data = data.zeta, x = ~position, y = ~zeta,
               type = "scatter", mode = "lines",
               hovertemplate = h.template) %>%
    layout( title = paste("Zeta Value for ", cylindertyp),
            xaxis = list(title = plotly::TeX("\\text{Valve Position } (\\%)"), dtick = 10),
            yaxis = list(title = plotly::TeX("\\text{Zeta Value } (\\zeta)"), type = "log", tickformat = "0.1e"),
            showlegend = FALSE) %>%
    config(displayModeBar = FALSE, mathjax = 'cdn')
  
  return(p)
}
