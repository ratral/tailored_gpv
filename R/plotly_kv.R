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
plotly_kv <- function( b, d, e, dn, zvs, cylindertyp){
  
  kvs   <- kv_value(dn, zvs)
  y.max <- ceiling(kvs/50)*50 # Round kvs up to the nearest 50
  
  data.kv <- tibble(position = 0:100) %>%
    mutate(kv = map_dbl( .x = .data$position, ~drm_LL3( .x, b, d, e)*kvs))
  
  h.template <- paste( "Flow Coefficient: %{y:,.1f} m³/h<br>",
                       "Valve Position: %{x:.1f}%",
                       "<extra></extra>")

  p <-  plot_ly() %>%
    add_trace( data = data.kv, x = ~position, y = ~kv,
               type = "scatter", mode = "lines",
               hovertemplate = h.template)  %>%
    layout( title = list(text = paste0('Flow Coefficient for ', cylindertyp,
                                       '<br>',
                                       '<sup>',
                                       'pressure drop (ΔP) across the valve with the flow rate (Q)',
                                       '</sup>')),
            xaxis = list( title = TeX("\\text{Valve Position } (\\%)"), dtick = 10),
            yaxis = list( title = TeX("K_v \\; [m^3/h][bar]")),
            showlegend = FALSE) %>%
    config(displayModeBar = FALSE, mathjax = 'cdn')
  
  return(p)
}