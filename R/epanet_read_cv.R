#' Read Control Valve results
#' 
#' Read the operation data for the Control Valve
#' The data is extracted from the output report produced from a simulation.
#' 
#' @param hmodel S3 object with entries for sections of the .inp file. 
#' Sections of the .inp file that are implemented appear in the Value section.
#' 
#' @param hreportobject (list) with a data.frame for node results and data.
#' frame for link results and a data.frame for energy usage. 
#' The node and link results data frames contain results from all the 
#' time periods to facilitate time series plots.
#' 
#' @import dplyr
#' @import tidyr
#'
#' @return Returns a table (data.frame) with the columns:
#' 
#'  \item{Timestamp}
#'  \item{ID} Control Valve ID
#'  \item{Diameter} in millimeter
#'  \item{Type} 
#'  \item{MinorLoss} unit-less
#'  \item{p1} in meter
#'  \item{p2} in meter
#'  \item{Headloss} in meter
#'  \item{Flow} in LPS (liter/sec)
#'  \item{Velocity} in m/s
#'  
#' @export
#'

epanet_read_cv <- function(hmodel, hreport, cv_name) {

  # Read  from the hmodel the parameter of the control valve to evaluate.
  cvalve <- hmodel$Valves |> filter(ID == cv_name) |> 
    select(ID, Node1, Node2, Diameter, Type, MinorLoss)
  
  # Values for the Nodes of the Valve
  nodes <- hreport$nodeResults |> 
    filter(ID %in% c(cvalve$Node1[1], cvalve$Node2[1])) |> 
    select(ID, Pressure, Timestamp) |> 
      pivot_wider(names_from = ID, values_from = Pressure)  
  
  names(nodes) <- c("Timestamp", "p1", "p2")
  
  # Values for the Valve (Link))
  links <- as_tibble( hreport$linkResults) |>  
    filter(ID == cv_name) |> 
    select(Timestamp, Headloss, Flow, Velocity)
  
  operation_data <- nodes |> 
    full_join(links, by ="Timestamp") |> 
    mutate(
      ID = cvalve$ID,
      Diameter = cvalve$Diameter,
      Type = cvalve$Type,
      MinorLoss = cvalve$MinorLoss
    ) |> 
    select(Timestamp, 
           ID, Diameter, Type, MinorLoss, 
           p1, p2, Headloss, Flow, Velocity)
  
  return(operation_data)
  
}