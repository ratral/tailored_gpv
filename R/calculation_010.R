
# Parmeter

# params <- list(
#   file.hmodel = "exercise_01_Res2Res",
#   valve.brand = "AVK-ACMO",
#   valve.cyl   = "PSEG",
#   valve.id    = "RIKO" # Name in the epanet .inp file
# )

# Libraries----

# library(tidyverse)
# library(here)
# library(glue)
# library(hyd4gpv)
# library(epanet2toolkit)
# library(epanetReader)

  
# Read Epanet model file; name in "params$file.hmodel"
file.hmodel <- read.inp(here::here("epanet",
                                   paste0(params$file.hmodel,".inp")))

# Read  from the epanet file the parameter of the control valve to evaluate.
# The name of the control valve is in "params$valve.id"
valve <- file.hmodel$Valves %>% 
  filter(ID == params$valve.id)

# Read Diameter of the valve.
params$valve.dn <- valve$Diameter[1]

# Read the nodes of the Control valve to analyze
params$valve.nodes <- c(valve[1,2], valve[1,3])

# Remove control.valve
rm(valve)

# Filter the control valves parameters table according to the brand, 
# cylinder type, and diameter..
params$valve.params <- hyd4gpv:::gpv_data %>% 
  filter(brand == params$valve.brand & 
         cyl_name == params$valve.cyl) %>% 
  filter(dn_min <= params$valve.dn &
         params$valve.dn <= dn_max)

# This part has the purpose of defining the controls that modify the 
# loss coefficient of the control valve (link TCV throttle control valve) 
# for the different opening positions of the valve (see file.hmodel$Times down).
zv4tcv <- tibble(position = seq(5, 100, by = 5)) %>%
  mutate(zv = zv_function(position, 
                          params$valve.params$kv_b, 
                          params$valve.params$kv_d, 
                          params$valve.params$kv_e, 
                          params$valve.params$zvs)) %>%
  select(position, zv) %>%
  mutate(zv = round(zv,2)) %>% arrange(zv) %>%
  mutate(time = seq(1, length(zv), 1)) %>%
  mutate(controls = glue('LINK {params$valve.id} {zv} AT TIME {time}; {position}% opening')) %>%
  pull(controls)

# 1. Add the open position for the control valve.
#    This represent Without loses/valve at the control valve position
# 2. Change the information in the epanet file.
file.hmodel$Controls <- c(
  glue('LINK {params$valve.id} OPEN AT TIME 0; without Valve'), 
  zv4tcv
)

# Remove zv4tcv
rm(zv4tcv)

# Trust is good, but control is better (see controls definitions)
# length of Controls (zv4tcv)  must be the same of the duration Time:
file.hmodel$Times <- c(
  glue("Duration {length(file.hmodel$Controls)-1}:00"),
  "Hydraulic Timestep 1:00",
  "Quality Timestep 0:05",
  "Pattern Timestep 1:00",
  "Pattern Start 0:00",
  "Report Timestep 1:00",
  "Report Timestep 0:00",
  "Start ClockTime 12 am",
  "Statistic NONE"
)

file.hmodel$Report <- c(
  "Status  FULL",
  "Summary Yes",
  "Energy Yes",
  "Page 0",
  "Nodes All",
  "Links All",
  "Pressure Precision 4",
  "Velocity Precision 4",
  "Flow Precision 4",
  "Headloss Precision 4"
)

# Write an epanet.inp object to params$file.hmodel
write.inp(
  file.hmodel, 
  here::here("epanet", paste0(params$file.hmodel,".inp"))
)

# runs a complete EPANET simulation
ENepanet(
  here::here("epanet",paste0(params$file.hmodel,".inp")),
  here::here("epanet",paste0(params$file.hmodel,".rpt"))
)

# reads an Epanet .rpt file into R
file.report <- read.rpt(here::here("epanet","exercise_01_Res2Res.rpt")) 

# Values for the Nodes of the Valve
nodes <- file.report$nodeResults  %>% 
  filter(ID %in% params$valve.nodes) %>% 
  select(ID, Pressure, position = Timestamp) %>% 
  mutate(position = as.integer(str_extract(position, "^(\\d+)*")),
         Pressure = Pressure/10) %>% 
  pivot_wider(names_from = ID, values_from = Pressure)  

names(nodes) <- c("position", "p1", "p2")

# Values for the Valve (Link))
links <- as_tibble( file.report$linkResults) %>% 
  filter(ID == params$valve.id) %>% 
  select(
    position = Timestamp, 
    dp = Headloss,       # in m
    flow = Flow,         # in lps
    velocity = Velocity  # in m/s
  ) %>% 
  mutate(
    position = as.integer(str_extract(position, "^(\\d+)*")),
    flow = flow * 3.6, # convert Flow from lps to m3/h
    dp = dp/10 # Convert Dp from m to bar
  ) 

# join NODES with the LINKS
params$valve.opvalues <- nodes %>% 
  full_join(links, by ="position") %>% 
  arrange(dp)

# Extract the flow without valve.
params$system.maxflow <- params$valve.opvalues$flow[1]

# Table with the Kv values 
params$valve.opvalues <- params$valve.opvalues %>% 
  filter(dp > 0) %>% 
  mutate(
    position = seq(100, 5, by = -5),
    kv.valve = kv(p1, p2, flow),
    kv.rel = kv.valve/max(kv.valve)*100,
  )

rm(nodes, links)



