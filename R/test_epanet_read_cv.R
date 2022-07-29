# Libraries----
library(tidyverse)
library(here)
library(lubridate)
library(epanetReader)

# Read Function
source(here::here("R", "epanet_read_cv.R"))

# Parmeter
name.hmodel <- "exercise_01_Res2Res"
cv_name <- "RIKO"

# Read Epanet model file; name in "params$file.hmodel"
hmodel <- read.inp(here::here("epanet", paste0(name.hmodel,".inp")))

# reads an Epanet .rpt file into R
hreport <- read.rpt(here::here("epanet",paste0(name.hmodel,".rpt")))

epanet_read_cv(hmodel, hreport, cv_name)
