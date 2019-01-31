library(rnassqs)
library(tidyverse)

# Get an NASS QuickStats API key: https://quickstats.nass.usda.gov/api

NASSQS_TOKEN = "2BD928ED-10EB-3FB3-9B42-F3F237A067AE"

states <- state.abb
dat <- data.frame()
for (i in unique(states)){
  hdat <- data.frame()
  pdat <- data.frame()
  qdat <- data.frame()
  check <- 0
  
# Get acres harvested
  params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="WHEAT", "source_desc"="SURVEY", "statisticcat_desc" = "AREA HARVESTED", "short_desc"="WHEAT - ACRES HARVESTED")
    a <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      hdat = nassqs_parse(req)
      check <- check + 1
    },error=function(e) e)
  
  # Get YIELD
  params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="WHEAT", "source_desc"="SURVEY", "statisticcat_desc" = "YIELD", "short_desc"="WHEAT - YIELD, MEASURED IN BU / ACRE")
      b <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      pdat = nassqs_parse(req)
      check <- check + 1
    },error=function(e) e) 
      
    # Get IRRIGATED ACRES
  params = list("state_alpha"=i, "agg_level_desc"="COUNTY","commodity_desc"="WHEAT", "source_desc"="SURVEY", "statisticcat_desc" = "AREA HARVESTED", "short_desc"="WHEAT, IRRIGATED - ACRES HARVESTED")
      c <- tryCatch({
      req = nassqs_GET(params=params, key=NASSQS_TOKEN)
      qdat = nassqs_parse(req)
      check <- check + 1
    },error=function(e) e) 
      

    dat <- rbind(dat, hdat, pdat, qdat)
    print(i)
}
  