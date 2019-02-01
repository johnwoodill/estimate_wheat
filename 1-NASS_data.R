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

saveRDS(dat, "data/wheat_data.rds")

ndat <- readRDS("data/full_ag_data.rds")
head(ndat)

head(dat)
dat$fips <- as.numeric(paste0(dat$state_fips_code, dat$county_code))
#dat$Value <- as.numeric(gsub(",", "", dat$Value))
mdat <- select(dat, year, state_alpha, fips, short_desc, Value)
mdat$row <- 1:nrow(mdat)
ldat <- spread(mdat, key=short_desc, value=Value)
ldat <- ldat %>% 
    group_by(state_alpha, fips, year) %>% 
    summarise_each(funs(sum(., na.rm=TRUE))) 
ldat$wheat_irrigated_a <- ifelse(ldat$`WHEAT - ACRES HARVESTED` != 0, ldat$`WHEAT, IRRIGATED - ACRES HARVESTED`/ldat$`WHEAT - ACRES HARVESTED`, 0)
ldat$wheat_irrigated_a <- ifelse(ldat$wheat_irrigated_a > 1, ((ldat$`WHEAT, IRRIGATED - ACRES HARVESTED`)/100)/ldat$`WHEAT - ACRES HARVESTED`, ldat$wheat_irrigated_a)
summary(ldat$wheat_irrigated_a)

ldat <- select(ldat, state_alpha, fips, year, wheat_irrigated_a)
names(ldat)[1] <- "state"
ldat$state <- tolower(ldat$state)
ldat$year <- as.numeric(ldat$year)
ndat <- left_join(ndat, ldat, by = c("state", "fips", "year"))
saveRDS(ndat, "data/reg_data.rds")
