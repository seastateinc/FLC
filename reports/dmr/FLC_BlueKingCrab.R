# ---------------------------------------------------------------------------- #
# FLC_BlueKingCrab.R
# Date; October 4, 2016
# Author: Steve Martell
# Notes:
#  This script is used to scrape the psc crab data from NOAA
# ---------------------------------------------------------------------------- #

library(lubridate)
library(dplyr)


# ---------------------------------------------------------------------------- #
#	Scrape the NOAA reports for crab psc
# ---------------------------------------------------------------------------- #
	url2015 <- "https://alaskafisheries.noaa.gov/sites/default/files/reports/car250_psc_crab2015.csv"
  # url2015 <- "car250_psc_crab2015.csv"
	url2016 <- "https://alaskafisheries.noaa.gov/sites/default/files/reports/car250_psc_crab2016.csv"

	car250_psc_crab2015 <- read.csv(url2015,skip=9) 
	car250_psc_crab2016 <- read.csv(url2016,skip=9) 

	psc <- full_join(car250_psc_crab2015,car250_psc_crab2016, 
	                 by = c("SPECIES", "GEAR", "TARGET", "WEEK", "SECTOR", 
	                        "GROUNDFISH..MT.", "NUMBER.OF.CRAB", 
	                        "NUMBER.OF.CRAB.MT.GROUNDFISH")) %>%
					mutate(Week = mdy(WEEK)) %>%
					tbl_df()

	bkcr <-  psc %>% filter(SPECIES=="BKCR") 


	bkc.catch <- bkcr %>% 
							 group_by(Year=year(Week),GEAR,SECTOR) %>% 
							 summarize("Blue King Crab"=sum(NUMBER.OF.CRAB))

