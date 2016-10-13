# FLC_bkc.R
# R-script for summarising blue king crab catch in FLC
library(dplyr)
library(lubridate)

load("../../../database/ObserverHaulReportPribilofHabitatArea.Rdata")
load("../../../database/ObserverHaulReportSpecies.Rdata")
load("../../../database/ObserverHaulReport.Rdata")


# load("../../../database/acct.Rdata")
# attach(dbo); on.exit(detach(dbo))

 #OBS species code for blue crab = 6

# This is a veiw that contains the field that indicates if
# the crab was landed in the Pribs Conservation Area
OHRPHA <- ObserverHaulReportPribilofHabitatArea %>% 
					tbl_df()

OHRS   <- ObserverHaulReportSpecies %>%
					tbl_df()

OHR    <- ObserverHaulReport %>% 
					left_join(OHRPHA,by=c("CatchReportId"="CatchReportId",
					                      "VesselId"="vesselx`Id",
					                      "Cruise"="cruise",
					                      "HaulDate"="haulDate",
					                      "HaulNumber"="haulNumber")) %>% 
					left_join(OHRS,by = c("VesselId", 
					                      "Cruise", 
					                      "HaulDate", 
					                      "HaulNumber", 
					                      "CreatedDate",
					                      "LastModifiedDate",
					                      "CreatedBy",
					                      "LastModifiedBy")) %>%
					filter(#FmpArea=="BSAI",
					       GearCode=="HAL",
					       # GearCode %in% c("HAL","POT"),
					       # VesselType=="CP",
					       # HaulSampled == 1,
					       SpeciesCode %in% c(6)) %>%
					mutate(Year = year(HaulDate)) %>%
					tbl_df()


BKC <- OHR %>% 
			 group_by(Year,ReportingArea,Sex,SampleType) %>% 
			 summarise(Count=sum(round(ExtrapolatedNumber),na.rm=TRUE))

AnnualCatch <- BKC %>% group_by(Year) %>% summarise("Blue King Crab"=sum(Count))
# 
# 
# Map Data
# 
library(sp)

df <- OHR %>% mutate(Longitude=-1*Longitude) %>% 
filter(year(HaulDate)==2016) %>%
select(Longitude,Latitude,
       bluekingcrab=ExtrapolatedNumber,
       VesselType) %>% as.data.frame()

coordinates(df) <- ~ Longitude+Latitude
projection(df) <- "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
mapview(df,zcol=c("bluekingcrab","VesselType"))


















