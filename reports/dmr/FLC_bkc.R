# FLC_bkc.R
# R-script for summarising blue king crab catch in FLC
library(dplyr)
library(lubridate)

load("../../../database/ObserverHaulReportPribilofHabitatAreaBlueKingStockArea.Rdata")
load("../../../database/ObserverHaulReportPribilofHabitatArea.Rdata")
load("../../../database/ObserverHaulReportSpecies.Rdata")
load("../../../database/ObserverHaulReport.Rdata")


# load("../../../database/acct.Rdata")
# attach(dbo); on.exit(detach(dbo))

 #OBS species code for blue crab = 6

PRIB <- ObserverHaulReportPribilofHabitatAreaBlueKingStockArea %>% tbl_df()


# This is a veiw that contains the field that indicates if
# the crab was landed in the Pribs Conservation Area
OHRPHA <- ObserverHaulReportPribilofHabitatArea %>% 
					tbl_df()

OHRS   <- ObserverHaulReportSpecies %>%
					tbl_df()

OHR    <- ObserverHaulReport %>% 
					left_join(OHRPHA,by=c("CatchReportId"="CatchReportId",
					                      "VesselId"="vesselId",
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

BKC <- OHR %>% filter(ReportingArea<600) %>%
			 group_by(Year,ReportingArea,Sex,SampleType) 


BKC.total <- OHR %>% 
			 group_by(Year,ReportingArea,Sex,SampleType) %>% 
			 summarise(Count=sum(round(ExtrapolatedNumber),na.rm=TRUE))

AnnualCatch <- BKC.total %>% group_by(Year) %>% summarise("Total"=sum(Count))

YearToDate  <- BKC %>% filter(week(HaulDate) <= week(.TODAY)) %>% 
	group_by(Year) %>%
	summarise("YTD"=sum(round(ExtrapolatedNumber),na.rm=TRUE))

CurrentWeek <- BKC %>% filter(week(HaulDate) == week(.TODAY)) %>%
	group_by(Year) %>%
	summarise("CurrentWeek"=sum(round(ExtrapolatedNumber),na.rm=TRUE))

	bkc_Table <- AnnualCatch %>% left_join(YearToDate) %>%
		 left_join(CurrentWeek) 


# Table Area by row 
library(tidyr)
tb1 <- BKC %>% filter(Year %in% .YEAR) %>% 
		group_by(ReportingArea,Year) %>%
		summarise(Count=sum(round(ExtrapolatedNumber),na.rm=TRUE)) %>%
		spread(Year,Count)

tb2 <- BKC %>% filter(Year %in% .YEAR,
                      week(HaulDate)<=week(.TODAY)) %>% 
		group_by(ReportingArea,Year) %>%
		summarise(Count=sum(round(ExtrapolatedNumber),na.rm=TRUE)) %>%
		spread(Year,Count)

tb3 <- BKC %>% filter(Year %in% .YEAR,
                      week(HaulDate)==week(.TODAY)) %>% 
		group_by(ReportingArea,Year) %>%
		summarise(Count=sum(round(ExtrapolatedNumber),na.rm=TRUE)) %>%
		spread(Year,Count)

tbl <- left_join(tb1,tb2,by="ReportingArea") %>%
		full_join(tb3,by="ReportingArea") %>% 
		select(-3) 

rtotal <- tbl %>% ungroup() %>% summarise_all(sum,na.rm=TRUE)

#Crab year is july 1-June 30  Need to adjust dates

# 
# 
# Map Data
# 
# library(sp)

# df <- OHR %>% mutate(Longitude=-1*Longitude) %>% 
# filter(year(HaulDate)==2016) %>%
# select(Longitude,Latitude,
#        bluekingcrab=ExtrapolatedNumber,
#        VesselType) %>% as.data.frame()

# coordinates(df) <- ~ Longitude+Latitude
# projection(df) <- "+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
# mapview(df,zcol=c("bluekingcrab","VesselType"))

# ---------------------------------------------------------------------------- #
# Determine counts in and out of Pribs		
# ---------------------------------------------------------------------------- #

T3.1 <- left_join(OHR,PRIB) %>% filter(Year %in% .YEAR) %>% group_by(Year,PHCA) %>% 
			mutate(BlueKingCrabNo=round(ExtrapolatedNumber,0)) %>%
			summarise(Count=sum(BlueKingCrabNo)) %>% na.omit() %>% 
			spread(Year,Count) %>% filter(PHCA==1)

T3.2 <- left_join(OHR,PRIB) %>% filter(Year %in% .YEAR,
                                       week(HaulDate) <= week(.TODAY)) %>% 
			group_by(Year,PHCA) %>% 
			mutate(BlueKingCrabNo=round(ExtrapolatedNumber,0)) %>%
			summarise(Count=sum(BlueKingCrabNo)) %>% na.omit() %>% 
			spread(Year,Count) %>% filter(PHCA==1)

T3.3 <- left_join(OHR,PRIB) %>% filter(Year %in% .YEAR,
                                       week(HaulDate) == week(.TODAY)) %>% 
			group_by(Year,PHCA) %>% 
			mutate(BlueKingCrabNo=round(ExtrapolatedNumber,0)) %>%
			summarise(Count=sum(BlueKingCrabNo)) %>% na.omit() %>% 
			spread(Year,Count) %>% filter(PHCA==1)

phca <- left_join(T3.1,T3.2,by="PHCA") %>% left_join(T3.3,by="PHCA")




T4.1 <- left_join(OHR,PRIB) %>% filter(Year %in% .YEAR) %>% group_by(Year,PBKCSA) %>% 
			mutate(BlueKingCrabNo=round(ExtrapolatedNumber,0)) %>%
			summarise(Count=sum(BlueKingCrabNo)) %>%
			spread(Year,Count) %>% filter(PBKCSA==1)


T4.2 <- left_join(OHR,PRIB) %>% filter(Year %in% .YEAR,
                                       week(HaulDate) <= week(.TODAY)) %>% 
			group_by(Year,PBKCSA) %>% 
			mutate(BlueKingCrabNo=round(ExtrapolatedNumber,0)) %>%
			summarise(Count=sum(BlueKingCrabNo)) %>% na.omit() %>% 
			spread(Year,Count) %>% filter(PBKCSA==1)

T4.3 <- left_join(OHR,PRIB) %>% filter(Year %in% .YEAR,
                                       week(HaulDate) == week(.TODAY)) %>% 
			group_by(Year,PBKCSA) %>% 
			mutate(BlueKingCrabNo=round(ExtrapolatedNumber,0)) %>%
			summarise(Count=sum(BlueKingCrabNo)) %>% na.omit() %>% 
			spread(Year,Count) %>% filter(PBKCSA==1)

pbkcsa <- left_join(T4.1,T4.2,by="PBKCSA") %>% left_join(T4.3,by="PBKCSA")

atbl <- full_join(rtotal[,-1],phca[,c(-1,-3)]) %>% full_join(pbkcsa[,c(-1,-3)])








