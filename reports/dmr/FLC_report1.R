# Aug 2, 2016


# ---------------------------------------------------------------------------- #
# HALIBUT DISCARD MORTALITY REPORT MOCKUP
# ---------------------------------------------------------------------------- #
# Author: steve
# Date Created: Aug 2, 2016.
# Summary:
# 	This is a mock up of a weekly halibut discard mortality rate report for 
#   vessel managers.  The objective is to inform the vessel managers about
# 	vessel performance over the past week and season-to-date performance on 
# 	halibut bycatch rates.
#
# Modifications: Aug 15, 2016.
#
# ---------------------------------------------------------------------------- #


# LIBRARIES
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

#Steve
# LINK TO DATA TABLES FROM SEASTATEINC
load("../../../database/AnalysisHaulTable.Rdata")
load("../../../database/RawObsLength.Rdata")

H <- AnalysisHaulTable %>% transform(HaulDate=as.Date(HaulDate))%>% tbl_df()
L <- RawObsLength %>% tbl_df()

.TODAY <- today()
.TODAY <- ymd("2016-07-28") # To compare with Janet's report.


# Vessel and permit numbers.
PermitVessel <- unique(L %>% select(permit,Vessel=vessel))

D <- dplyr::left_join(H,L,by=c("VesselId"="permit",
                               "Cruise"="cruise",
                               "HaulDate"="haul_offload_date",
                               "HaulNumber"="haul_offload_number"))

D <- D %>%
		 dplyr::mutate(month=lubridate::month(HaulDate),
			             depth=cut(D$BottomDepthFathoms,seq(0,500,by=50))) %>% 
		 dplyr::filter(GearCode=="HAL",
			             FmpArea=="BSAI",
			             species_name=="PACIFIC HALIBUT")

# ---------------------------------------------------------------------------- #
# Fig 1 & 2. Catch Summary for PCod & PHal in CP sector (excluding CDQ)
# ---------------------------------------------------------------------------- #
# The following filter should reduce HaulTable to CP, HAL, BSAI for FLC.
tc <- H %>%
			dplyr::filter(VesselType=="CP",
			              GearCode=="HAL",
			              FmpArea=="BSAI",
			              ManagementSector=="FLC")

yrs <- c(2015,2016)
tc <- tc %>% 
		dplyr::mutate(Year=lubridate::year(HaulDate),Week=lubridate::week(HaulDate)) %>%
		dplyr::mutate(Season=ifelse(lubridate::yday(HaulDate)<171,"A","B")) %>%
		dplyr::filter(Year %in% yrs) %>%
		dplyr::group_by(Year,Week,Season)

flc_catch <-tc %>% 
						summarise(PCodWt=sum(PacificCodWt),
                      PHalWt=sum(HalibutMortalityWt),
                      PHalCt=sum(PreMortalityPacificHalibutWt)) %>%
						mutate(PHalRt=PHalCt/PCodWt)  											%>%
						group_by(Year) 																			%>%
						mutate(PHalCw=cumsum(PHalWt)) 											%>%
						mutate(PCodCw=cumsum(PCodWt))
							
flc_catch.g <- gather(flc_catch,Variable,Value,PCodWt:PCodCw) %>%
						mutate(Variable=as.factor(Variable))

p1 <- ggplot(flc_catch%>%filter(Season=="B"),
       			 aes(Week,PCodWt,fill=factor(Year)))+
			geom_bar(stat="Identity",alpha=0.5,position="dodge") +
			labs(x="Week of the Year",y="Catch (mt)",fill="Year")
			ggtitle("Weekly catch")

p2 <- ggplot(flc_catch%>%filter(Season=="B"),
       			 aes(Week,PCodCw,fill=factor(Year)))+
			geom_bar(stat="Identity",alpha=0.5,position="dodge") +
			labs(x="Week of the Year",y="Catch (mt)",fill="Year") +
			ggtitle("Cumulative catch")




## Feedback from FLC.  For p3, need to winnow in on current week.
## Maybe add grey polygon for previous years, and transparent barplot
## for the current year for clarity.
wk   <- seq(week(today())-3,week(today())+3)
gdat <- flc_catch.g %>% 
				filter(Season=="B",Variable %in% c("PCodWt"),
				       Week <= max(wk))

# change labels for facets
levels(gdat$Variable)[1:2] <- c("Weekly Catch")				

p3 <- ggplot(gdat) + 
			geom_area(aes(Week,Value/1000),data=gdat%>%filter(Year==year(today())-1),fill="orange",alpha=0.5) +
			geom_bar(aes(Week,Value/1000),data=gdat%>%filter(Year==year(today())),stat="identity",alpha=0.5)+
			labs(x="Week of the year",y="Catch (1000 mt)",fill="Year") 
			
			# geom_bar(stat="identity",position="dodge") +
			# facet_wrap(~Variable,scales="free_y")


hdat <- flc_catch.g %>%
				filter(Season=="B",Variable %in% c("PHalWt","PHalCw"))

# change labels for facets
levels(hdat$Variable)[c(3,5)] <- c("Cumulative Mortality","Weekly Mortality")	

p4 <- ggplot(hdat,aes(Week,Value,fill=factor(Year))) + 
			geom_bar(stat="identity",position="dodge") +
			labs(x="Week of the year",y="Mortality (mt)",fill="Year") +
			facet_wrap(~Variable,scales="free_y")				

# ---------------------------------------------------------------------------- #
# Table 1. Summary (all vessels) viability sampling.
# ---------------------------------------------------------------------------- #
str_injury=c("Minor","Moderate","Severe","Dead")
mr <- c(0.035,0.363,0.662,1.000)				# discard mortality rates for code 1-4
t1 <- D %>% 
			dplyr::filter(injury != 9) %>%  	# remove missing viability samples.
			dplyr::group_by(injury)


c1 <- t1 %>% dplyr::summarise("ytd"=n())
c2 <- t1 %>% dplyr::filter(HaulDate >= today()-14) %>% dplyr::summarise("p2w"=n())
c3 <- t1 %>% dplyr::filter(HaulDate >= today()-07) %>% dplyr::summarise("p1w"=n())

counts <- full_join(c1,c2,by="injury") 	%>% 
					full_join(c3,by="injury") 		%>% 
					rename("Code"=injury)					%>%
					cbind(str_injury)

counts[is.na(counts)]=0

TBL_1 <- counts %>%
				dplyr::select("Injury Type"=str_injury,
				  						"Previous week"=p1w,
				  						"Previous 2-weeks"=p2w,
				  						"Year to date"=ytd)






# ---------------------------------------------------------------------------- #
# Table 2. Discard mortality rate summary by stat area.
# ---------------------------------------------------------------------------- #
td <- H %>%
			dplyr::filter(VesselType=="CP",
			              GearCode=="HAL",
			              FmpArea=="BSAI") %>%
			dplyr::group_by(RA=ReportingArea)

tripTarget <- c("Cod","Turbot")

fa_catch <- td %>% ungroup() %>% group_by(TripTarget,RA) %>% 
	filter(HaulDate >= today()-7,TripTarget %in% tripTarget) %>%
	# summarise(PCod.mt=sum(PacificCodWt),
	#           PHal.kg=sum(PreMortalityPacificHalibutWt)*1000,
	#           PHal.mt=sum(HalibutMortalityWt)) %>%
	summarise_each(funs(sum),
	               # PacificCodWt,
	               # TurbotWt,
	               Otc,
	               PreMortalityPacificHalibutWt,
	               HalibutMortalityWt) %>%
	# mutate(Rate=PreMortalityPacificHalibutWt*1000/PacificCodWt) %>%
	# dplyr::select(Target=TripTarget,
	#               `NMFS\nReg Area`=RA,
	#               `Cod (mt)`=PacificCodWt,
	#               `Halibut (kg)`=PreMortalityPacificHalibutWt,
	#               `Encounter\nRate (kg/mt)`=Rate,
	#               `Halibut\nMortality (mt)` = HalibutMortalityWt) %>%
	# tidyr::gather(Target,Catch,PacificCodWt:TurbotWt) %>%
	group_by(TripTarget,RA) %>%
	summarise_each(funs(sum),
	               Otc,
	               PreMortalityPacificHalibutWt,
	               HalibutMortalityWt) %>%
	mutate(Rate = PreMortalityPacificHalibutWt/Otc*1000) %>%
	select('Trip\nTarget'=TripTarget,
	       'NMFS\nReg Area'=RA,
	       'OTC (mt)' = Otc,
	       'Halibut (mt)'=PreMortalityPacificHalibutWt,
	       'Encounter\nRate (kg/mt)'=Rate,
	       'Halibut\nMortality (mt)' = HalibutMortalityWt)



# cs_catch <- fa_catch %>% group_by(Target) %>% summarise_each(funs(sum))


# catch_tbl <- bind_rows(fa_catch,cs_catch)


# ---------------------------------------------------------------------------- #
#	Rank Vessels by Halibut bycatch rates
# ---------------------------------------------------------------------------- #

vesselRates.old <- H %>%
		dplyr::filter(VesselType == "CP",
		              GearCode   == "HAL",
		              FmpArea    == "BSAI") %>%
		dplyr::mutate(PacificHalibutRate = PreMortalityPacificHalibutWt/Otc) %>%
		dplyr::filter(HaulDate >= today()-7,TripTarget %in% tripTarget) %>%
		dplyr::left_join(PermitVessel,by=c("VesselId"="permit")) %>%
		dplyr::group_by(TripTarget,Vessel) %>%
		dplyr::summarise(Rate = sum(PreMortalityPacificHalibutWt*1000)/sum(Otc))%>%
		dplyr::mutate(Rank = rank(Rate)) %>% 
		dplyr::group_by(TripTarget) %>%
		dplyr::arrange(Rate)



# ---------------------------------------------------------------------------- #
# Halibut Hotspot map  (USE MARMAP for BATHYMETRY)
# ---------------------------------------------------------------------------- #
# library(ggmap)
# library(maptools)
# library(rgdal)
# library(RColorBrewer)
	
# 	# Define the based map.
# 	map <- get_map("St Paul Alaska",zoom=5,maptype="hybrid",color="bw")
# 	bmap <- ggmap(map)

# mapData <- H %>%
# 			dplyr::filter(VesselType=="CP",
# 			              GearCode=="HAL",
# 			              FmpArea=="BSAI") %>%
# 			dplyr::mutate(Longitude=-Longitude,
# 			              Rate = PreMortalityPacificHalibutWt/Otc*1000) %>%
# 			dplyr::filter(HaulDate >= today()- 14) %>%
# 			dplyr::select(HaulDate, HaulNumber, lon=Longitude, lat=Latitude, Rate) %>%
# 			dplyr::mutate(hotSpot = ifelse(Rate>quantile(Rate,na.rm=TRUE,prob=0.9),TRUE,FALSE))

# p <- bmap + geom_point(data=mapData %>% filter(hotSpot==TRUE),aes(lon,lat))







# ---------------------------------------------------------------------------- #
# Table X. Discard mortality rate summary by stat area.
# ---------------------------------------------------------------------------- #
df <- data.frame("Type"=str_injury,injury=1:4,mr)
t2 <- D %>% 
			dplyr::filter(injury !=9) %>%
			dplyr::group_by(RetrieveAdfgStatArea,injury)

# Stat area rates
rates <- t2 %>% 
			rename(id=RetrieveAdfgStatArea) %>% 
			summarise(count=n()) %>% 
			full_join(df,by="injury") %>%
			group_by(id) %>%
			summarise(dmr = (count/sum(count)) %*% mr )

# Summary Table with top 10 Stat Areas.



# Map with ADFG Stat Area Rates
# See http://www.markhneedham.com/blog/2014/11/17/r-ggmap-overlay-shapefile-with-filled-polygon-of-regions/

getMap <- function(){

	map <- get_map("St Paul Alaska",zoom=5,maptype="hybrid")
	p <- ggmap(map)

	# ADF&G stat area shapefile
	sfn<-(readOGR("../../../HotSpots/data/gis_shapefiles/pvg_stat_2001/","pvg_stat_2001"))
	sfn.f <- sfn %>% fortify(region="STAT_AREA")
	# some code to deal with anitmeridian
	sfn.f$long[sfn.f$long>0] <- -(180 + (180 - sfn.f$long[sfn.f$long>0]))

	statAreaRates <- merge(sfn.f,sfn@data,by.x='id',by.y='STAT_AREA') %>% 
									tbl_df() %>% mutate(id=as.integer(id)) %>%
									right_join(rates,by="id")

	## Need a lighter weight map.
	p <- p +  
		geom_polygon(aes(fill=dmr,x=long,y=lat,group=group),
		             data=statAreaRates,alpha=0.40,size=0.1) +
		scale_fill_distiller("DMR",palette="Spectral",direction=-1)

}




























