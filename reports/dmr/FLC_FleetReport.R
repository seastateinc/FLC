# ---------------------------------------------------------------------------- #
# MOCKUP for FLC FLEET REPORT
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
#  FEEDBACK:
# 		- Mostly positive, reduce the number of graphics. Weekly catch summary
# 			seems important, cumulative catch of cod & halibut not needed.
#   	- Chad asked to see the dates aligned with Janet's next report. 
# 		- Janet bases reports on the totals ending on Saturdays.
#
# Modifications Aug 23, 2016.
# 	- Optimized the number of filters to improve performance and logic.
# 	- Construct a table with columns (YTD: 2015, 2016, Past2Weeks, Past1Week)
# 			- Rows:
#       - Number of Sets,
#       - OTC (mt)
#				- Pacific Cod (mt)
#       - Greenland Turbot (mt)
#				- Halibut Discards (kg)
#				- Halibut Encounter Rate (kg/mt OTC)
#				- Halibut Mortality (kg)
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# LIBRARIES
# ---------------------------------------------------------------------------- #
library(tidyr)
library(dplyr) 
library(ggplot2)
library(lubridate)
library(pander)
library(scales)
# ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#	GLOBAL VARIABLES
# ---------------------------------------------------------------------------- #
# Note that Janet Smoker's reports use weekly data through the end of saturdays.
	.TODAY <- today()
	.TODAY <- today()-(8-wday(today()))
	# .TODAY <- as.Date("2016-07-28") 					# To compare with Janet's report.
	.YEAR  <- c(year(.TODAY)-1,year(.TODAY))
	.WEEK  <- week(.TODAY-1)
	.SDAY  <- as.Date("2015-01-03")-7
	.BDAY  <- as.Date("2016-06-10")
	.Rday1  <- as.Date("2015-06-4")						# Start date for weekly reporting.
	.Rday2  <- as.Date("2016-06-4")						# Start date for weekly reporting.
	.RWKS  <- c( seq(.Rday1, .TODAY-366, by=7),
	             seq(.Rday2, .TODAY,     by=7))
	.RWKS  <- seq(.SDAY, .TODAY+6, by = 7)







# ---------------------------------------------------------------------------- #
# LINK TO DATA TABLES FROM SEASTATEINC
# ---------------------------------------------------------------------------- #
	# ALT AnalysisLandingTable
	load("../../../database/AnalysisLandingTable.Rdata")
	ALT <- AnalysisLandingTable %>% tbl_df()	

	# AnalysisHaulTable -> AHT
	load("../../../database/AnalysisHaulTable.Rdata")
	AHT <- AnalysisHaulTable %>%
				 transform( HaulDate=as.Date(HaulDate) ) %>% 
				 dplyr::filter(VesselType == "CP",
				               GearCode == "HAL",
				               FmpArea == "BSAI",
				               ManagementSector %in% c("FLC","FLC - BSAI - non-Cod")) %>%
				 dplyr::mutate(Year=lubridate::year(HaulDate),
				               Week=lubridate::week(HaulDate) ) %>%
				 dplyr::mutate( RepWeek = as.Date(cut(HaulDate,breaks = .RWKS, 
				                                     right=TRUE,inc=FALSE)) + 7 ) %>%
				 dplyr::mutate( Rweek = week(RepWeek) ) %>%
				 dplyr::mutate(PHalWt.rate = HalibutMortalityWt / TotalFmpGroundfishWeight * 1000) %>%
				 dplyr::mutate(PHalWt.Erate = PreMortalityPacificHalibutWt / TotalFmpGroundfishWeight * 1000) %>%
				 dplyr::tbl_df()

	# Sampling period dates
	Period <- AHT %>% filter(WeekNumber == week(.TODAY)) %>% summarise(sow=min(HaulDate),eow=max(HaulDate))


	# RawObsLength - > ROL
	load("../../../database/RawObsLength.Rdata")
	ROL <- RawObsLength %>%
				 transform( haul_offload_date=as.Date(haul_offload_date)) %>%
				 tbl_df()

	# Vessel and permit numbers.
	PermitVessel <- unique(ROL %>% select(permit,Vessel=vessel))


	# Join up AHT and ROL into a data.frame
	D <- dplyr::left_join(AHT,ROL,
	                      	by=c("VesselId"="permit",
                               "Cruise"="cruise",
                               "HaulDate"="haul_offload_date",
                               "HaulNumber"="haul_offload_number"))
# ---------------------------------------------------------------------------- #








# ---------------------------------------------------------------------------- #
# OVERVIEW TABLE
# ---------------------------------------------------------------------------- #
	O <- AHT %>%
		group_by(Year,WeekNumber) %>%
							summarise(PHalWt.mt=sum(PreMortalityPacificHalibutWt),
							PCodWt.mt=sum(PacificCodWt),
							EncounterRate=weighted.mean(PHalWt.Erate,TotalFmpGroundfishWeight),
							NumberOfSets=n(),
							PHalMortality.mt = sum(HalibutMortalityWt))

	O1 <- O %>% 
		group_by(Year) %>% filter(WeekNumber <= .WEEK) %>%
		summarise(PHalWt.mt=sum(PHalWt.mt),
		          PCodWt.mt=sum(PCodWt.mt),
		          EncounterRate=mean(EncounterRate),
		          NumberOfSets = sum(NumberOfSets),
		          PHalMortality.mt = sum(PHalMortality.mt)) %>%
		filter(Year %in% .YEAR)

	O2 <- O %>% 
		filter(WeekNumber %in% .WEEK,Year %in% .YEAR)

	OverviewTable <- as.data.frame(t(rbind(O1[,-1],O2[,c(-1,-2)])))
	colnames(OverviewTable) <- c(paste("Cumulative\n", .YEAR),
	                             paste("Week",.WEEK," \n", .YEAR))
	rownames(OverviewTable) <- c("P. Halibut",
	                             "P. Cod",
	                             "Avg. Rate (kg/mt)",
	                             "Number of Sets",
	                             "Halibut Mortality")

# ---------------------------------------------------------------------------- #







# ---------------------------------------------------------------------------- #
# CATCH SUMMARY
# ---------------------------------------------------------------------------- #
	G2 <- O %>% 
			filter(Year %in% .YEAR) %>% 
			# filter(between(WeekNumber, week(.BDAY-14),week(.TODAY+6))) %>%
			filter(between(WeekNumber, week(.TODAY-72),week(.TODAY+6))) %>%
			gather(Variable,Value,-Year,-WeekNumber) %>%
			filter(Variable %in% c("PHalWt.mt","PCodWt.mt")) %>%
			ggplot(aes(factor(WeekNumber),Value,fill=factor(Year))) + 
			geom_bar(stat="identity",position="dodge",alpha=0.75) +
			facet_wrap(~Variable,scale="free_y",
			           labeller = as_labeller(c(`PHalWt.mt`= "Pacific Halibut",
			                                  `PCodWt.mt`= "Pacific Cod"))) + 
			labs(x="Week of the Year",y="Metric tons caught",fill="Year")

# ---------------------------------------------------------------------------- #






# ---------------------------------------------------------------------------- #
# STAT AREA RATES
# ---------------------------------------------------------------------------- #
	sar <- AnalysisHaulTable %>%
		 transform( HaulDate=as.Date(HaulDate) ) %>% 
				 dplyr::filter(VesselType == "CP",
				               GearCode == "HAL",
				               FmpArea == "BSAI",
				               ManagementSector %in% c("FLC","CDQ","FLC - BSAI - non-Cod")) %>%
				 dplyr::mutate(Year=lubridate::year(HaulDate),
				               Week=lubridate::week(HaulDate) ) %>%
				 dplyr::mutate( RepWeek = as.Date(cut(HaulDate,breaks = .RWKS, 
				                                     right=TRUE,inc=FALSE)) + 7 ) %>%
				 dplyr::mutate(PHalWt.kg = PreMortalityPacificHalibutWt *1000,
				               PHalWt.rate = PreMortalityPacificHalibutWt / TotalFmpGroundfishWeight * 1000) %>%
				 dplyr::tbl_df() %>%
		 group_by(RepWeek,TripTarget,RA=ReportingArea) %>%
		 summarise(NSets = n(),
		           GF.mt = sum(TotalFmpGroundfishWeight),
		           PHalWt.kg = sum(PHalWt.kg),
		           Rate = weighted.mean(PHalWt.rate,TotalFmpGroundfishWeight),
		           Mort = sum(HalibutMortalityWt)) %>%
		 filter( RepWeek == .RWKS[length(.RWKS)-1]) %>%
		 group_by(TripTarget,RA) 
		 
		 # --- deprecate ---
		 statAreaRates <- sar %>% 
		 		select("Trip\nTarget"=TripTarget,
		        "NMFS\nArea" = RA,
		        "Groundfish\n(mt)" = GF.mt,
		        "Halibut (kg)" = PHalWt.kg,
		        "Rate (kg/mt)" = Rate,
		        "Halibut \nMortality (mt)" = Mort)
		 # ^^^ deprecate ^^^

		 	statAreaTotals <- sar %>% ungroup() %>%
		 		summarise_at(vars(NSets,GF.mt,PHalWt.kg,Mort),funs(sum)) %>%
		 		mutate(TripTarget="Total",Rate=PHalWt.kg/GF.mt) 

		 	sar_Table <- full_join(sar,statAreaTotals) %>%
		 		select("Trip\nTarget"=TripTarget,
		        "NMFS\nArea" = RA,
		        "Groundfish\n(mt)" = GF.mt,
		        "Halibut (kg)" = PHalWt.kg,
		        "Rate (kg/mt)" = Rate,
		        "Halibut \nMortality (mt)" = Mort)

# ---------------------------------------------------------------------------- #









# ---------------------------------------------------------------------------- #
#	Rank Vessels by Halibut bycatch rates
# ---------------------------------------------------------------------------- #

	vesselRates <- AnalysisHaulTable %>%
		transform( HaulDate=as.Date(HaulDate) ) %>% 
		dplyr::filter(VesselType == "CP",
		             GearCode == "HAL",
		             FmpArea == "BSAI",
		             ManagementSector %in% c("FLC","CDQ","FLC - BSAI - non-Cod")) %>%
		dplyr::mutate(Year=lubridate::year(HaulDate),
		             Week=lubridate::week(HaulDate) ) %>%
		dplyr::mutate( RepWeek = as.Date(cut(HaulDate,breaks = .RWKS, 
		                                   right=TRUE,inc=FALSE)) + 7 ) %>%
		dplyr::mutate(PHalWt.kg = PreMortalityPacificHalibutWt *1000,
		             PHalWt.rate = PreMortalityPacificHalibutWt / TotalFmpGroundfishWeight * 1000) %>%
		dplyr::tbl_df() %>%
		dplyr::left_join(PermitVessel,by=c("VesselId"="permit")) %>%
		dplyr::filter(TripTarget %in% c("Cod","Turbot")) %>%
		dplyr::filter( RepWeek == .RWKS[length(.RWKS)-1]) %>%
		dplyr::group_by(TripTarget,Vessel) %>%
		dplyr::summarise(Rate = weighted.mean(PHalWt.rate,TotalFmpGroundfishWeight))%>%
		# dplyr::summarise(Rate = sum(PreMortalityPacificHalibutWt*1000)/sum(TotalFmpGroundfishWeight))%>%
		dplyr::mutate(Rank = rank(Rate)) %>% 
		dplyr::arrange(TripTarget,Rate) %>%
		rename("Trip Target" = TripTarget)

# ---------------------------------------------------------------------------- #









# ---------------------------------------------------------------------------- #
# VIABILITY SAMPLING AND DISCARD MORTALITY RATES
# ---------------------------------------------------------------------------- #

	# discard mortality rates for code 1-4
	InjuryCodes 	 <- c("Minor","Moderate","Severe","Dead") %>% as.factor()
	MortalityRates <- c(0.035,0.363,0.662,1.000)
	mr <- data.frame(Code=InjuryCodes,mr=MortalityRates) %>% tbl_df()

	NumberOfHaulsSampled <- AHT %>% 
		dplyr::filter(HaulSampled == "Y", Year %in% .YEAR) %>% 
		dplyr::group_by(Year) %>%
		dplyr::summarise(n())

	T3 <- D %>%
				dplyr::filter(HaulSampled == "Y", 
				              Year %in% year(.TODAY),
				              injury != 9) %>%
				dplyr::group_by(TripTarget,injury)

	c1 <- T3 %>% dplyr::summarise("ytd"=n())
	c2 <- T3 %>% dplyr::filter(WeekNumber == .WEEK) %>% dplyr::summarise('Week1'=n())
	c3 <- T3 %>% dplyr::filter(WeekNumber == .WEEK-1) %>% dplyr::summarise('Week2'=n())

	counts <- left_join(c1,c2,by=c("TripTarget","injury")) %>% 
						left_join(c3,by=c("TripTarget","injury")) %>%
						rename("Code" = injury) 
  counts[is.na(counts)] <- 0

	counts$Code <- InjuryCodes[counts$Code]


 	fdmr <- function(x,w) {
 		wmu <- weighted.mean(x,w,na.rm=TRUE)
 		wmu <- ifelse(is.na(wmu),0,wmu)
 		return(wmu)
 	}


	dmr <- left_join(counts,mr,by="Code") %>%
			dplyr::group_by(TripTarget) %>%
			summarise(dmr.ytd=percent(fdmr(mr,ytd)),
			          dmr.wk1=percent(fdmr(mr,Week1)),
			          dmr.wk2=percent(fdmr(mr,Week2)))
			# dplyr::summarise(dmr.ytd=percent(weighted.mean(mr,ytd,na.rm=TRUE)),
			                 # dmr.wk1=percent(weighted.mean(mr,Week1,na.rm=TRUE)),
			                 # dmr.wk2=percent(weighted.mean(mr,Week2,na.rm=TRUE))) 
	colnames(dmr) <- c("Trip\nTarget","DMR\nYear-to-date",
	                   paste("Week\n",c(.WEEK,.WEEK-1)))


	TBL_1 <- counts
	colnames(TBL_1) <- c("Trip\nTarget","Injury Type","Year-to-date",
	                   paste("Week\n",c(.WEEK,.WEEK-1)))
# ---------------------------------------------------------------------------- #






























