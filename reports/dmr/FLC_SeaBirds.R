# FLC_SeaBirds.R

# 
# List of Tables Required:
# 
# CatchReportSpecies
# EntityRelationshipDescendant
# ObserverHaulReport
# ObserverHaulReportSpecies
# SpecialArea

library(tidyr)
library(dplyr)
library(lubridate)


load("../../../database/acct.Rdata")

.TODAY <- today()-(8-wday(today()))
.YEAR  <- c(year(today()),year(today())-1)
.WEEK  <- week(.TODAY-1)

	ohr  <- dbo$ObserverHaulReport
	ohrs <- dbo$ObserverHaulReportSpecies %>% filter(year(HaulDate)%in%.YEAR)
	cr   <- dbo$CatchReport
	crs  <- dbo$CatchReportSpecies	      
	erd  <- dbo$EntityRelationshipDescendant	
	spc  <- dbo$Species		

	bdf  <- ohrs %>% left_join(crs,by=c("CatchReportSpeciesId"="Id","PercentRetained", "CreatedDate", "LastModifiedDate", "CreatedBy", "LastModifiedBy")) %>%
	 				left_join(ohr,by=c("VesselId", "Cruise", "HaulDate", "HaulNumber", "CatchReportId","CreatedDate","LastModifiedDate","CreatedBy","LastModifiedBy")) %>%
					left_join(cr,by=c("CatchReportId"="Id","CreatedDate","LastModifiedDate","CreatedBy","LastModifiedBy"))
	
# At this point get FLC vessles from the EntityRelationshipDescendant$RootEntityId
managementProgramEric<- c(47,60,73,74)
managementProgram <- c(52,57,58,49,53,48,50,69,51,56,54)

	flc  <- erd %>% 
					filter(rootEntityId %in% managementProgramEric )  %>%
					distinct(childEntityRelationshipId) %>%
					right_join(bdf,by=c("childEntityRelationshipId"="QuotaHolderEntityRelationshipId")) %>%
					left_join(spc,by=c("SpeciesCode"="Code")) %>%
					filter(SpeciesCode %in% c(849,850,851,852,853,854,855,856,857,874,875,876,878,879,893,893,894,896,897,998,1116) )
					
# example table
	tbl.1 <- flc %>% mutate(Year=year(HaulDate)) %>% group_by(Year,Name) %>% summarise(N = sum(SampleNumber))


# Bird Sp Count
	tbl.2 <- flc %>% group_by(Name,Year=year(HaulDate)) %>% summarise(N=sum(SampleNumber)) %>% spread(Year,N)