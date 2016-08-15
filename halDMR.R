# halDMR.R
library(dplyr)
library(ggplot2)
library(lubridate)

# Load data
load("../database/AnalysisHaulTable.Rdata")
load("../database/RawObsLength.Rdata")

H <- AnalysisHaulTable %>% transform(HaulDate=as.Date(HaulDate))%>% tbl_df()
L <- RawObsLength %>% tbl_df()

D <- dplyr::left_join(H,L,by=c("VesselId"="permit",
                               "Cruise"="cruise",
                               "HaulDate"="haul_offload_date",
                               "HaulNumber"="haul_offload_number")) %>% 
			dplyr::mutate(month=month(HaulDate),
			              depth=cut(D$BottomDepthFathoms,seq(0,500,by=50))) %>% 
			dplyr::filter(GearCode=="HAL",FmpArea=="BSAI",species_name=="PACIFIC HALIBUT")

E <- 	D  %>%
			group_by(month,injury,length) %>% 
			summarise(frequency=sum(frequency))

F <- 	D %>%
			group_by(depth,injury,length) %>%
			summarise(frequency=sum(frequency))

p1 <- ggplot(E,aes(length,frequency,color=factor(injury)))+
			geom_freqpoly(stat="identity")+facet_wrap(~month) +
			labs(x="Halibut length (cm)",y="Frequency",color="Injury Code")

p2 <- ggplot(F,aes(length,frequency,color=factor(injury)))+
			geom_freqpoly(stat="identity")+facet_wrap(~depth) +
			labs(x="Halibut length (cm)",y="Frequency",color="Injury Code")



	p3 <- ggplot(E %>% filter(injury!=9),aes(length,frequency,fill=factor(injury)))+
				geom_bar(stat="identity",position="fill",alpha=0.8)+
				facet_wrap(~month) +
				labs(x="Halibut length (cm)",y="Proporition",fill="Injury Code") +
				scale_fill_brewer(palette=2,type="qual")

	p4 <- ggplot(F %>% filter(injury!=9),aes(length,frequency,fill=factor(injury)))+
				geom_bar(stat="identity",position="fill",alpha=0.8)+
				facet_wrap(~depth,dir="v") +
				labs(x="Halibut length (cm)",y="Proporition",fill="Injury Code") +
				scale_fill_brewer(palette=4,type="qual")
# select * from AnalysisHaul h
# left join RawObsLength l
# on h.vesselId = l.permit
# and h.cruise = l.cruise
# and h.HaulDate = l.haul_offload_date
# and h.HaulNumber = l.haul_offload_number



