USE [Seastate]
GO

--PribilofHabitatConservationArea
--could join view ObserverHaulReportPribilofHabitatArea to CatchReport or ObserverHaulReport or AnalysisHaulTable (or replicate logic in this select)
select CatchReportId, cruise, vesselId, haulDate, haulNumber, latitude, -1*longitude longitude, Code 
from ObserverHaulReport 
left join (select * from SpecialArea where Code = 'PHCA') p 
on p.[Geometry].STIntersects(geography::Point(latitude, case when longitude <=180 then -1*longitude else 360-longitude end, 4326)) = 1


 --Clipper Seafoods birds for current year using ObserverHaulReport 
 select * from observerhaulreportspecies ohrs 
 join catchreportspecies crs
 on ohrs.catchreportspeciesid = crs.id
 join observerhaulreport ohr
 on ohr.catchreportid = crs.catchreportid
 join catchreport cr 
 on cr.id = crs.catchreportid
 --all descendant entity relationships of clipper seafoods 
 join (select distinct childEntityRelationshipId from EntityRelationshipDescendant where rootEntityId = 50) erd 
 on erd.childEntityRelationshipId = cr.quotaholderentityrelationshipid
 join Species sp
 on sp.code = ohrs.speciescode
 where ohrs.Speciescode
 --obs code list from janet
 in (849,850,851,852,853,854,855,856,857,874,875,876,878,879,893,893,894,896,897,998,1116)
 and year(ohrs.haulDate) = year(getDate())
 
 --All FLC birds for current year using ObserverHaulReport
 select * from observerhaulreportspecies ohrs 
 join catchreportspecies crs
 on ohrs.catchreportspeciesid = crs.id
 join observerhaulreport ohr
 on ohr.catchreportid = crs.catchreportid
 join catchreport cr 
 on cr.id = crs.catchreportid
 --all descendant entity relationships of 4 FLC sectors
 join (select distinct childEntityRelationshipId from EntityRelationshipDescendant where rootEntityId in (47, 60, 73, 74)) erd 
 on erd.childEntityRelationshipId = cr.quotaholderentityrelationshipid
 join Species sp
 on sp.code = ohrs.speciescode
 where ohrs.Speciescode
 --obs code list from janet
 in (849,850,851,852,853,854,855,856,857,874,875,876,878,879,893,893,894,896,897,998,1116)
 and year(ohrs.haulDate) = year(getDate())
