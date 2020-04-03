#library(tidyverse)

# read in Archived flows

QGP   <- "Queensland Gas Pipeline"  
APLNG <-"APLNG Pipeline"
GLNG  <-"GLNG Gas Transmission Pipeline" 
WGP   <-"Wallumbilla to Gladstone Pipeline" 


if (!exists("archived.flows.csv")) archived.flows.csv <- "data/archived/ActualFlows.csv"

arch.flows <- read_csv(archived.flows.csv ) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::mutate(gasdate=lubridate::dmy(gasdate), lastchanged=lubridate::dmy_hm(lastchanged)) %>%
  dplyr::rename(facilityid=plantid, facilityname=plantname, locationid=zoneid, locationname=zonename, lastupdated=lastchanged) 

# names(arch.flows)
# 
# q.flows<-arch.flows %>% subset(facilityname %in% c(QGP, 
#                                           APLNG,
#                                           GLNG, 
#                                           WGP  ))
# ggplot(q.flows, aes(gasdate, actualquantity, col=facilityname))+geom_line()
# 
       