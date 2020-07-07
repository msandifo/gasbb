# fn <-"~/Dropbox/msandifo/Documents/programming/r/2020/gasbb/data/Actual Flow and Storage_20200229092916.csv"
# fn <-"~/Dropbox/msandifo/documents/programming/r/2020/gasbb/data/Actual Flow and Storage_20200302091154.csv"
# fn <-"~/Dropbox/msandifo/documents/programming/r/2020/gasbb/data/Actual Flow and Storage_20200304114556.csv"
# QGP   <- "Queensland Gas Pipeline"  
# APLNG <-"APLNG Pipeline"
# GLNG  <-"GLNG Gas Transmission Pipeline" 
# WGP   <-"Wallumbilla to Gladstone Pipeline" 
# 

source("r/read_flows.R")
# Curtis Island demand 
flows.ci <- flows %>% 
  subset(locationname == "Curtis Island" & facilitytype=="PIPE") %>% 
  group_by(gasdate) %>%
  summarise(demand=sum(demand)) %>% 
  mutate(month= year(gasdate)+(month(gasdate)-.5)/12)
flows.ci %>% tail()
# #archived data
# if (!exists("arch.flows"))  source('~/Dropbox/msandifo/documents/programming/r/2020/gasbb/ci.arch.flows.R')
#   
flows.ci.arhived <- archived.flows %>%
  subset(facilityname %in% c(WGP, APLNG, GLNG  ) & flowdirection=="DELIVERY") %>%
    group_by(gasdate) %>%
    summarise(demand=sum(actualquantity)) %>%
    mutate(month= year(gasdate)+(month(gasdate)-.5)/12)
 flows.ci.b <-bind_rows(flows.ci.arhived,flows.ci) #%>% subset(gasdate>ymd("2015-01-01"))
  
# names(flows.ci.arhived)
# (flows %>% subset(LocationName == "Curtis Island" & FacilityType=="PIPE"))$FacilityName %>% unique()
flow.qgc<-flows %>% 
  subset(facilityname == "Queensland Gas Pipeline" & locationname=="Regional - QLD")


prod.qld <-flows %>% subset(locationname == "Regional - QLD" & facilitytype=="PROD") %>% 
  group_by(gasdate) %>%
  summarise(supply=sum(supply)) %>% 
  mutate(month= year(gasdate)+(month(gasdate)-.5)/12)


prod.qld.archived <- archived.flows %>%
  subset(state %in% c("QLD") & facilitytype=="PROD")%>%
  group_by(gasdate) %>%
  summarise(supply=sum(actualquantity)) %>% 
  mutate(month= year(gasdate)+(month(gasdate)-.5)/12)

prod.qld.b<-bind_rows(prod.qld.archived,prod.qld ) %>%
  merge( flows.ci.b[,c("gasdate", "demand")]) %>%
  mutate(balance= supply-demand)

prod.qld.b.month<-prod.qld.b %>% group_by(month)  %>%
  summarise(gasdate=mean(gasdate),supply=mean(supply), demand=mean(demand), balance=supply-demand)

#-------

