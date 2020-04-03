#prepare archived flows

# archived.flows.csv <- "data/archived/ActualFlows.csv"
# current.flows.csv <-"data/Actual Flow and Storage_20200302091154.csv"
# current.flows.csv <-"data/Actual Flow and Storage_20200304114556.csv"
# update=T

if (!exists("current.flows") | update) current.flows <- data.table::fread(current.flows.csv, skip=0 ) %>%  
  as_tibble() %>%
  mutate(GasDate= dmy(GasDate)) %>%
  rename_all(tolower) %>%
  select(-lastupdated)

if (!exists("archived.flows") | update) {
  archived.flows <- read_csv(archived.flows.csv ) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::mutate(gasdate=lubridate::dmy(gasdate), lastchanged=lubridate::dmy_hm(lastchanged)) %>%
  dplyr::rename(facilityid=plantid, facilityname=plantname, locationid=zoneid, locationname=zonename, lastupdated=lastchanged) 
tail(archived.flows ,10)  


facility.types<-current.flows[,c(2,3,4)] %>% 
  group_by(facilityid) %>%
  dplyr::summarise(facilitytype=facilitytype[1])

archived.flows <-  left_join(archived.flows,  facility.types )
state.type<- current.flows[,c(10,12)]%>% group_by(locationid) %>% dplyr::summarise(state=state[1])
state.type.1<-  current.flows[,c(2,10)]%>% group_by(facilityname) %>% dplyr::summarise(state=state[1])
archived.flows <-  left_join(archived.flows,   state.type.1 )
archived.flows <-  left_join(archived.flows,   state.type )
}

if (!exists("flows")| update) flows<-bind_rows(archived.flows, current.flows) 
