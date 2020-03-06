library(tidyverse)
library(readxl)
facilities.csv <- "/Volumes/data/Dropbox/msandifo/documents/programming/r/2020/gasbb/gasbb/data/current/Facilities_20200207153346.csv" 
facilities <- read_csv(facilities.csv ) %>% 
  dplyr::rename_all(tolower) 


  archived.flows.csv <- "data/archived/ActualFlows.csv"
#archived.flows <- data.table::fread(archived.flows.csv ) %>% dplyr::rename_all(tolower)
archived.flows <- read_csv(archived.flows.csv ) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::mutate(gasdate=lubridate::dmy(gasdate), lastchanged=lubridate::dmy_hm(lastchanged)) %>%
  dplyr::rename(facilityid=plantid, facilityname=plantname, locationid=zoneid, locationname=zonename, lastupdated=lastchanged) %>% 
  
  left_join(facilities[,c(2,3)])%>% 
 # dplyr::select(-facilityname) %>%
  left_join(facilities[,c(1,2)]) %>% 
  dplyr::distinct()
  
  
tail(archived.flows ,10)  


flows.1819.csv<-"data/archived/PipelineConnectionFlow_History.csv"
#flows.1819.csv <- "data/current/Actual Flow and Storage_2018_2019.csv"
flows.1819 <- read_csv(flows.1819.csv  ) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::mutate(gasdate=lubridate::dmy(gasdate) )%>% 
  
  left_join(facilities[,c(2,3)])%>% 
 # dplyr::select(-facilityname) %>%
  left_join(facilities[,c(1,2)]) %>% dplyr::distinct()
tail(flows.1819 %>% subset(state=="QLD" & facilitytype=="PROD") %>% arrange(gasdate),30)
archived.flows<-left_join(archived.flows, flows.1819[,c(2,3,4,9)])
 
state.type <- flows.1819[,c(8,10)]%>% group_by(locationid) %>% dplyr::summarise(state=state[1])

archived.flows <-  left_join(archived.flows,   state.type )%>% 
  dplyr::distinct()
 tail(archived.flows ,10) 
tail(flows.1819)


actual.flows<- rbind(archived.flows %>% 
  dplyr::select(gasdate,    facilityname,   facilitytype,      facilityid, actualquantity, flowdirection, state, locationname ,   locationid , state),#, connectionpointname, connectionpointid  ),
flows.1819 %>%
  dplyr::select( gasdate,    facilityname,   facilitytype,      facilityid, actualquantity, flowdirection, state, locationname ,    locationid ,state)#), connectionpointname, connectionpointid  )
)

tail(actual.flows %>% subset(state=="QLD" & facilitytype=="PROD") %>% arrange(gasdate),30)
head(actual.flows %>% subset(state=="QLD" & facilitytype=="PROD") %>% arrange(gasdate),30)

actual.flows <-actual.flows %>% dplyr::distinct()

actual.flows$facilitytype[actual.flows$locationname == "Ballera (BAL)"] <- "PROD"
actual.flows$facilitytype[actual.flows$locationname == "Roma (ROM)"] <- "PROD"

actual.flows$facilitytype[actual.flows$locationname == "Victorian Principal Transmission System"] <- "STOR"
tail(actual.flows )

gasbb.w <- actual.flows %>%
  group_by(gasdate ,  facilitytype, locationname,locationid,facilityid,facilityname, flowdirection  )%>%
  subset(actualquantity<2000)%>%
  dplyr::summarise( actualquantity= mean(actualquantity, na.rm=T)) %>% arrange(gasdate)

tail(gasbb.w)



gasbb.w[str_detect(gasbb.w$locationname, "Moom") | str_detect(gasbb.w$locationname, "SA"),]$locationid %>% unique()-> moom
gasbb.w[str_detect(gasbb.w$locationname, "Rom") | str_detect(gasbb.w$locationname, "QLD") | str_detect(gasbb.w$locationname, "Bal"),]$locationid %>% unique()-> roma
#gasbb.w[str_detect(gasbb.w$locationname, "Rom")   ,]$locationid %>% unique()-> roma
gasbb.w[str_detect(gasbb.w$locationname, "Gipp") | str_detect(gasbb.w$locationname, "VIC")| str_detect(gasbb.w$locationname, "Port") |str_detect(gasbb.w$locationname, "Vic"),]$locationid %>% unique() ->gipp
# gasbb.w[str_detect(gasbb.w$locationname, "Port"),]$locationid %>% unique() ->port
# gasbb.w[str_detect(gasbb.w$locationname, "Vic"),]$locationid %>% unique() ->bass
gasbb.w[str_detect(gasbb.w$locationname, "Syd") | str_detect(gasbb.w$locationname, "NSW"),]$locationid %>% unique() ->syd
#gasbb.w[str_detect(gasbb.w$locationname, "Bal"),]$locationid %>% unique() ->bal

vic.prod<- gasbb.w %>% subset(locationid %in% gipp    )   %>%
  group_by( facilitytype, flowdirection,gasdate) %>%
  dplyr::summarise(actualquantity=sum(actualquantity), prodzone="Gippsland"  ) %>% dplyr::arrange(gasdate)

sa.prod<- gasbb.w %>% subset(locationid %in% moom   )   %>%
  group_by( facilitytype, flowdirection,gasdate) %>%
  dplyr::summarise(actualquantity=sum(actualquantity), prodzone="Moomba")

qld.prod<- gasbb.w %>% subset(locationid %in% roma  )   %>%
  group_by( facilitytype, flowdirection,gasdate) %>%
  dplyr::summarise(actualquantity=sum(actualquantity), prodzone="Roma")
tail(qld.prod)
# port.prod<- gasbb.w %>% subset(locationid %in% port     )   %>%
#   group_by( facilitytype, flowdirection,gasdate) %>%
#   dplyr::summarise(actualquantity=sum(actualquantity), prodzone="Port Campbell")

# bass.prod<- gasbb.w %>% subset(locationid %in% bass   )   %>%
#   group_by( facilitytype, flowdirection,gasdate) %>%
#   dplyr::summarise(actualquantity=sum(actualquantity), prodzone="Bass")

nsw.prod<- gasbb.w %>% subset(locationid %in% syd   )   %>%
  group_by( facilitytype, flowdirection,gasdate) %>%
  
  dplyr::summarise(actualquantity=sum(actualquantity), prodzone="Sydney")
 
# bal.prod<- gasbb.w %>% subset(locationid %in% bal   )   %>%
#   group_by( facilitytype, flowdirection,gasdate) %>%
#   dplyr::summarise(actualquantity=sum(actualquantity), prodzone="Ballera")

total.prod <-rbind(bass.prod, gipp.prod, moom.prod,roma.prod, port.prod, syd.prod ) %>% arrange(gasdate)
total.prod <-rbind(nsw.prod, qld.prod, vic.prod, sa.prod) %>% arrange(gasdate)
tail(total.prod)
total.prod.month <- total.prod %>% 
  subset(facilitytype=="PROD" & flowdirection=="DELIVERY") %>% 
  mutate(year=lubridate::year(gasdate), month=lubridate::month(gasdate)) %>%
  group_by(year, month, prodzone) %>%
  dplyr::summarise(actualquantity=mean(actualquantity), gasdate=mean(gasdate))
tail(total.prod.month)
ggplot(total.prod.month %>% subset(prodzone=="Roma"), aes(gasdate, actualquantity, fill=prodzone))+
  geom_area()+
  scale_fill_manual(
   
    values = c(
      
      "plum", 
      "darkblue",
      "red2", 
      "grey80", 
      "green4",  
      "lightblue",
      "lightblue4"
    )
  )+theme(legend.position = "bottom")







ggplot(gasbb.w %>% subset(locationid==540027 )  , aes(gasdate, actualquantity, col=factor(facilityid)))+geom_line() 
ggplot(gasbb.w %>% subset(locationid==540027 & facilitytype=="PROD" )  , aes(gasdate, actualquantity, col=factor(facilityname)))+geom_line() +facet_wrap(~facilityname,ncol=3)

ggplot(gasbb.w %>% subset(locationid==540027 & facilitytype=="PIPE" & flowdirection=="RECEIPT" )  , aes(gasdate, actualquantity, col=factor(facilityname)))+geom_line() +facet_wrap(~facilityname,ncol=3)
ggplot(gasbb.w %>% subset(locationid== 540030  & facilitytype=="PIPE" & flowdirection=="DELIVERY" )  , aes(gasdate, actualquantity, col=factor(facilityname)))+geom_line() +facet_wrap(~facilityname,ncol=3)

gasbb.w[str_detect(gasbb.w$locationname, "Curtis"),]$locationid %>% unique()
gasbb.w[str_detect(gasbb.w$locationname, "Gipp"),]$locationid %>% unique() ->longford
ggplot(gasbb.w %>% subset(locationid  %in% longford & facilitytype=="PROD" & flowdirection=="DELIVERY" )  , aes(gasdate, actualquantity, col=factor(facilityname)))+geom_line() +facet_wrap(~facilityname,ncol=3)


gasbb.w[str_detect(gasbb.w$locationname, "Moom"),]$locationid %>% unique()->moo
ggplot(gasbb.w %>% subset(locationid %in% moo & facilitytype=="PROD" & flowdirection=="DELIVERY" )  , aes(gasdate, actualquantity, col=factor(facilityname)))+geom_line() +facet_wrap(~facilityname,ncol=3)


gasbb.w[str_detect(gasbb.w$locationname, "Rom"),]$locationid %>% unique()-> rom
ggplot(gasbb.w %>% subset(locationid %in% rom & facilitytype=="PROD" & flowdirection=="DELIVERY" )    , 
       aes(gasdate, actualquantity, col=factor(facilityname)))+geom_line() +facet_wrap(~facilityname,ncol=3)

ggplot(gasbb.w %>% subset(locationid %in% rom & facilitytype=="PROD" & flowdirection=="DELIVERY" )   %>%
         group_by(gasdate) %>%
         dplyr::summarise(actualquantity=sum(actualquantity)), 
       aes(gasdate, actualquantity))+geom_line()  



tail(total.prod.month)
gasbb.w[str_detect(gasbb.w$locationname, "Port"),]$locationid %>% unique()-> rom
ggplot(gasbb.w %>% subset(locationid %in% rom & facilitytype=="PROD" & flowdirection=="DELIVERY" )    , 
       aes(gasdate, actualquantity, col=factor(facilityname)))+geom_line() +facet_wrap(~facilityname,ncol=3)
