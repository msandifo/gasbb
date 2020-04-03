
af.reorder <-tidyr::pivot_wider(archived.flows,names_from =flowdirection,   values_from = actualquantity, values_fill=list(DELIVERY=0, RECEIPT=0) )%>% 
  dplyr::rename( supply=DELIVERY, demand=RECEIPT) %>%
  dplyr::select(gasdate,facilityname ,      facilityid ,  facilitytype, demand, supply , locationname  , locationid  , state )
cf.reorder <-current.flows %>%
  dplyr::select(gasdate,facilityname ,      facilityid , facilitytype, demand, supply , locationname  , locationid  , state )

actual.flows <- bind_rows(af.reorder, cf.reorder) %>% subset(  !is.na(gasdate)) %>% subset(state %ni% c("NT" , "TAS") )
#tail(actual.flows)
actual.flows$state %>% unique()
actual.flows$facilityname[is.na(actual.flows $facilitytype)] %>% unique()

actual.flows$facilityname[!is.na(actual.flows $facilitytype)] %>% unique()
actual.flows$facilitytype[actual.flows$facilityname %in% c("Ballera Gas Plant" ,"Wungoona" )] <-"PROD"
actual.flows$facilitytype[actual.flows$facilityname %in% c("Chookoo Storage Ballera" ) ] <-"STOR"
actual.flows$facilitytype[actual.flows$facilityname %in% c("Longford to Melbourne" ,    "NSW-Victoria Interconnect", "South West Pipeline","Spring Gully Pipeline" )  ] <-"PIPE"

actual.flows$facilityname[is.na(actual.flows$state)] %>% unique()
actual.flows$state[actual.flows$facilityname %in% c("Ballera Gas Plant", "Berwyndale South" )  ] <-"QLD"
actual.flows[actual.flows$state=="NSW" & !is.na(actual.flows$state),]

#---------

curtis.day <-actual.flows   %>%  
  subset(facilitytype=="PIPE" & state=="QLD" & 
           (str_detect(locationname, "Curtis Island" ) 
            # str_detect(facilityname, "Wallumbilla to Gladstone Pipeline" )|
            #   str_detect(facilityname, "GLNG Gas Transmission Pipeline" )
            # str_detect(facilityname,"APLNG Pipeline")
           )) %>% 
  group_by(gasdate, locationname ) %>% 
  dplyr::summarise(state="QLD", demand= sum(demand), supply=sum(supply ), adj.supply =max(demand,supply, na.rm=T), facilityname=facilityname[1]) %>%
  dplyr::arrange(gasdate)  %>% subset(adj.supply>0)

curtis.month <- curtis.day %>% 
  mutate(year=lubridate::year(gasdate), month=lubridate::month(gasdate)) %>%
  group_by(year, month, state) %>%
  dplyr::summarise(demand= mean(demand, na.rm=T) , supply=mean(supply, na.rm=T), adj.supply=mean(adj.supply, na.rm=T), gasdate=mean(gasdate)) 

curtis.month$gasdate = lubridate::ymd(paste0( curtis.month$year,"-", curtis.month$month,"-15"))

#-------


parasitic <-mean(head(curtis.month[curtis.month$gasdate > ymd("2016-01-01"),]$adj.supply, -1)/glad.lng[glad.lng$date> ymd("2016-01-01"),]$TJ)

glad.lng.ym <- glad.lng %>%
  dplyr::mutate(month=month(date), year=year(date))  %>%
  select(year, month, TJ, date)
#smoother
les <-(length(glad.lng.ym$TJ)-1)
glad.lng.ym$TJ[2:les] = glad.lng.ym$TJ[2:les -1]/6 +glad.lng.ym$TJ[2:les]*4/6 +glad.lng.ym$TJ[2:les +1]/6
glad.lng.ym$TJ[les+1] = glad.lng.ym$TJ[les]*3/4 +glad.lng.ym$TJ[les+1]*1/4


#---

curtis.month.2015 <- tibble( year= glad.lng.ym$year[2:10], 
                             month= glad.lng.ym$month[2:10], 
                             state="QLD", 
                             demand=NA, 
                             supply=glad.lng.ym$TJ[2:10], 
                             adj.supply=glad.lng.ym$TJ[2:10]*parasitic,
                             gasdate=glad.lng.ym$date[2:10] )

curtis.month<- bind_rows(curtis.month.2015, curtis.month)


# actual.flows   %>%  
#   subset(facilitytype=="PROD") %>% 
#   group_by(gasdate) %>% 
#   dplyr::summarise(demand= sum(demand ), supply=sum(supply )) %>% 
#   subset(supply<15000) %>% 
#   ggplot(aes(gasdate, supply ))+
#   geom_line()  

#---

total.prod.day <-actual.flows   %>% 
  subset(facilitytype=="PROD" & state!="NT")%>%   subset(supply<15000) %>% 
  group_by(gasdate, state)  %>%
  dplyr::summarise(demand= sum(demand, na.rm=T),supply=sum(supply, na.rm=T))

#ggplot(total.prod.day  , aes(gasdate, supply , fill=state))+geom_area() 

total.prod.week<- total.prod.day %>% 
  mutate(year=lubridate::year(gasdate), week=lubridate::week(gasdate)) %>%
  group_by(year, week, state) %>%
  dplyr::summarise(demand= mean(demand, na.rm=T) , supply=mean(supply, na.rm=T), gasdate=mean(gasdate)) 


total.prod.month <- total.prod.day %>% 
  mutate(year=lubridate::year(gasdate), month=lubridate::month(gasdate)) %>%
  group_by(year, month, state) %>%
  dplyr::summarise(demand= mean(demand, na.rm=T) , supply=mean(supply, na.rm=T), gasdate=mean(gasdate)) 

# ind <-  which( total.prod.month$state=="VIC" &  total.prod.month$supply >1150) 
# 
# total.prod.month[total.prod.month$state=="NSW",]%>% dplyr::arrange( desc(supply))
ind <-  which( total.prod.month$state=="NSW" &  total.prod.month$supply >100) 
total.prod.month$supply[ind] <-16
g.date <- c(total.prod.week$gasdate[ind] , total.prod.week$gasdate[1066]) 

parasitic <-mean(head(curtis.month[curtis.month$gasdate > ymd("2016-01-01"),]$adj.supply, -1)/glad.lng[glad.lng$date> ymd("2016-01-01"),]$TJ)

#total.prod.month.join <-left_join(total.prod.month , glad.lng.ym )  
total.prod.month.join <-left_join(total.prod.month , curtis.month[, c("gasdate","adj.supply")]    %>% dplyr::rename(TJ= adj.supply))
tail(curtis.month)
tail(total.prod.month.join %>% subset(state=="QLD"),20)
total.prod.month.join$TJ[is.na(total.prod.month.join$TJ)] <-0
total.prod.month.join$adj.supply <- total.prod.month.join$supply
# total.prod.month.join$adj.supply[total.prod.month.join$state=="QLD"] <- total.prod.month.join$supply[total.prod.month.join$state=="QLD"] -
#   total.prod.month.join$TJ[total.prod.month.join$state=="QLD"]* parasitic
total.prod.month.join$adj.supply[total.prod.month.join$state=="QLD"] <- total.prod.month.join$supply[total.prod.month.join$state=="QLD"] -
  total.prod.month.join$TJ[total.prod.month.join$state=="QLD"] #* parasitic

#total.prod.month.join$adj.supply[total.prod.month.join$adj.supply<=0] =1


dd= total.prod.month.join %>% subset(state=="NSW") #%>% arrange((adj.supply)) 
total.prod.month.join$adj.supply[total.prod.month.join$adj.supply>100 & total.prod.month.join$state=="NSW"] <-0
total.prod.month.join$adj.supply[total.prod.month.join$adj.supply<1 & total.prod.month.join$state=="QLD"] <-1
total.prod.month.join$adj.supply[is.na(total.prod.month.join$adj.supply)& total.prod.month.join$state=="NSW"] <-1
total.prod.month.join$date <- ymd(paste0(total.prod.month.join$year,"-", total.prod.month.join$month, "-", 15))

total.prod.year <- total.prod.month %>% 
  group_by(year, state) %>%
  summarise(supply = mean(supply), 
            gasdate=mean(gasdate))  


g.date.1 <- c(total.prod.week$gasdate[ind] , total.prod.week$gasdate[311], total.prod.week$gasdate[1071]) 
#g.date <- as.Date(c("2010-06-14", "2010-06-07", "2014-01-05"))
total.prod.month$supply[total.prod.month$supply>100 & total.prod.month$state=="NSW"] <-0



total.prod.month.1 <- total.prod.day %>% 
  mutate(year=lubridate::year(gasdate), month=lubridate::month(gasdate)) %>%
  group_by(year, month, state) %>%
  dplyr::summarise(demand= mean(demand, na.rm=T) , supply=mean(supply, na.rm=T), gasdate=mean(ymd(paste(year,"-",month,"-15")))) 

library(lubridate)
# from ratio of infliws and exports
parasitic <-mean(head(curtis.month[curtis.month$gasdate > ymd("2016-01-01"),]$adj.supply, -1)/glad.lng[glad.lng$date> ymd("2016-01-01"),]$TJ)

#total.prod.month.join <-left_join(total.prod.month , glad.lng.ym )  
total.prod.month.join <-left_join(total.prod.month.1 , curtis.month[, c("gasdate","adj.supply")]    %>% dplyr::rename(TJ= adj.supply))
tail(curtis.month)
tail(total.prod.month.join %>% subset(state=="QLD"),20)
total.prod.month.join$TJ[is.na(total.prod.month.join$TJ)] <-0
total.prod.month.join$adj.supply <- total.prod.month.join$supply
# total.prod.month.join$adj.supply[total.prod.month.join$state=="QLD"] <- total.prod.month.join$supply[total.prod.month.join$state=="QLD"] -
#   total.prod.month.join$TJ[total.prod.month.join$state=="QLD"]* parasitic
total.prod.month.join$adj.supply[total.prod.month.join$state=="QLD"] <- total.prod.month.join$supply[total.prod.month.join$state=="QLD"] -
  total.prod.month.join$TJ[total.prod.month.join$state=="QLD"] #* parasitic

#total.prod.month.join$adj.supply[total.prod.month.join$adj.supply<=0] =1


dd= total.prod.month.join %>% subset(state=="NSW") #%>% arrange((adj.supply)) 
total.prod.month.join$adj.supply[total.prod.month.join$adj.supply>100 & total.prod.month.join$state=="NSW"] <-0
total.prod.month.join$adj.supply[total.prod.month.join$adj.supply<1 & total.prod.month.join$state=="QLD"] <-1
total.prod.month.join$adj.supply[is.na(total.prod.month.join$adj.supply)& total.prod.month.join$state=="NSW"] <-1
total.prod.month.join$date <- ymd(paste0(total.prod.month.join$year,"-", total.prod.month.join$month, "-", 15))

reservation <- .08
total.prod.month.join$adj.supply.force.maj <- total.prod.month.join$supply
total.prod.month.join$adj.supply.force.maj[total.prod.month.join$state=="QLD"] <- total.prod.month.join$supply[total.prod.month.join$state=="QLD"] -
  total.prod.month.join$TJ[total.prod.month.join$state=="QLD"]*(1-reservation ) 


