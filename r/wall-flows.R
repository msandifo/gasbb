flows<-read_csv("~/Dropbox/msandifo/Documents/programming/r/2020/gasbb/data/Actual Flow and Storage_20200229092916.csv") %>%  mutate(GasDate= dmy(GasDate))

flows.1 <-flows %>% subset(LocationName == "Curtis Island" & FacilityType=="PIPE") %>% 
  group_by(GasDate) %>%
   summarise(Demand=sum(Demand)) %>% 
  mutate(month= year(GasDate)+(month(GasDate)-.5)/12)

ggplot(flows.1, aes(GasDate, Demand))     +
  geom_line(size=.2)  +
  geom_smooth(aes(group=month), method="lm", se=F, formula=y~1)+
  hrbrthemes::theme_ipsum_rc()+
  labs(y= "TJ/day",
       x = NULL,
       title= "Curtis Island demand",
       subtitle="monthly averages")
  
  
ggsave("curtis-demand_4.png", width=7, height=4)


flows.2 <-flows %>% subset(LocationName == "Regional - QLD" & FacilityType=="PROD") %>% 
  group_by(GasDate) %>%
  summarise(Supply=sum(Supply)) %>% 
  mutate(month= year(GasDate)+(month(GasDate)-.5)/12)

flows.2$balance <-flows.2$Supply-flows.1$Demand

ggplot(head(flows.2, -1), aes(GasDate, Supply))     +
  geom_line(size=.3, col="pink2")  +
  geom_smooth(aes(group=month), method="lm", se=F, formula=y~1)+
  hrbrthemes::theme_ipsum_rc()+
  labs(y= "TJ/day",
       x = NULL,
       title= "Regional - QLD, supply",
       subtitle="monthly averages")

ggsave("regionalQLD_prod_1.png", width=7, height=4)

ggplot(head(flows.2, -1), aes(GasDate, balance))     +
  geom_line(size=.2)  +
  geom_smooth(aes(group=month), method="lm", se=F, formula=y~1)+
  hrbrthemes::theme_ipsum_rc()+
  
  labs(y= "TJ/day",
       x = NULL,
       title= "Regional - QLD, balance",
       subtitle="monthly averages")

ggsave("regionalQLD_balance_1.png", width=7, height=4)


# flows.3 <-flows %>% subset(LocationName == "Wallumbilla Hub" & FacilityType=="PIPE") %>% 
#    group_by(GasDate) %>%
#   summarise(net= sum(Supply+TransferIn - Demand -TransferOut))  %>% 
#   mutate(month= year(GasDate)+(month(GasDate)-.5)/12)
# tail(flows.3)
# 
# ggplot(head(flows.3, -1), aes( GasDate, net))     +
#   geom_line(size=.2)  +
#   geom_smooth(aes(group=month), method="lm", se=F, formula=y~1)+
#   hrbrthemes::theme_ipsum_rc()+
#   
#   labs(y= "TJ/day",
#        x = NULL,
#        title= "Regional - QLD",
#        subtitle="monthly averages")

ifn <-"South West Queensland Pipeline"

flows.4 <-flows %>% subset(FacilityName == ifn & LocationName=="Moomba Hub") %>% 
   group_by(GasDate) %>%
 #summarise(TransferIn=max(TransferIn), Supply=max(Supply)) %>% 
  mutate(month= year(GasDate)+(month(GasDate)-.5)/12, 
         net= Supply+TransferIn - Demand -TransferOut) %>% 
  dplyr::select(GasDate,   month, net)   

flows.arch <- read_csv("~/Dropbox/msandifo/Documents/programming/r/2020/gasbb/data/ActualFlows.csv") %>%  
  rename(GasDate=GASDATE) %>%
  mutate(GasDate= dmy(GasDate)) 
 
flows.arch.4  <- flows.arch %>%
  subset(PLANTNAME=="South West Queensland Pipeline" & ZONENAME=="Moomba (MOO)" )  %>% 
  spread(FLOWDIRECTION, ACTUALQUANTITY) 
flows.arch.4$DELIVERY[is.na(flows.arch.4$DELIVERY)]=0
flows.arch.4$RECEIPT[is.na(flows.arch.4$RECEIPT)]=0

flows.arch.4 <-flows.arch.4 %>%  mutate(month= year(GasDate)+(month(GasDate)-.5)/12, 
         net= -(DELIVERY-RECEIPT)) %>% 
  dplyr::select(GasDate,   month, net)  
  
cflows.4 <-bind_rows(flows.arch.4,flows.4) 
df <- head(cflows.4 %>% subset(GasDate> ymd("2008-01-01")), -1) %>% mutate(m=month(GasDate))
ggplot(df, 
       aes( GasDate, net))     +
  geom_line(size=.15, col="pink")  +
  geom_smooth(aes(group=month), method="lm", se=F, formula=y~1, col="grey40")+
   geom_smooth(data= .%>% subset(m==2), aes(group=month), method="lm", se=F, formula=y~1, col="red2")+
  hrbrthemes::theme_ipsum_rc()+
  scale_x_date(date_minor_breaks="1 year")+
  labs(y= "TJ/day",
       x = NULL,
       title= "SWQ-Moomba",
       subtitle="monthly averages +ve flow into QLD")+
  geom_hline(yintercept = 0, size=.3)

ggsave("wal-flows_3.png", width=7, height=4)

# names(flows)
# (flows$FacilityName) %>% unique()
# (flows$LocationName) %>% unique()

(flows %>%subset(GasDate>"2020-02-20" & LocationName=="Moomba Hub" &FacilityName == ifn))[,c(1,5,6,7,8,10,11)]

