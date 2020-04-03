wall.flow.plots <- list()
wall.flow.names <- list()

# ------------------------
# curtis.demand
# ------------------------

wall.flow.names[[1]] <- "curtis.demand" 
wall.flow.plots[[1]] <-  ggplot(flows.ci.b, aes(gasdate, demand))     +
  geom_line(size=.2, colour = "lightblue3")  +
  geom_smooth(aes(group=month), method="lm", se=F, formula=y~1, col="black", size=.5)+
  #hrbrthemes::theme_ipsum_rc(grid_col = "grey90")+
  # coord_cartesian(ylim=c(0,4050))+
  # geom_hline(yintercept = seq(3200,4000, 200), size=.05, col="grey70")+
  coord_cartesian(ylim=c(1400,4050))+
  labs(y= "TJ/day",
       x = NULL,
       title= "Curtis Island demand",
       subtitle="monthly averages")


# ggplot(flows.ci, aes(gasdate, demand))     +
#   geom_line(size=.2, colour = "lightblue3")  +
#   geom_smooth(aes(group=month), method="lm", se=F, formula=y~1, col="black", size=.5)+
#   #hrbrthemes::theme_ipsum_rc(grid=F)+
#    coord_cartesian(ylim=c(3200,4050))+
#   geom_hline(yintercept = seq(3200,4000, 200), size=.05, col="grey70")+
#   labs(y= "TJ/day",
#        x = NULL,
#        title= "Curtis Island demand",
#        subtitle="monthly averages")
# 





# facility.types<-flows[,c(3,4)] %>% 
#   group_by(facilityid) %>%
#   dplyr::summarise(facilitytype=facilitytype[1])

# archived.flows <-  left_join(arch.flows,  facility.types )
# state.type<- flows[,c(10,12)]%>% group_by(locationid) %>% dplyr::summarise(state=state[1])
# state.type.1<- flows[,c(2,10)]%>% group_by(facilityname) %>% dplyr::summarise(state=state[1])
# archived.flows <-  left_join(archived.flows,   state.type.1 )
# archived.flows <-  left_join(archived.flows,   state.type )


#----------------
# regional.qld.supply
#----------------

wall.flow.names[[2]] <- "regional.qld.supply" 
wall.flow.plots[[2]] <-  ggplot(head(prod.qld.b, -1), aes(gasdate, supply))   +
  geom_line(size=.3, col="lightblue3")  +
  
  geom_smooth(aes(group=month), method="lm", se=F, formula=y~1, col="grey30", size=.5)+
  #geom_hline(yintercept = seq(3400,4200, 200), size=.05, col="grey70")+
  # scale_y_log10()+
  # geom_hline(yintercept =c(100, 250, 500,1000,2500 ), size=.05, col="grey70")+
  #hrbrthemes::theme_ipsum_rc(grid_col="grey95")+
  labs(y= "TJ/day",
       x = NULL,
       title= "Regional - QLD, supply",
       subtitle="monthly averages")


#----------------
# regional.qld.balance
#----------------

#mean prior to production
qld.prod.mean.11.14 <-mean((prod.qld.archived %>% 
                              subset(month>2010 & month<2015))$supply)

prod.qld.b1<-bind_rows(prod.qld.archived %>% subset(gasdate< ymd("2015-01-01"))%>%
                         mutate(balance=supply, q="total prod") %>% 
                         select(gasdate, balance,q,month) , 
                       prod.qld.b %>% mutate(q= "prod- CI demand") %>% 
                         select(gasdate, balance,q,month) )

wall.flow.names[[3]] <- "regional.qld.balance"
wall.flow.plots[[3]] <-  ggplot(head(prod.qld.b1, -1), aes(gasdate, balance))     +
  geom_line(size=.2, aes(col=q))  +
  geom_smooth(aes(group=month), method="lm", se=F, formula=y~1, col="grey30", size=.5)+
  #hrbrthemes::theme_ipsum_rc(grid=T, grid_col=mmt::add.alpha("grey70",.25),)+
  theme(legend.position="None")+
  # geom_hline(yintercept = seq(-200,800, 200), size=.05, col="grey70")+
  geom_hline(yintercept = c(0, qld.prod.mean.11.14), size=.15, col="grey40")+
  annotate("text", 
           x=ymd("2009-06-01"), 
           y=qld.prod.mean.11.14, 
           label= paste(round(qld.prod.mean.11.14,0),"TJ/day\n2011-2014"), 
           size=2.4,
           vjust=.5)+
  annotate("text", 
           x=ymd("2015-05-01"), 
           y=420, 
           label= "No\nCurtis\nIsland\ndemand\ndata", 
           size=2.,
           vjust=.5)+
  
  scale_y_continuous(breaks = seq(-200,800, 200))+
  scale_x_date( date_minor_breaks = "1 year")+
  labs(y= "TJ/day",
       x = NULL,
       title= "Regional - QLD domestic gas supply",
       subtitle="production less Curtis Island demand") + 
  coord_cartesian(ylim=c(-250,900))   
#   geom_line(data=NEM.month, aes(date, VWP*12-250), size=.2, col="grey40")  


#----------------
# swq.moomba.flows
#----------------



# flows.3 <-flows %>% subset(locationname == "Wallumbilla Hub" &
#                              facilitytype=="PIPE") %>%
#    group_by(gasdate) %>%
#   summarise(net= sum(supply+transferin - demand -transferout))  %>%
#   mutate(month= year(gasdate)+(month(gasdate)-.5)/12)
# tail(flows.3)
# 
# ggplot(head(flows.3, -1), aes( gasdate, net))     +
#   geom_line(size=.2)  +
#   geom_smooth(aes(group=month), method="lm", se=F, formula=y~1)+
#   #hrbrthemes::theme_ipsum_rc()+
# 
#   labs(y= "TJ/day",
#        x = NULL,
#        title= "Regional - QLD",
#        subtitle="monthly averages")

ifn <-"South West Queensland Pipeline"
iln <-"Moomba Hub"
flows.4 <-current.flows %>% subset(facilityname == ifn & locationname==iln) %>% 
  # group_by(gasdate) %>%
  #summarise(TransferIn=max(TransferIn), Supply=max(Supply)) %>% 
  mutate(month= year(gasdate)+(month(gasdate)-.5)/12, 
         net= supply+transferin - demand -transferout) %>% 
  dplyr::select(gasdate,   month, net)   


flows.arch.4  <-  archived.flows %>%
  subset(facilityname=="South West Queensland Pipeline" & locationname=="Moomba (MOO)" )  %>% 
  spread(flowdirection, actualquantity)  %>% rename_all(tolower)
flows.arch.4$delivery[is.na(flows.arch.4$delivery)]=0
flows.arch.4$receipt[is.na(flows.arch.4$receipt)]=0

flows.arch.4 <-flows.arch.4 %>%  mutate(month= year(gasdate)+(month(gasdate)-.5)/12, 
                                        net= -(delivery-receipt)) %>% 
  dplyr::select(gasdate,   month, net)  

cflows.4 <-bind_rows(flows.arch.4,flows.4) 

df <- head(cflows.4 %>% subset(gasdate> ymd("2010-01-01")), -1) %>%
  mutate(m=month(gasdate))

wall.flow.names[[4]] <- "swq.moomba.flows"
wall.flow.plots[[4]] <-  ggplot(df, 
       aes( gasdate, net))     +
  geom_line(size=.15, col="pink")  +
  geom_smooth(aes(group=month), method="lm", se=F, formula=y~1, col="grey40")+
  geom_smooth(data=. %>% mutate(year=year(gasdate)),
              aes(group=year ), method="lm", se=F, formula=y~1, col="grey40", size=.4)+
  
  geom_smooth(data= .%>% subset(m==2), aes(group=month), method="lm", se=F, formula=y~1, col="red2")+
  #hrbrthemes::theme_ipsum_rc()+
  scale_x_date(date_minor_breaks="1 year")+
  labs(y= "TJ/day",
       x = NULL,
       title= "SWQ-Moomba",
       subtitle="monthly averages +ve flow into QLD")+
  geom_hline(yintercept = 0, size=.3)+
  coord_cartesian(ylim=c(-350,350))


df.y<- df %>% 
  subset(gasdate>=ymd("2013-01-01") & gasdate<ymd("2020-01-01")) %>% 
  mutate(year=year(gasdate)) %>%
  group_by(year) %>%
  summarise(gasdate=mean(gasdate), net=mean(net))

wall.flow.names[[5]] <- "swq.moomba.flows.annual" 
wall.flow.plots[[5]] <-  ggplot(df %>% subset(gasdate>ymd("2013-01-01")), 
       aes( gasdate, net))     +
  geom_line(size=.15, col="grey70")  +
  # geom_smooth(aes(group=month), method="lm", se=F, formula=y~1, col="grey40")+
  geom_smooth(data=. %>% mutate(year=year(gasdate)),
              aes(group=year ), method="lm", se=F, formula=y~1, col="grey10", size=.3)+
  
  #geom_smooth(data= .%>% subset(m==2), aes(group=month), method="lm", se=F, formula=y~1, col="red2")+
  geom_line(  data=df.y, size=.8, col="grey50")+
  
  geom_point(  data=df.y, size=3, col="white")+
  geom_point(  data=df.y, size=2.3, col="grey30")+
  
  #hrbrthemes::theme_ipsum_rc()+
  scale_x_date(date_minor_breaks="1 year")+
  labs(y= "TJ/day",
       x = NULL,
       title= "SWQ-Moomba",
       subtitle="monthly averages +ve flow into QLD")+
  geom_hline(yintercept = 0, size=.3)+
  coord_cartesian(ylim=c(-350,350))


#----------------
#
#----------------
if(!exists("plot.type")) plot.type <- "png"


if (do_save){
  for (i in 1:length(wall.flow.plots))
    gcsave(paste0("figs/", wall.flow.names[[i]],".", plot.type), 
           wall.flow.plots[[i]],  
           width=width, 
           height=height)
}


# wall.flow.plots <- list(
#   curtis.demand, 
#   regional.qld.supply, 
#   regional.qld.balance,
#   swq.moomba.flows,
#   swq.moomba.flows.annual
# )
# if (do_save){
# 
# ggsave("figs/curtis.demand.png", 
#        plot=curtis.demand, 
#        width=width, height=height)
# ggsave("figs/regional.qld.supply.png", 
#        plot=regional.qld.supply, 
#        width=width, 
#        height=height)
# ggsave("figs/regional.qld.balance.png",
#        plot=regional.qld.balance,
#        width=width, height=height)
# ggsave("figs/swq.moomba.flows.png",
#        plot=swq.moomba.flows,
#        width=width, 
#        height=height)
# ggsave("figs/swq.moomba.flows.annual.png", 
#        plot=swq.moomba.flows.annual,
#        width=width, height=height)
# 
# }
# 
# # names(flows)
# # (flows$FacilityName) %>% unique()
# # (flows$LocationName) %>% unique()
# 
# # (flows %>% subset(  gasdate>"2020-02-20" & 
# #                       locationname=="Moomba Hub" & 
# #                       facilityname == ifn))[,c(1,5,6,7,8,10,11)]
