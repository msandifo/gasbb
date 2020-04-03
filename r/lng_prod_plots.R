total.prod.month<- total.prod.month %>% mutate(gasdate = ymd(paste0(year, "-", month,"-15")))
tz(total.prod.month$gasdate)<- "Australia/Brisbane"

  
lng.prod.plots  <- list()
lng.prod.names <- list()
  
#------------
# aus.east.coast.exports.domestic
#------------
  
lng.prod.names[[1]] <-"aus.east.coast.exports.domestic"
    
lng.prod.plots[[1]] <- ggplot(total.prod.month %>% subset(state!="NSW") %>%
         mutate(state = factor(state, levels=c(#"NSW",
                                               "SA", "VIC", "QLD"))), #  %>%#subset (gasdate != g.date) , 
  aes(gasdate, supply, fill=state  )) +
  geom_area(position="stack",col=c("grey80" ), size=.1) +
  geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
  geom_line(data=glad.lng.ym  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
  
  # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
  #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
  annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
  annotate("text", x=as.Date("2018-01-01"), y=4100. ,label= "Curtis Island\nLNG Demand Zone", size=2.6, colour="white") +
  annotate("text", x=as.Date("2019-01-01"), y=2700. ,label= "Gladstone\nLNG exports", size=2.6, colour="yellow") +
  scale_fill_manual(
    values =  c(
    #  "lightblue1" ,
      "red2",
      "darkblue",    
      "green4") )+
   scale_y_continuous(  expand = c(0,0))+
  scale_x_date(  expand = c(0,0) )+
  # hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
  theme(legend.position = c(.144,.8),
        legend.background = element_rect(fill=  "white" , size=0),
        #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
        #panel.ontop = T,
        legend.direction = "vertical", 
        legend.title = element_blank())+
  labs(title= "Australian east coast gas market - production", 
      # subtitle="production - TJ/day",
       y= "TJ/day",
       x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
  geom_hline(yintercept = 1:5*1000, size=.15, col= mmt::add.alpha("grey20",.5))+
  geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
             size=.15, col= mmt::add.alpha("grey20",.5))



#------------
#   aus.east.coast.exports.domestic.stack
#------------
  
 
lng.prod.names[[2]] <- "aus.east.coast.exports.domestic.stack" 

lng.prod.plots[[2]]<-   ggplot( total.prod.month %>% subset( state !="NSW") %>%
          mutate(state = factor(state, levels=c(#"NSW", 
                                                "SA", "VIC", "QLD"))) %>%
          subset (gasdate %ni% g.date.1) , aes(gasdate, supply , fill=state))+
  geom_area(position="fill",col=c("grey80" ), size=.1) +
  scale_fill_manual(
    values = c(
   #   "lightblue1" ,
      "red2",
      "darkblue",    
      "green4" ))+
  scale_y_continuous(  expand = c(0,0), labels = scales::percent, breaks= (1:4)*2/10)+
  scale_x_date(  expand = c(0,0) )+
 # hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
  
  theme(legend.position = c(.85,.3),
        legend.background = element_rect(fill= mmt::add.alpha("white", .5), size=0),
        legend.direction = "vertical", legend.title = element_blank())+
  labs(title= "Australian east coast gas market - production", 
    #   subtitle="production - TJ/day",
       y= " ",
       x=NULL, caption="data source from AEMO")+
 # geom_hline(yintercept= c(.3, .7), col="white", linetype=2, size=.1)+
  geom_hline(yintercept = (1:4)*2/10, size=.15, col= mmt::add.alpha("grey80",.5))+
  geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
             size=.15, col= mmt::add.alpha("grey80",.5))

  

#------------
#aus.east.coast.exports.domestic.balance
#------------
  
  
ndays.offset<-10
  
lng.prod.names[[3]] <-"aus.east.coast.exports.domestic.balance"
  
lng.prod.plots[[3]]<-ggplot(total.prod.month.join %>% subset( gasdate< Sys.Date()-days(ndays.offset) & state!="NSW")%>%
               mutate(state = factor(state, levels=c("NSW", "SA", "VIC", "QLD"))) , # %>%
             #subset (gasdate != g.date) , 
             aes(date, adj.supply , fill=state))+
    geom_area(position="stack",col=c("grey80" ), size=.1) +
    geom_area(data= total.prod.month.join %>% subset( gasdate< Sys.Date()-days(ndays.offset))%>%
                subset(state=="QLD"), position="stack",col=c("grey80" ), aes(y= -TJ), size=.1) +
    # geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
    # geom_line(data=glad.lng  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
    # 
    # # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
    # #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
    # annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
      annotate("text", x=as.Date("2018-07-01"), y=-1500. ,label= paste0("Curtis Island\nLNG Demand Zone\n(~LNG exports x ", ( round(parasitic, 2) ) ,")") , size=2.6, colour="white") +
     annotate("text", x=as.Date("2014-01-01"), y=900. ,label= "domestic supply", size=2.6, colour="white") +
     scale_fill_manual(
      values =  c(
    #    "lightblue1" ,
        "red2",
        "darkblue",    
        "green4") )+
    scale_y_continuous(  expand = c(0,0), limits=c(-4300,2300))+
    scale_x_date(  expand = c(0,0) )+
   # hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
    
    theme(legend.position = c(.144,.2),
        # legend.background = element_rect(fill=  "white" , size=0),
          legend.background = element_rect(fill= mmt::add.alpha( "white", .5) , size=0),
          #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
          #panel.ontop = T,
          legend.direction = "vertical", 
          legend.title = element_blank())+
    
    labs(title= "Australian east coast gas market - production balance", 
         # subtitle="production - TJ/day",
         y= "TJ/day",
         x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
    geom_hline(yintercept = -4:3*1000, size=.15, col= mmt::add.alpha("grey20",.5))+
    geom_hline(yintercept = 0, size=.25, col= mmt::add.alpha("white",.3))+
    geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
               size=.15, col= mmt::add.alpha("grey20",.5))

#------------
# aus.east.coast.exports.domestic.balance.0
#------------
 

  lng.prod.names[[4]]<-"aus.east.coast.exports.domestic.balance.0" 
  lng.prod.plots[[4]]<-
    ggplot(total.prod.month.join %>% subset( gasdate< Sys.Date()-days(ndays.offset)  &  state!="NSW")%>%
               mutate(state = factor(state, levels=c("NSW", "SA", "VIC", "QLD"))) , # %>%
             #subset (gasdate != g.date) , 
             aes(date, adj.supply , fill=state))+
    geom_area(position="stack",col=c("grey80" ), size=.1) +
 #   geom_area(data= total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20))%>% subset(state=="QLD"), position="stack",col=c("grey80" ), aes(y= -TJ*parasitic), size=.1) +
    # geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
    # geom_line(data=glad.lng  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
    # 
    # # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
    # #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
    # annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
    annotate("text", x=as.Date("2018-07-01"), y=-1500. ,label= paste0("exports +\n", ( round(parasitic, 3) -1)*100 ,"% load") , size=2.6, colour="white") +
    annotate("text", x=as.Date("2014-01-01"), y=900. ,label= "domestic supply", size=2.6, colour="white") +
    scale_fill_manual(
      values =  c(
       # "lightblue1" ,
        "red2",
        "darkblue",    
        "green4") )+
    scale_y_continuous(  expand = c(0,0), limits=c(0,2500))+
    scale_x_date(  expand = c(0,0) )+
   # hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
    
    theme(legend.position = c(.144,.2),
        #  legend.background = element_rect(fill=  "white" , size=0),
          legend.background = element_rect(fill= mmt::add.alpha( "white", .5) , size=0),
          #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
          #panel.ontop = T,
          legend.direction = "vertical", 
          legend.title = element_blank())+
    
    labs(title= "Australian east coast gas market - domestic balance", 
         # subtitle="production - TJ/day",
         y= "TJ/day",
         x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
    geom_hline(yintercept = -0:3*1000, size=.15, col= mmt::add.alpha("grey20",.5))+
    geom_hline(yintercept = 0, size=.25, col= mmt::add.alpha("white",.3))+
    geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
               size=.15, col= mmt::add.alpha("grey20",.5))


#------------
# aus.east.coast.exports.domestic.balance.8
#------------

lng.prod.names[[5]] <- "aus.east.coast.exports.domestic.balance.8"  
lng.prod.plots[[5]] <- ggplot(total.prod.month.join %>% subset( gasdate< Sys.Date()-days(ndays.offset)  &  state!="NSW")%>%
                mutate(state = factor(state, levels=c("NSW", "SA", "VIC", "QLD"))) , # %>%
              #subset (gasdate != g.date) , 
              aes(date, adj.supply.force.maj , fill=state))+
    geom_area(position="stack",col=c("grey80" ), size=.1) +
    #   geom_area(data= total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20))%>% subset(state=="QLD"), position="stack",col=c("grey80" ), aes(y= -TJ*parasitic), size=.1) +
    # geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
    # geom_line(data=glad.lng  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
    # 
    # # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
    # #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
    # annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
    annotate("text", x=as.Date("2018-07-01"), y=-1500. ,label= paste0("exports +\n", ( round(parasitic, 3) -1)*100 ,"% load") , size=2.6, colour="white") +
    annotate("text", x=as.Date("2014-01-01"), y=970. ,label= "domestic supply", size=2.6, colour="white") +
    scale_fill_manual(
      values =  c(
        # "lightblue1" ,
        "red2",
        "darkblue",    
        "green4") )+
    scale_y_continuous(  expand = c(0,0), limits=c(0,2500))+
    scale_x_date(  expand = c(0,0) )+
   # hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
    
    theme(legend.position = c(.144,.2),
          legend.background = element_rect(fill= mmt::add.alpha( "white", .5) , size=0),
          #legend.background = element_rect(fill=  "white" , size=0),
          #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
          #panel.ontop = T,
          legend.direction = "vertical", 
          legend.title = element_blank())+
    
    labs(title= paste0("Australian east coast gas market - domestic balance with ",round(( reservation)*100,0),"% reservation"), 
         # subtitle="production - TJ/day",
         y= "TJ/day",
         x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
    geom_hline(yintercept = -0:3*1000, size=.15, col= mmt::add.alpha("grey20",.5))+
    geom_hline(yintercept = 0, size=.25, col= mmt::add.alpha("white",.3))+
    geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
               size=.15, col= mmt::add.alpha("grey20",.5))


#------------
# aus.east.coast.exports.domestic.balance.0.stack
#------------

lng.prod.names[[6]] <-"aus.east.coast.exports.domestic.balance.0.stack" 
lng.prod.plots[[6]] <-ggplot(total.prod.month.join %>% subset( state !="NSW" &gasdate< Sys.Date()-days(ndays.offset))%>%
                               mutate(state = factor(state, levels=c(#"NSW", 
                                                     "SA", "VIC", "QLD"))) , # %>%
             #subset (gasdate != g.date) , 
             aes(date, adj.supply , fill=state))+
     geom_area(position="fill",col=c("grey80" ), size=.1) +
     #geom_area(data= total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20))%>% subset(state=="QLD"), position="stack",col=c("grey80" ), aes(y= -TJ*parasitic), size=.1) +
    # # geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
    # # geom_line(data=glad.lng  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
    # # 
    # # # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
    # # #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
    # # annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
    # annotate("text", x=as.Date("2018-07-01"), y=-1500. ,label= paste0("exports +\n", ( round(parasitic, 3) -1)*100 ,"% load") , size=2.6, colour="white") +
    # annotate("text", x=as.Date("2014-01-01"), y=900. ,label= "domestic supply", size=2.6, colour="white") +
     scale_fill_manual(
      values =  c(
       # "lightblue1" ,
        "red2",
        "darkblue",    
        "green4") )+
    scale_y_continuous(  expand = c(0,0),  labels = scales::percent, breaks= (1:4)*2/10)+
    scale_x_date(  expand = c(0,0) )+
   # hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
    
    theme(legend.position = c(.16,.59),
          legend.background = element_rect(fill= mmt::add.alpha( "white", .5) , size=0),
          #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
          #panel.ontop = T,
          legend.direction = "vertical", 
          legend.title = element_blank())+
 
    labs(title= "Australian east coast gas market - domestic supply balance", 
         # subtitle="production - TJ/day",
         y= " ",
         x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
    geom_hline(yintercept = (1:4)*2/10, size=.15, col= mmt::add.alpha("grey20",.5))+
    #geom_hline(yintercept = 1:4, size=.25, col= mmt::add.alpha("white",.3))+
    geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
               size=.15, col= mmt::add.alpha("grey20",.5))

#------------
# 
#------------
# 
# aus.east.coast.exports.domestic.balance.8 <-
# ggplot(total.prod.month.join %>% subset( gasdate< Sys.Date()-days(ndays.offset)  &  state!="NSW")%>%
#                 mutate(state = factor(state, levels=c("NSW", "SA", "VIC", "QLD"))) , # %>%
#               #subset (gasdate != g.date) , 
#               aes(date, adj.supply.force.maj , fill=state))+
#     geom_area(position="stack",col=c("grey80" ), size=.1) +
#     #   geom_area(data= total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20))%>% subset(state=="QLD"), position="stack",col=c("grey80" ), aes(y= -TJ*parasitic), size=.1) +
#     # geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
#     # geom_line(data=glad.lng  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
#     # 
#     # # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
#     # #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
#     # annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
#     annotate("text", x=as.Date("2018-07-01"), y=-1500. ,label= paste0("exports +\n", ( round(parasitic, 3) -1)*100 ,"% load") , size=2.6, colour="white") +
#     annotate("text", x=as.Date("2014-01-01"), y=970. ,label= "domestic supply", size=2.6, colour="white") +
#     scale_fill_manual(
#       values =  c(
#         # "lightblue1" ,
#         "red2",
#         "darkblue",    
#         "green4") )+
#     scale_y_continuous(  expand = c(0,0), limits=c(0,2500))+
#     scale_x_date(  expand = c(0,0) )+
#    # hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
#     
#     theme(legend.position = c(.144,.2),
#           legend.background = element_rect(fill= mmt::add.alpha( "white", .5) , size=0),
#           #legend.background = element_rect(fill=  "white" , size=0),
#           #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
#           #panel.ontop = T,
#           legend.direction = "vertical", 
#           legend.title = element_blank())+
#     
#     labs(title= paste0("Australian east coast gas market - domestic balance with ",round(( reservation)*100,0),"% reservation"), 
#          # subtitle="production - TJ/day",
#          y= "TJ/day",
#          x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
#     geom_hline(yintercept = -0:3*1000, size=.15, col= mmt::add.alpha("grey20",.5))+
#     geom_hline(yintercept = 0, size=.25, col= mmt::add.alpha("white",.3))+
#     geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
#                size=.15, col= mmt::add.alpha("grey20",.5))
#  
# aus.east.coast.exports.domestic.balance.0.stack.8
# 
#  

#--------------------
# aus.east.coast.exports.domestic.balance.8.stack
#--------------------

lng.prod.names[[7]] <-"aus.east.coast.exports.domestic.balance.8.stack"

lng.prod.plots[[7]] <-  ggplot(total.prod.month.join %>% subset( state !="NSW" &gasdate< Sys.Date()-days(ndays.offset))%>%
               mutate(state = factor(state, levels=c(#"NSW", 
                 "SA", "VIC", "QLD"))) , # %>%
             #subset (gasdate != g.date) , 
             aes(date, adj.supply.force.maj , fill=state))+
    geom_area(position="fill",col=c("grey80" ), size=.1) +
    #geom_area(data= total.prod.month.join %>% subset( gasdate< Sys.Date()-days(20))%>% subset(state=="QLD"), position="stack",col=c("grey80" ), aes(y= -TJ*parasitic), size=.1) +
    # # geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
    # # geom_line(data=glad.lng  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
    # # 
    # # # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
    # # #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
    # # annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
    # annotate("text", x=as.Date("2018-07-01"), y=-1500. ,label= paste0("exports +\n", ( round(parasitic, 3) -1)*100 ,"% load") , size=2.6, colour="white") +
    # annotate("text", x=as.Date("2014-01-01"), y=900. ,label= "domestic supply", size=2.6, colour="white") +
    scale_fill_manual(
      values =  c(
        # "lightblue1" ,
        "red2",
        "darkblue",    
        "green4") )+
    scale_y_continuous(  expand = c(0,0),  labels = scales::percent, breaks= (1:4)*2/10)+
    scale_x_date(  expand = c(0,0) )+
   # hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.0)  )+
    
    theme(legend.position = c(.16,.59),
          legend.background = element_rect(fill= mmt::add.alpha( "white", .5) , size=0),
          #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
          #panel.ontop = T,
          legend.direction = "vertical", 
          legend.title = element_blank())+
    
    labs(title= paste0("Australian east coast gas market - domestic balance with ",round(( reservation)*100,0),"% reservation"), 
         # subtitle="production - TJ/day",
         y= " ",
         x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")+
    geom_hline(yintercept = (1:4)*2/10, size=.15, col= mmt::add.alpha("grey20",.5))+
    #geom_hline(yintercept = 1:4, size=.25, col= mmt::add.alpha("white",.3))+
    geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
               size=.15, col= mmt::add.alpha("grey20",.5))
 

#----------------
#  aus.east.coast.exports.domestic.reservation.percentage
#----------------

 f <-function(reservation, df=df) {
   df$adj.supply.force.maj[df$state=="QLD"] <- df$supply[df$state=="QLD"] -
     df$TJ[df$state=="QLD"]*(1-reservation) 
   
   df %>% dplyr::group_by(year, month)  %>%
     dplyr::summarise(adj.supply.force.maj= sum(adj.supply.force.maj)) %>%
     dplyr::group_by(year) %>%
     dplyr::summarise(adj.supply.force.maj= mean(adj.supply.force.maj)) %>% 
     mutate(reservation= paste0(reservation*100, "%"))%>% 
     head(-1)  %>% tail(-2)
 }
 
f <-function(reservation, df=df) {
  df$adj.supply.force.maj[df$state=="QLD"] <- df$supply[df$state=="QLD"] -
    df$TJ[df$state=="QLD"]*(1-reservation) 
  
  df %>% dplyr::group_by(year, month)  %>%
    dplyr::summarise(supply = sum(supply), adj.supply.force.maj= sum(adj.supply.force.maj), max=max(supply), min=min(supply)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(supply = mean(supply),adj.supply.force.maj= mean(adj.supply.force.maj), m.max=max(max), m.min=min(min)) %>% 
    mutate(reservation= paste0(reservation*100, "%"))%>% 
    head(-1)  %>% tail(-2)
}

m.av <- total.prod.month %>%  dplyr::ungroup()%>%
  dplyr::group_by(year, month) %>% 
  dplyr::summarise(supply=sum(supply),  demand=sum(demand), gasdate=mean(gasdate)) %>% 
  mutate(reservation="0%")
 
w.av <- total.prod.week %>%  dplyr::ungroup()%>%
  dplyr::group_by(year, week) %>% 
  dplyr::summarise(supply=sum(supply), max=max(supply), min=min(supply), demand=sum(demand), gasdate=mean(gasdate))%>% 
  mutate(reservation="0%")

y.av <-purrr::map_df(seq(0,0.12,0.04), f, df=total.prod.month.join)
 y.7 <-purrr::map_df(.065, f, df=total.prod.month.join)
# y.av$reservation[y.av$reservation =="0%"] <- "actual"

lng.prod.names[[8]] <- "aus.east.coast.exports.domestic.reservation"
(lng.prod.plots[[8]] <- ggplot(y.av %>% subset(reservation !="0%" & year>=2014), 
                aes(year+ .5,adj.supply.force.maj, col=reservation ))+
    geom_line(data= m.av, aes(x=year+(month-.5)/12, y= supply), colour="Grey60", size=.1)+
   geom_line()+
   geom_line(data=y.av %>% subset(reservation =="0%"), col="black")+
   #geom_line(data=y.7, col="black")+
   geom_point(size=2, col="white")+
   geom_point(size=1.3 )+
  #geom_point(data=y.av %>% subset(reservation =="0%"), col="black",size=1.3)+
 # hrbrthemes::theme_ipsum()+
    scale_y_continuous(sec.axis = sec_axis(~./mean( y.av$adj.supply.force.maj[1:4]),
                                           
                                           labels= scales::label_percent()), lim=c(1450,2300))+
    scale_x_continuous(breaks= seq(2010,2019,2), lim=c(2009.5, 2020.5))+
    geom_hline(yintercept = mean(y.av$adj.supply.force.maj[1:4])*c(.9,1,1.1), linetype=2,size=.2)+
    theme(legend.position=c(.15, .2), legend.background = element_rect(colour="white"))+
   labs(title= "Australian east coast gas market - domestic supply balance", 
        # subtitle="production - TJ/day",
        y= "TJ/day",
        x=NULL, caption="data sourced from AEMO")+
     annotate("text", y=1830,x=2012, label="no reservation - actual")+
 annotate("text", y=mean(y.av$adj.supply.force.maj[1:4])-10,x=2012, label="2010-2014 average", size= 2)
)
   
 
 
 lng.prod.names[[9]] <-"aus.east.coast.exports.domestic.reservation.percentage" 
 lng.prod.plots[[9]] <-ggplot(y.av %>% subset(reservation !="0%"& year>=2014), aes(year,adj.supply.force.maj/ mean( y.av$adj.supply.force.maj[1:4]) , col=reservation ))+
  geom_line()+
  geom_line(data=y.av %>% subset(reservation =="0%"), col="black")+
  #geom_line(data=y.7, col="black")+
  geom_point(size=2, col="white")+
  geom_point(size=1.3 )+
 # hrbrthemes::theme_ipsum()+
   scale_y_continuous(labels=scales::percent)+
   scale_x_continuous(breaks= seq(2010,2019,2))+
   
   theme(legend.position=c(.15, .2))+
   labs(title= "Australian east coast gas market -  % 2010-2014 average", 
        # subtitle="production - TJ/day",
        y= " ",
        x=NULL, caption="data sourced from AEMO and Gladstone Port Authority")
 
 #aus.east.coast.exports.domestic.reservation.percentage
 
#------------
# qld gas production
#------------

 
lng.prod.names[[10]] <-"qld.prod" 
 lng.prod.plots[[10]] <-ggplot(total.prod.month %>% subset(state=="QLD") %>%
                  mutate(state = factor(state, levels=c(#"NSW",
                    "SA", "VIC", "QLD"))) , # %>%
                #subset (gasdate != g.date) , 
                aes(gasdate, supply , fill=state))+
    geom_area(position="stack",col=c("grey80" ), size=.1) +
    geom_line(data=curtis.month, aes(y=adj.supply),colour="white", alpha=1,size=.55)+
    geom_line(data=curtis.month, aes(y=adj.supply),colour="grey20", size=.15, alpha=1)+
    geom_line(data=glad.lng.ym  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
    
    # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
    #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
    annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
    annotate("text", x=as.Date("2018-01-01"), y=4000. ,label= "Curtis Island\ndemand", size=2.6, colour="grey20") +
    annotate("text", x=as.Date("2019-04-01"), y=2650. ,label= "Curtis Island\nLNG exports", size=2.6, colour="yellow") +
    scale_fill_manual(
      values =  c(
        
        "green4") )+
    scale_y_continuous(  expand = c(0,0) )+
    scale_x_date(  expand = c(0,0) )+
    scale_x_date(limits=c(ymd("2009-01-01"),Sys.Date()))+
   # hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey20",.25)  )+
    
    theme(legend.position = "None",#c(.144,.8),
          legend.background = element_rect(fill=  "white" , size=0),
          #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
          #panel.ontop = T,
          legend.direction = "vertical", 
          legend.title = element_blank())+
    
    labs(title= "Queensland gas production", 
         # subtitle="production - TJ/day",
         y= "TJ/day",
         x=NULL, caption="data sourced from AEMO and Gladstone Port Authority") #+
  # geom_hline(yintercept = c(500,400), size=.15, col= mmt::add.alpha("grey20",.5))+
  # geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
  #            size=.15, col= mmt::add.alpha("grey20",.5))
 
 #------------
 # vic.prod
 #------------
 

 total.prod.year <- total.prod.month %>% 
   group_by(year, state) %>%
   summarise(supply = mean(supply), 
             gasdate=mean(gasdate))  
 
 
lng.prod.names[[11]] <-"vic.prod" 
lng.prod.plots[[11]] <-ggplot(total.prod.month %>% subset(state=="VIC") %>%
                               mutate(state = factor(state, levels=c(#"NSW",
                    "SA", "VIC", "QLD"))) , # %>%
                #subset (gasdate != g.date) , 
                aes(gasdate, supply , fill=state))+
     geom_area(position="stack",col=c("grey80" ), size=.1) +
     geom_smooth( aes(group=year), method="lm", formula=y~1, se=F, col=mmt::add.alpha("white", .5), size=1.3)+
     geom_smooth( aes(group=year), method="lm", formula=y~1, se=F, col="black", size=.35)+
     # geom_line(data=curtis.month, aes(y=adj.supply),colour="white", size=.25)+
     # geom_line(data=glad.lng.ym  %>% mutate(state="QLD"), aes(x=date, y=TJ),colour="yellow", size=.25)+
     # 
     # annotate( "line", x=c(as.Date("2015-01-01"), as.Date("2015-10-01")),y=c(150,1400),colour="white", size=.25,
     #           arrow=arrow(angle = 20, length = unit(0.1, "inches"), ends = "last", type="closed" ), linetype=1)+
     annotate( "point", x=as.Date("2014-12-15"),y=0,colour="yellow", size=1. )+
     annotate("text", x=as.Date("2018-01-01"), y=4000. ,label= "Gladstone\ndemand", size=2.6, colour="white") +
     annotate("text", x=as.Date("2019-01-01"), y=2700. ,label= "Gladstone\nLNG exports", size=2.6, colour="yellow") +
     scale_fill_manual(
       values =  c(
         
         "blue3") )+
     coord_cartesian(ylim = c(300,1500))+
     scale_y_continuous(  expand = c(0,0))+
     scale_x_date(  expand = c(0,0) )+
   scale_x_date(limits=c(ymd("2009-01-01"),Sys.Date()))+
  ## hrbrthemes::theme_ipsum(grid_col=mmt::add.alpha("grey50",.35)  )+
     
     theme(legend.position = "None", # c(.144,.2),
           legend.background = element_rect(fill=  "white" , size=0),
           #panel.grid = element_line(size=.05, colour= mmt::add.alpha("grey50",.1)),
           #panel.ontop = T,
           legend.direction = "vertical", 
           legend.title = element_blank())+
     
     labs(title= "Victorian gas production", 
          # subtitle="production - TJ/day",
          y= "TJ/day",
          x=NULL, caption="data sourced from AEMO") +
     #   geom_hline(yintercept = c(500,400), size=.15, col= mmt::add.alpha("grey20",.5))+
     geom_vline(xintercept = seq(as.Date("2009/1/1"), as.Date("2020/1/1"), "years"), 
                size=.15, col= mmt::add.alpha("grey20",.5))
 
 
 #------------
 # save 
 #------------
 
if(!exists("plot.type")) plot.type <- "png"

if (do_save){
  for (i in 1:length(lng.prod.plots)){
    gcsave(paste0("figs/", lng.prod.names[[i]],".", plot.type), 
           lng.prod.plots[[i]] ,   
           width=width, 
           height=height )
  }
}
 
