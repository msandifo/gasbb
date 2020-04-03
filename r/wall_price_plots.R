wall.price.plots <- list()
wall.price.names <- list()

 
# ------------------------
# nem.wall.month.time.plot
# ------------------------

wall.price.names[[1]] <- "nem.wall.month.time.plot" 
wall.price.plots[[1]] <-  ggplot(m.df, aes(date, vwp))+
  geom_hline(data=tail(m.df,1), aes(yintercept=vwp), size=.2, linetype=2)+
  geom_hline(data=tail(m.df,1), aes(yintercept=benchmark_price*10), size=.2, linetype=2, col="red2")+
  geom_line(size=.35, col="grey40")+
  geom_line(aes(y=benchmark_price*10),col="white",alpha=.9, size=1)+
  geom_line(aes(y=benchmark_price*10),col="red2", size=.4)+
  #hrbrthemes::theme_ipsum_rc(grid_col="grey90")+
  scale_y_continuous(sec.axis = sec_axis(~./10, 
                                         name = "gas benchmark- $/GJ")  )+
  
  labs(y="electricity volume weighted - $/MWhr", 
       x=NULL,
       title="NEM electricity VWP, gas benchmark - by month",
       caption=paste0("excludes settlement prices > $",rrp.cap, "MWhr, hub =",hub))+
  geom_point(data=tail(m.df,1), col="white", size=2.5) +
  geom_point(data=tail(m.df,1), col="black", size=1.8) +
  geom_point(data=tail(m.df,1), aes(y=benchmark_price*10), col="white", size=1.8) +
  geom_point(data=tail(m.df,1), aes(y=benchmark_price*10), col="red2", size=1.) +
  
  
  theme( 
    axis.text.y.left = element_text( size = rel(1.)),
    axis.text.y.right = element_text(color = "red2", size = rel(1.)),
    axis.title.y.left= element_text(    size = rel(1.) ),
    axis.title.y.right= element_text(angle = -90,   color = "red2", size = rel(1.))) 



# ------------------------
# wall.nem.month.plot
# ------------------------


wall.price.names[[2]] <- "wall.nem.month.plot" 
 wall.price.plots[[2]] <-  ggplot(data=m1.df, aes(x=benchmark_price, y=vwp)) +
  geom_hline(data=m2.df, aes(yintercept =vwp ,  col=month.average), size=.2, linetype=2)+
  geom_vline(data=m2.df, aes(xintercept =benchmark_price , col=month.average), size=.2, linetype=2)+
  geom_smooth( data=m1.df %>% subset(benchmark_price<10),
               aes( col=month.average, fill =month.average),
                method = "lm",
                 se=T,alpha=.15, #col="grey30", 
                 span=.9,size=.1,linetype=2)+
  # geom_smooth(aes(  col=period ),
  #             method = "lm", se=T,alpha=.1,  size=.5,linetype=2)+
  geom_point(size=2., aes(  col=month.average,shape=month.average))+
  geom_point(data=  m1.df %>% subset(month ==month(Sys.Date()-1) & year>=2016), size=5,  col="white", shape=21)+
  geom_point(data=  m1.df %>% subset(month ==month(Sys.Date()-1) & year>=2016), aes(col=month.average), size=4,  shape=21)+
  #hrbrthemes::theme_ipsum_rc(grid_col = "grey90")+
  # geom_density_2d(  data=m1.df %>% subset(benchmark_price<10),
  #                   aes(  col=month.average),
  #                   bins=5,
  #                   size=.1 , alpha=.6#, col="pink4"
  #                     )+
  scale_color_manual(values=c("blue3", "red3"))+
   scale_fill_manual(values=c("blue3", "red3"))+
   labs(    y="electricity volume weighted - $/MWhr", 
           x="gas benchmark - $/GJ",
           title="NEM electricity VWP, gas benchmark - by month",
           caption=paste0("excludes settlement prices > $",rrp.cap, "MWhr, hub =",hub))+
  
  theme( legend.position = c(.11,.85), 
           legend.title = element_blank(),
         legend.background = element_rect(colour="white"),
         axis.text.y = element_text( size = rel(1.2)),
         axis.title.y = element_text(    size = rel(1.2) ),
         axis.text.x = element_text( size = rel(1.)),
         axis.title.x = element_text(    size = rel(1.) ))+
  ggrepel::geom_label_repel(data=   m1.df  %>% subset(month ==month(Sys.Date()-1) & year>=2016), 
                           aes(label= paste0(format(gasdate, "%b %Y"), "\n$",
                                            round(vwp,0),"MWhr/$", round(benchmark_price,1), "GJ")), 
                           segment.size = 0.25,
                           label.size = 0.025,
                           nudge_x = c(-1,1)*2,
                           nudge_y = c(10,-10)*4,
                           box.padding=1,
                           col="blue3",
                           size=3)+
   annotate("text", x=13, y=120, col="blue2", label = get_fits(m1.df %>% subset(benchmark_price<10 & month.average=="2016 on" )), size=3.2) +
#   annotate("text", x=15, y=45, col="red2", label = get_fits(m1.df %>% subset(benchmark_price<10 & month.average!="after 2016" )), size=2.2)+
   coord_cartesian(ylim=c(25,130))
 

# ------------------------
# wall.nem.week.time.plot
# ------------------------

wall.price.names[[3]] <- "wall.nem.week.time.plot" 
wall.price.plots[[3]] <-  ggplot(merge.byweek.df %>% subset(date>ymd("2014-01-01")), 
       aes(date, vwp))+
  geom_hline(data=tail(merge.byweek.df,1), aes(yintercept=vwp), size=.2, linetype=2)+
  geom_line(size=.25, colour="grey20")+ 
  geom_line(size=.75, colour="white", aes(y=benchmark_price*10))+ 
  geom_line(size=.25,  colour="red2", aes(y=benchmark_price*10))+ 
  coord_cartesian(ylim=c(0,160))+
  labs(y= "electricity volume weighted - $/MWhr",
       x = NULL,
       title= "NEM electricity VWP, gas benchmark - by week",
         caption=paste0("excludes settlement prices > $",rrp.cap, "MWhr, hub =",hub))+
  # theme_minimal()+
  #hrbrthemes::theme_ipsum_rc(grid = F)+
  #geom_hline(yintercept = c(5,10,15)*10, size=.05, linetype=2)+
  #  geom_point(data=head(gas.bm,1), col="white",size=2.5)+
  geom_point(data=tail(merge.byweek.df,1), col="white", size=2.5) +
  geom_point(data=tail(merge.byweek.df,1), col="black", size=1.8) +
  geom_point(data=tail(merge.byweek.df,1), aes(y=benchmark_price*10 ), col="white", size=1.8) +
  geom_point(data=tail(merge.byweek.df,1), aes(y=benchmark_price*10), col="red2", size=1.) +
  scale_y_continuous(sec.axis = sec_axis(~./10,  name = "gas benchmark - $/GJ" ))+
  theme( axis.text.y.left = element_text( size = rel(1.)),
         axis.text.y.right = element_text(color = "red2", size = rel(1.)),
         axis.title.y.left= element_text(    size = rel(1.) ),
         #    axis.line.y.right= element_line(colour="red2"),
         axis.title.y.right= element_text(angle = -90,   color = "red2", size = rel(1.)))

# ------------------------
# wall.nem.week.plot
# ------------------------

last.week <-tail(merge.byweek.df$week,1)
weekly.coef <- (lm(data=merge.byweek.df %>% subset(vwp<rrp.cap),  (vwp)~benchmark_price))$coef
weekly.lin.fit =  data.frame(x=seq(0,12, .1), y=seq(0,12, .1)*weekly.coef[2]+weekly.coef[1])
wall.price.names[[4]] <- "wall.nem.week.plot"
wall.price.plots[[4]] <-  ggplot(merge.byweek.df, 
                                 aes(benchmark_price,vwp)) +
  geom_line(data=weekly.lin.fit, aes(x,y), size=.2, col="green4", linetype=2)+
  geom_smooth(data= merge.byweek.df %>% subset(benchmark_price<=16 & benchmark_price>.5),
            #     method="lm",
              size=0, col="grey70", se=T,
              alpha=.3)+
  # geom_hline(yintercept= c(25,50,100,200), size=.05, linetype=2)+
  # geom_vline(xintercept = c(5,10), size=.05, linetype=2)+
  geom_boxplot( data= merge.df %>% subset(benchmark_price<=10.5 & benchmark_price>0.5),aes(group=F) ,
                outlier.size=0,
                size=.2,
                outlier.colour="white",
                colour="grey60",
                alpha=.5, fill="yellow3", notch=T )+
  geom_point(  aes(col=Y),alpha=.7, size=1.25,colour='grey90')+
  geom_point( size=.7, aes(col=Y),alpha=1)+
  coord_cartesian(ylim=c(20,150), xlim=c(0, 13.5))+
  #  geom_violin( aes(group=F) , alpha=0 )+
  labs(x= "gas benchmark - $/GJ",
       y = "electricity volume weighted - $/MWhr\nlog10 scaling",      
       caption=paste0("excludes settlement prices > $",rrp.cap, "MWhr, hub =",hub),
       title= "NEM electricity VWP, gas benchmark - by week")+
  # theme_minimal()+
  #hrbrthemes::theme_ipsum_rc(grid_col="grey95")+
  scale_color_manual(values = c("blue3", "red3"))+
  theme(legend.position=c(.9, .2), 
        legend.title=element_blank() , 
        legend.background =element_rect(colour="grey30", size=.1)
  )+
  # annotate(geom="text", x=12, y=50, label="$50/MWhr", vjust=1.3, size=3)+
  # annotate(geom="text", x=5, y=170, label="$5/GJ", vjust=-.5, angle=90, size=3)
  scale_y_log10(breaks=c(25,35,50,70,100,150, 200))+
  annotate("text", x=2, y=115, label = get_fits(merge.byweek.df %>% subset(date>ymd("2014-01-01")) ), size=2.2, hjust=0)+
  #   geom_point(data=tail(merge.byweek.df,1), size=3.7, col="white", shape=17)+
  # geom_point(data=tail(merge.byweek.df,1), size=2.7, col="black", shape=17)+
  #   ggrepel::geom_text_repel(data=tail(merge.byweek.df,1), aes(label=paste(date)),
  #                            nudge_y=-2, 
  #                            nudge_x=1, 
  #                            size=3,
  #                            segment.size = .3)+
  #   
  #   geom_point(data=tail(merge.byweek.df,53)[1,], size=3.7, col="white", shape=17)+
  #   geom_point(data=tail(merge.byweek.df,53)[1,], size=2.7, col="black", shape=17)+
  #   ggrepel::geom_text_repel(data=tail(merge.byweek.df,53)[1,], aes(label=paste(date)),
#                            nudge_y=-2, 
#                            nudge_x=1, 
#                            size=3,
#                            segment.size = .3)+

geom_point(data= merge.byweek.df %>% subset(week==last.week ), size=3.7, col="white", shape=17)+
  geom_point(data=merge.byweek.df %>% subset(week==last.week ), size=2.7, aes(col=Y) ,shape=17, show.legend = F)+
  ggrepel::geom_label_repel(data=merge.byweek.df %>% subset(week==last.week ), 
                           aes(col=Y,label=paste0(date,"\n(",round(vwp),"/", round(benchmark_price,1),")")),
                           nudge_y=c(.4, .4,-.4), 
                           nudge_x=c(-.6,.6,.6), 
                           label.size = 0,
                           size=2.4,
                           segment.size = .3, show.legend = F)


# ------------------------
# nem.month.time.plot
# ------------------------


wall.price.names[[5]] <- "nem.month.time.plot" 
wall.price.plots[[5]] <-  ggplot(nem.month %>% subset(date>ymd("2010-01-01"))   ,
       aes(date, vwp))+
  geom_line(  size=.35, colour="lightblue3" )+ 
  geom_smooth(aes(group=year), method="lm", se=F, formula=y~1, col="grey30", size=.3)+
  coord_cartesian(ylim=c(30,150))+
  labs(y= "AUD$/MWhr",
       x = NULL,
       title= "NEM spot price",
       subtitle="monthly averages")+
  #hrbrthemes::theme_ipsum_rc(grid_col = "grey90")+
 # geom_hline(yintercept = tail(nem.month$vwp), size=.15, linetype=1)+
  
  geom_hline(yintercept = c(5,10,15)*10, size=.15, linetype=2)+
  #  geom_point(data=head(gas.bm,1), col="white",size=2.5)+
  geom_point(data=tail(merge.byweek.df,1)[1,], col="white", size=3) +
  geom_point(data=tail(merge.byweek.df,1)[1,], col="blue4", size=2.) 


#--------
  
if(!exists("plot.type")) plot.type <- "png"
  
  if (do_save){
    for (i in 1:length(wall.price.plots))
      gcsave(paste0("figs/", wall.price.names[[i]],".", plot.type), 
             wall.price.plots[[i]],  
             width=width*1.3, 
             height=height*1.3)
  } 

