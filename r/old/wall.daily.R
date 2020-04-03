
#--------------
# data prep

aemo<- read_aemo()
nem.vwp<- nem_vwp(aemo)
gas.bm<-read_wall_bm()

merge.df <-merge(nem.vwp, 
                 gas.bm %>% 
                   rename(date=gasdate) %>% 
                   select(date, benchmark_price))  

merge.df$F <- round(merge.df$benchmark_price,0) %>% as.factor()
merge.df$Y <- "< 2016-05-01"
merge.df$Y[ merge.df$date >= ymd("2016/05/01")]<- "> 2016-05-01"


merge.byweek.df <- merge.df %>% 
  mutate(year=year(date), 
         week =week(date)) %>%
  group_by(year,week) %>%
  summarise(vwp= mean(vwp), 
            benchmark_price=mean( benchmark_price), 
            date=tail(date,1), Y=head(Y,1) ,F=head(F,1))

#--------------
# plots

ggplot(merge.byweek.df %>% subset(date>ymd("2016-01-01")), 
       aes(date, benchmark_price))+
   geom_line(#data=NEM.vwp %>% subset(DATE> ymd("2014-03-01")),
            size=.25, colour="red2", aes(y=vwp/10))+ 
  geom_line(size=.75, colour="white")+ 
  geom_line(size=.25, colour="grey20")+ 
  coord_cartesian(ylim=c(3,17))+
  labs(y= "Wallumbilla gas benchmark - $/GJ",
                 x = NULL,
       title= "Electricity and gas price",
       subtitle="weekly averages")+
 # theme_minimal()+
  hrbrthemes::theme_ipsum_rc(grid = F)+
  geom_hline(yintercept = c(5,10,15), size=.05, linetype=2)+
#  geom_point(data=head(gas.bm,1), col="white",size=2.5)+
  geom_point(data=tail(merge.byweek.df,1), col="white", size=3.5) +
  geom_point(data=tail(merge.byweek.df,1), col="black", size=2.8) +
  geom_point(data=tail(merge.byweek.df,1), aes(y=vwp/10), col="white", size=1.8) +
  geom_point(data=tail(merge.byweek.df,1), aes(y=vwp/10), col="red2", size=1.) +
  scale_y_continuous(sec.axis = sec_axis(~.*10,  name = "NEM volume weighted - $/MWhr" ))+
    theme( axis.text.y.left = element_text( size = rel(1.)),
    axis.text.y.right = element_text(color = "red2", size = rel(1.)),
    axis.title.y.left= element_text(    size = rel(1.) ),
#    axis.line.y.right= element_line(colour="red2"),
    axis.title.y.right= element_text(angle = -90,   color = "red2", size = rel(1.))) 


ggsave("figs/wal-bench_1.png",  width=7, height=4) 

last.week <-tail(merge.byweek.df$week,1)
ggplot(merge.byweek.df, aes(benchmark_price,vwp)) +
  geom_smooth(data= merge.byweek.df %>% subset(benchmark_price<=16 & benchmark_price>.5),
            #   method="glm",
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
  coord_cartesian(ylim=c(20,200), xlim=c(0, 13.5))+
#  geom_violin( aes(group=F) , alpha=0 )+
    labs(x= "Wallumbilla gas benchmark - $/GJ",
       y = "NEM volume weighted - $/MWhr\nlog10 scaling", 
       title= "Electricity spot price v. gas  benchmark price",
       subtitle="weekly averages")+
  # theme_minimal()+
  hrbrthemes::theme_ipsum_rc(grid_col="grey95")+
  scale_color_manual(values = c("blue3", "red3"))+
  theme(legend.position=c(.9, .2), 
        legend.title=element_blank() , 
        legend.background =element_rect(colour="grey30", size=.1)
        )+
  # annotate(geom="text", x=12, y=50, label="$50/MWhr", vjust=1.3, size=3)+
  # annotate(geom="text", x=5, y=170, label="$5/GJ", vjust=-.5, angle=90, size=3)
  scale_y_log10(breaks=c(25,35,50,70,100,150, 200))+
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
  ggrepel::geom_text_repel(data=merge.byweek.df %>% subset(week==last.week ), 
                           aes(col=Y,label=paste0(date,"\n(",round(vwp),"/", round(benchmark_price,1),")")),
                           nudge_y=c(.4, .4,-.4), 
                           nudge_x=c(-.6,.6,.6), 
                           size=2.4,
                           segment.size = .3, show.legend = F)


ggsave("figs/wal-nem.png", width=7, height=4) 

# ggplot(merge.byweek.df  , 
#        aes(benchmark_price,vwp)) +
#   geom_point(size=.3,aes(col=Y)) +
#   ylim(c(25,170))+
#   geom_density_2d(  nlevel=10,  size=.5 , alpha=.6)+
#   theme_minimal()+
#   theme(legend.position=c(.85, .2), 
#         legend.title=element_blank())
#   
#   
#        
# ggplot(merge.df, aes(benchmark_price,vwp)) +
#   geom_point( size=.2, aes(col=Y))+
#   coord_cartesian(ylim=c(25,250))+
#   geom_boxplot( aes(group=F) , alpha=0 )+
#   geom_hline(yintercept=70.50, size=.2)+
#   geom_vline(xintercept = 2, size=.2)+
#   labs(x= "Wallumbilla gas benchmark - $/GJ",
#        y = "NEM max daily - $/MWhr", 
#        subtitle= "max daily  electricity price")+
#   theme_classic()+
#   theme(legend.position=c(.85, .2), 
#         legend.title=element_blank())+
#   scale_y_log10()
#  


# source('~/Dropbox/msandifo/Documents/programming/r/twitter/2018/001/src/functions.R')
# download_aemo_aggregated (year=2010:2019, month=1:12)
# download_aemo_aggregated (year=2020, month=1:2)
# download_aemo_current()
# l.path=validate_directory(NULL) 
# list.files(  l.path)
# build_aemo() ->aemo
# 
# 
# tz(aemo$SETTLEMENTDATE) <-  "Australia/Brisbane"
#  as.Date(aemo$SETTLEMENTDATE)
# names(aemo)
# aemo %>% str()
# aemo %>% mutate(DATE= as.Date(  SETTLEMENTDATE)) %>%
#   group_by( DATE) %>%
#   summarise(vwp = sum(RRP*TOTALDEMAND)/sum(TOTALDEMAND),
#             maxP=max(RRP))  ->
# NEM.vwp


#fn <-"data/PUBLIC_WALLUMBILLABENCHMARKPRICE_20200301.CSV"
#fn <-"http://nemweb.com.au/Reports/Current/GSH/Benchmark_Price/PUBLIC_WALLUMBILLABENCHMARKPRICE_20200218_0000000319248202.zip"
# read_csv( fn  ,skip=1) %>%
# subset(str_detect(PRODUCT_LOCATION, "WAL") & 
#          !str_detect(PRODUCT_LOCATION, "WAL-Non-Netted"))  %>%
# mutate(DATE= as.Date(lubridate::ymd_hms(GAS_DATE, tz = "Australia/Brisbane")) )%>% 
# select(DATE, benchmark_price)
# merge1.df$F <- floor(merge1.df$benchmark_price) %>% as.factor()
# merge1.df$Y <-  "< 2016-05-01"
# merge1.df$Y[ merge1.df$DATE >= ymd("2016/05/01")]<- "< 2016-05-01"


 
