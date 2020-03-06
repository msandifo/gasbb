library(tidyverse)
library(reproscir)
library(lubridate)
        source('~/Dropbox/msandifo/Documents/programming/r/twitter/2018/001/src/functions.R')
download_aemo_aggregated (year=2010:2019, month=1:12)
download_aemo_aggregated (year=2020, month=1:2)
download_aemo_current()
l.path=validate_directory(NULL) 
list.files(  l.path)
build_aemo() ->aemo


tz(aemo$SETTLEMENTDATE) <-  "Australia/Brisbane"
 as.Date(aemo$SETTLEMENTDATE)
names(aemo)
aemo %>% str()
aemo %>% mutate(DATE= as.Date(  SETTLEMENTDATE)) %>%
  group_by( DATE) %>%
  summarise(VWP = sum(RRP*TOTALDEMAND)/sum(TOTALDEMAND),
            maxP=max(RRP))  ->
NEM.VWP

aemo %>% mutate(DATE= as.Date(  SETTLEMENTDATE)) %>%
  group_by(DATE, REGION) %>%
  summarise(RRP = sum(RRP*TOTALDEMAND)/sum(TOTALDEMAND), 
            TOTALDEMAND= max(TOTALDEMAND)) %>%
group_by(DATE) %>%
  summarise(VWP = mean(RRP))->
  NEM1.VWP

tail(NEM.VWP)
str(NEM.VWP)
fn <-"data/PUBLIC_WALLUMBILLABENCHMARKPRICE_20200227.CSV"
#fn <-"http://nemweb.com.au/Reports/Current/GSH/Benchmark_Price/PUBLIC_WALLUMBILLABENCHMARKPRICE_20200218_0000000319248202.zip"
gas.bm<-read_csv( fn  ,skip=1) %>%
  subset(str_detect(PRODUCT_LOCATION, "WAL") & 
           !str_detect(PRODUCT_LOCATION, "WAL-Non-Netted"))  %>%
  mutate(DATE= as.Date(lubridate::ymd_hms(GAS_DATE, tz = "Australia/Brisbane")) )%>% 
  select(DATE, BENCHMARK_PRICE_1)

head(gas.bm)
merge(NEM.VWP, gas.bm) -> merge.df
merge(NEM1.VWP, gas.bm) -> merge1.df
merge.df$F <- floor(merge.df$BENCHMARK_PRICE_1) %>% as.factor()
merge.df$Y <- "< 2016-05-01"
merge.df$Y[ merge.df$DATE >= ymd("2016/05/01")]<- "> 2016-05-01"

merge1.df$F <- floor(merge1.df$BENCHMARK_PRICE_1) %>% as.factor()
merge1.df$Y <-  "< 2016-05-01"
merge1.df$Y[ merge1.df$DATE >= ymd("2016/05/01")]<- "< 2016-05-01"

merge.byweek.df <- merge.df %>% 
  mutate(year=year(DATE), week =week(DATE)) %>%
  group_by(year,week) %>%
  summarise(VWP= mean(VWP), 
            BENCHMARK_PRICE_1=mean( BENCHMARK_PRICE_1), DATE=mean(DATE), Y=head(Y,1) ,F=head(F,1))

ggplot(merge.byweek.df %>% subset(DATE>ymd("2016-01-01")), aes(DATE, BENCHMARK_PRICE_1))+
   geom_line(#data=NEM.VWP %>% subset(DATE> ymd("2014-03-01")),
            size=.25, colour="red2", aes(y=VWP/10))+ 
  geom_line(size=.75, colour="white")+ 
  geom_line(size=.25, colour="grey20")+ 
  coord_cartesian(ylim=c(3,17))+
  labs(y= "Wallumbilla gas benchmark - $/GJ",
                 x = NULL,
       title= "Electricity and gas price",
       subtitle="weekly averages")+
 # theme_minimal()+
  hrbrthemes::theme_ipsum_rc()+
 # geom_hline(yintercept = c(5,10,15), size=.15, linetype=2)+
#  geom_point(data=head(gas.bm,1), col="white",size=2.5)+
  geom_point(data=tail(merge.byweek.df,2)[2,], col="white", size=3.5) +
  geom_point(data=tail(merge.byweek.df,2)[2,], col="black", size=2.8) +
  geom_point(data=tail(merge.byweek.df,2)[2,], aes(y=VWP/10), col="white", size=1.8) +
  geom_point(data=tail(merge.byweek.df,2)[2,], aes(y=VWP/10), col="red2", size=1.) +
  scale_y_continuous(sec.axis = sec_axis(~.*10,  name = "NEM volume weighted - $/MWhr" ))+
    theme( axis.text.y.left = element_text( size = rel(1.)),
    axis.text.y.right = element_text(color = "red2", size = rel(1.)),
    axis.title.y.left= element_text(    size = rel(1.) ),
#    axis.line.y.right= element_line(colour="red2"),
    axis.title.y.right= element_text(angle = -90,   color = "red2", size = rel(1.))) 


ggsave("wal-bench_1.png",  width=9, height=6) 

ggplot(merge.byweek.df, aes(BENCHMARK_PRICE_1,VWP)) +
  geom_boxplot( data= merge.df %>% subset(BENCHMARK_PRICE_1<=9.99 & BENCHMARK_PRICE_1>1.01),aes(group=F) ,
                outlier.size=0,colour="grey60",
                alpha=.5, fill="yellow3", notch=T )+
  geom_point( size=.7, aes(col=Y),alpha=.6)+
  coord_cartesian(ylim=c(20,200), xlim=c(0, 13.5))+
#  geom_violin( aes(group=F) , alpha=0 )+
   geom_hline(yintercept= 51, size=.2, linetype=2)+
  geom_vline(xintercept = 5, size=.2, linetype=2)+
    labs(x= "Wallumbilla gas benchmark - $/GJ",
       y = "NEM volume weighted - $/MWhr\nlog10 scaling", 
       title= "Electricity v. gas price",
       subtitle="weekly averages")+
  # theme_minimal()+
  hrbrthemes::theme_ipsum_rc()+
  scale_color_manual(values = c("blue3", "red3"))+
  theme(legend.position=c(.85, .2), 
        legend.title=element_blank(), 
        legend.background =element_rect(colour="grey30", size=.3))+
   scale_y_log10(breaks=c(20,30,40,50,70,100,140, 200))+
  # geom_smooth(data= merge.df %>% subset(BENCHMARK_PRICE_1<=9 & BENCHMARK_PRICE_1>.5),
  #                                       method="lm", col="black", se=F)+
  annotate(geom="text", x=12, y=50, label="$51/MWhr", vjust=1.3)+
  annotate(geom="text", x=5, y=170, label="$5/GJ", vjust=-.5, angle=90)
  

ggsave("wal-nem.png", width=9, height=6) 

ggplot(merge.byweek.df  , 
       aes(BENCHMARK_PRICE_1,VWP)) +
  geom_point(size=.3,aes(col=Y)) +
  ylim(c(25,170))+
  geom_density_2d(  nlevel=10,  size=.5 , alpha=.6)+
  theme_minimal()+
  theme(legend.position=c(.85, .2), 
        legend.title=element_blank())
  
  
       
ggplot(merge.df, aes(BENCHMARK_PRICE_1,VWP)) +
  geom_point( size=.2, aes(col=Y))+
  coord_cartesian(ylim=c(25,250))+
  geom_boxplot( aes(group=F) , alpha=0 )+
  geom_hline(yintercept=70.50, size=.2)+
  geom_vline(xintercept = 2, size=.2)+
  labs(x= "Wallumbilla gas benchmark - $/GJ",
       y = "NEM max daily - $/MWhr", 
       subtitle= "max daily  electricity price")+
  theme_classic()+
  theme(legend.position=c(.85, .2), 
        legend.title=element_blank())+
  scale_y_log10()
 

 
