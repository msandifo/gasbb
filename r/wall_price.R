library(magrittr)
library(tidyverse)
library(lubridate)

#http://nemweb.com.au/Reports/Current/GSH/Benchmark_Price/
fn <-"data/PUBLIC_WALLUMBILLABENCHMARKPRICE_20200227.CSV"
#fn <-"http://nemweb.com.au/Reports/Current/GSH/Benchmark_Price/PUBLIC_WALLUMBILLABENCHMARKPRICE_20200218_0000000319248202.zip"
gas.bm<-read_csv( fn  ,skip=1) %>%
  subset(str_detect(PRODUCT_LOCATION, "WAL") & 
            !str_detect(PRODUCT_LOCATION, "WAL-Non-Netted"))
tail(gas.bm)
names(gas.bm)
gas.bm$GAS_DATE
gas.bm.s<-gas.bm %>%
  subset(str_detect(PRODUCT_LOCATION, "WAL")) %>%
  mutate(GAS_DATE= lubridate::ymd_hms(GAS_DATE)) %>%
  mutate(year=lubridate::year(GAS_DATE),  month=lubridate::month(GAS_DATE)) %>%
  group_by(year, month) %>%
  dplyr::summarise(GAS_DATE=  as.Date(mean(GAS_DATE)), BENCHMARK_PRICE_1 =max(BENCHMARK_PRICE_1))

load("~/Dropbox/msandifo/documents/programming/r/twitter/2018/001/data/data.Rdata")
NEM.month

m.df <-merge(gas.bm.s, NEM.month)
ggplot(m.df, aes(date, VWP))+
  geom_line(size=.35, col="grey40")+
  geom_line(aes(y=BENCHMARK_PRICE_1*10),col="white",alpha=.9, size=1)+
  geom_line(aes(y=BENCHMARK_PRICE_1*10),col="red2", size=.4)+
  hrbrthemes::theme_ipsum_rc()+
  scale_y_continuous(sec.axis = sec_axis(~./10,  name = "WALL Benchmark- $/GJ\ngas")  )+
  
  labs(y="NEM VWP  $/MWH\nelectricity", x=NULL,
       title="NEM electricity VWP, WALLUMBILLA gas bench.")+

  theme( 
    axis.text.y.left = element_text( size = rel(1.2)),
    axis.text.y.right = element_text(color = "red2", size = rel(1.2)),
    axis.title.y.left= element_text(    size = rel(1.2) ),
    axis.title.y.right= element_text(angle = -90,   color = "red2", size = rel(1.5))) 
ggsave("figs/wall.1.png", width=7, height=4) 


m1.df <- m.df %>% subset(year<2020) 
m1.df$month.average <-"before May 2016"
m1.df$month.average[m1.df$date>lubridate::ymd("2016/06/01")] <-"after May 2016"

m2.df <- m1.df%>% 
  group_by(month.average) %>%
  summarise(BENCHMARK_PRICE_1=mean(BENCHMARK_PRICE_1), 
            VWP=mean(VWP), date=mean(date))
  
ggplot(data=m1.df, aes(x=BENCHMARK_PRICE_1, y=VWP)) +
  geom_hline(data=m2.df, aes(yintercept =VWP , col=month.average))+
  geom_vline(data=m2.df, aes(xintercept =BENCHMARK_PRICE_1 , col=month.average))+
  geom_smooth(   method = "lm", se=T,alpha=.1, col="grey40", size=.5,linetype=2)+
  # geom_smooth(aes(  col=period ),
  #             method = "lm", se=T,alpha=.1,  size=.5,linetype=2)+
  geom_point(size=2.5, aes(  col=month.average,shape=month.average))+
  hrbrthemes::theme_ipsum_rc()+
  geom_density_2d(    size=.1 , alpha=.6)+
  scale_color_manual(values=c("blue3", "red3"))+
  labs(y="NEM VWP  $/MWH\nelectricity", 
       x="WALL Benchmark- $/GJ\ngas",
       title="NEM electricity VWP, WALLUMBILLA gas bench.") +
   theme( legend.position = c(.13,.7), 
         #  legend.title = element_blank(),
         legend.background = element_rect(colour="white"),
         axis.text.y = element_text( size = rel(1.2)),
         axis.title.y = element_text(    size = rel(1.2) ),
axis.text.x = element_text( size = rel(1.)),
axis.title.x = element_text(    size = rel(1.) ))

  
  ggsave("figs/wall.2.png", width=7 , height=4 ) 
 #   ggplot(gas.bm.s,aes(year+(month-.5)/12, BENCHMARK_PRICE_1))+geom_line()

   