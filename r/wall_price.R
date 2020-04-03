if (!exists("hub")) hub<-"WAL"
if (update) download_wall_bm()
gas.bm <-read_wall_bm(hub=hub)


if (!exists("aemo")){
   if(!exists("update.aemo")) update.aemo=F
  aemo<- read_aemo(update=update.aemo) %>% rename_all(tolower)
  tz(aemo$settlementdate) <-  "Australia/Brisbane"
  as.Date(aemo$settlementdate) 
} 
 
if (!exists("rrp.cap")) rrp.cap<-13000
message("rrp.cap set at $",rrp.cap)

nem.vwp<- nem_vwp(aemo %>% subset(rrp<=rrp.cap))
nem.month <-aemo %>% subset(rrp<=rrp.cap) %>%mutate(date= as.Date(  settlementdate), month=month(date), year=year(date)) %>%
  group_by(year,month) %>%
  summarise(vwp = sum(rrp*totaldemand)/sum(totaldemand), 
            totaldemand=mean(totaldemand), date=tail(date,1) ) 


merge.df <-merge(nem.vwp, 
                 gas.bm %>% 
                   rename(date=gasdate) %>% 
                   select(date, benchmark_price))  

merge.df$F <- round(merge.df$benchmark_price,0) %>% as.factor()
merge.df$Y <- "< 2016-01-01"
merge.df$Y[ merge.df$date >= ymd("2016/01/01")]<- "> 2016-01-01"


merge.byweek.df <- merge.df %>% 
  mutate(year=year(date), 
         week =week(date)) %>%
  group_by(year,week) %>%
  summarise(vwp= mean(vwp), 
            benchmark_price=mean( benchmark_price), 
            date=tail(date,1), Y=head(Y,1) ,F=head(F,1))

gas.bm.s<-gas.bm %>%
 #   subset(str_detect(product_location,  hub)) %>%
    mutate(year=lubridate::year(gasdate),  month=lubridate::month(gasdate)) %>%
   group_by(year, month) %>%
   dplyr::summarise(gasdate=  as.Date(mean(gasdate)),benchmark_price = max(benchmark_price))

# load("~/Dropbox/msandifo/documents/programming/r/twitter/2018/001/data/data.Rdata")
# NEM.month



m.df <-merge(gas.bm.s,nem.month)

m1.df <- m.df %>% subset(year<2021) 
m1.df$month.average <-"pre 2016"
m1.df$month.average[m1.df$date>lubridate::ymd("2016/01/01")] <-"2016 on"

m2.df <- m1.df%>% 
  group_by(month.average) %>%
  summarise(benchmark_price=mean(benchmark_price), 
            vwp=mean(vwp), date=tail(date,1))


