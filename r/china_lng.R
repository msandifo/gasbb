# country<-"China"
# last.month <-lubridate::month(Sys.Date())-1
# glad.lng.china <-rbind(purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2015, country=country),
#                  purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2016, country=country ),
#                  purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2017, country=country ),
#                  purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2018, country=country ),
#                  purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2019, country=country ),
#                  purrr::map_df(1:last.month,    reproscir::read_gladstone_ports, year=2020, country=country )
# )

glad.lng.china <- update_lng(update=F) %>% subset(country=="China")
glad.lng.china$t <- glad.lng.china$tonnes/glad.lng.china$mdays *365/12
 p.change <- diff( tail(glad.lng.china$tonnes/glad.lng.china$mdays,2))/tail(glad.lng.china$tonnes/glad.lng.china$mdays,2)[1]*100
 p.change <- diff( tail(glad.lng.china$t,2))/tail(glad.lng.china$t,2)[1]*100
 
lng.exports.china <- ggplot(glad.lng.china  , aes(date, t/1e6)) +
  geom_line(size=.15, aes(y=tonnes/1e6), linetype=1, col="red2")+
  geom_line(size=.25)+
  geom_point(size=1.5, col="white")+
  geom_point(size=.75)+
  geom_point(data= . %>% tail(1), size=2.2, shape=25, col="white")+
  geom_point(data= . %>% tail(1), size=1.5, shape=25,col="black", fill="yellow2")+
  geom_text(data= . %>% tail(2), size=2.5, aes(label=paste(round(t/1e6,2),"mt")), hjust=-.25)+
 annotate("text", x=tail(glad.lng.china$date,1), y= tail(glad.lng.china$t,1)/1e6,
          label= paste0(round(p.change,0), "%*" ), vjust= -1.7, hjust=-.15,size=3., col="red3")+
  #hrbrthemes::theme_ipsum_rc(grid_col = "grey90")+
  labs(title="Curtis Island LNG exports to China",
       caption= "data from Gladstone Port Authority\n*monthly rates adjusted to standard duration",
       x=NULL,
       y="million tonnes per month*")+
  scale_x_date(limits= ymd(c("2015-01-01", "2020-05-01")),
               expand=c(0.05, 0.0)
  )



glad.lng.country <- update_lng(update=F) %>% subset(country %ni% c("Total", "India", "Singapore"))
glad.lng.total <- update_lng(update=F) %>% subset(country=="Total")


lng.exports.destination <- ggplot(glad.lng.country  , aes(date, TJ)) +
     geom_area(size=.1, col="white", aes( fill=country))+
  geom_line(data=glad.lng.total, size=.65, col="white")+
  geom_line(data=glad.lng.total, size=.2)+
  geom_point(data=glad.lng.total, size=.45, col="white")+
  geom_point(data=glad.lng.total, size=.05)+
  # geom_point(size=1.5, col="white")+
  # geom_point(size=.75)+
  # geom_point(data= . %>% tail(1), size=3, col="white")+
  # geom_point(data= . %>% tail(1), size=2, col="black")+
  # geom_text(data= . %>% tail(2), size=2.5, aes(label=paste(round(t/1e6,2),"mt")), hjust=-.25)+
  # annotate("text", x=tail(glad.lng.china$date,1), y= tail(glad.lng.china$t,1)/1e6,
  #          label= paste0(round(p.change,0), "%*" ), vjust= -2., hjust=-.15,size=3., col="red3")+
   #hrbrthemes::theme_ipsum_rc(grid_col = "grey95")+
  labs(title="Curtis Island LNG exports by destination",
       caption= "data from Gladstone Port Authority",
       x=NULL,
       y="TJ/day")+
  theme(legend.position = c(.125,.7), legend.title = element_blank()) +
  scale_x_date(expand=c(0.01, 0.0))+
  scale_fill_manual(values=c("red3", "darkorchid", "yellow3", "lightblue3", "black"))
#scale_fill_manual(values=mmt::add.alpha(c("red3", "darkorchid", "yellow3", "lightblue3", "black"),0))

china.lng =list(
  lng.exports.china,
  lng.exports.destination
)

if (do_save){
ggsave('figs/lng.exports.china.png',  
       plot=lng.exports.china,
       width=width, height=height)

ggsave('figs/lng.exports.destination.png', 
       plot=lng.exports.destination,
       width=width, height=height)
}
