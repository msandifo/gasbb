tf.df <- feather::feather("data/aemo/techfuel.feather")  %>%
  as_tibble() %T>%
  glimpse()
pd.df <- feather::feather("data/aemo/price_demand.feather") %>% as_tibble()


tf.df$date <- tf.df$SETTLEMENTDATE %>% 
  as.character() %>% 
  ymd_hms(tz="Australia/Brisbane") +
  hours(20)

region.df <-tf.df %>% 
  group_by(date, REGIONID) %>%
  summarise(MWH=sum(MWH))

ggplot(region.df , aes(date,MWH*2, col=REGIONID))+geom_line(size=.1)


pd.df$date <- pd.df$SETTLEMENTDATE %>% as.character() %>% 
   ymd_hms(tz="Australia/Brisbane")  -hours(11) - minutes(30)

 

# tech.fuel <-   feather::read_feather("data/aemo/techfuel.feather") 
# price.demand <-  feather::read_feather("data/aemo/price_demand.feather")

state="QLD1"
fuel="gas"

qld.gas <-tf.df %>% subset(REGIONID==state & str_detect(FUELTECH, fuel)  ) 

qld.gas.c <- qld.gas %>% 
  group_by(date) %>%
  summarise (MWH=sum(MWH) ) 
  

tech.fuel <- merge( qld.gas.c,
                    pd.df %>% subset(REGIONID==state))   %>% 
  mutate(year=year(date), month=month(date), y=factor(year))

# ggplot(tech.fuel %>% subset(month>5 & month<9 &year>2010), aes( MWH*2/1000,RRP)) +
#   geom_point(size=.1, col="lightblue3")+
#   facet_wrap(~y, ncol=3)+
#   coord_cartesian(ylim=c(10,350))+
#   geom_smooth(method="lm", se=F, )+
#   scale_y_log10()+
# hrbrthemes::theme_ipsum_rc(grid_col = "grey95")+
#   labs(title=paste0(state,", ", fuel), x="GW")

ggplot(tech.fuel %>% subset(month>3 & month < 10 &year>=2010),
       aes( MWH*2/1000,RRP, col=y, fill=y)) +
  # geom_point(size=.1, col="lightblue3")+
  # facet_wrap(~y, ncol=3)+
  coord_cartesian(ylim=c(10,160))+
  geom_smooth(method="lm", se=T, fullrange=F, size=.5,alpha=.1)+
  #scale_y_log10()+
 # hrbrthemes::theme_ipsum_rc(grid_col = "grey95")+
  labs(title=paste0(state,", ", fuel), x="GW")

ggsave(paste0("figs/",state,"_", fuel,".png"), width=7, height=4 )

       