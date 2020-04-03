
ggplot(nem.month.vwp %>% subset(date>ymd("2010-01-01"))   ,
       aes(date, vwp))+
  geom_line(  size=.25, colour="lightblue3" )+ 
  geom_smooth(aes(group=year), method="lm", se=F, formula=y~1, col="grey30", size=.3)+
  coord_cartesian(ylim=c(30,150))+
  labs(y= "AUD$/MWhr",
       x = NULL,
       title= "NEM spot price",
       subtitle="monthly averages")+
 hrbrthemes::theme_ipsum_rc(grid_col = "grey90")+
  geom_hline(yintercept = c(5,10,15)*10, size=.15, linetype=2)+
  #  geom_point(data=head(gas.bm,1), col="white",size=2.5)+
  geom_point(data=tail(merge.byweek.df,3)[1,], col="white", size=3.5) +
  geom_point(data=tail(merge.byweek.df,3)[1,], col="red3", size=2.8) 

ggsave("wal-bench_3.png", width=7, height=4)

 