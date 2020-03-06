aemo %>% mutate(DATE= as.Date(  SETTLEMENTDATE), year=year(DATE), month=month(DATE)) %>%
  group_by( year, month) %>%
  summarise(VWP = sum(RRP*TOTALDEMAND)/sum(TOTALDEMAND),
            maxP=max(RRP), DATE=mean(DATE))  ->
  NEM.month.VWP

ggplot(NEM.month.VWP %>% subset(DATE>ymd("2010-01-01"))  ,
       aes(DATE, VWP))+
  geom_line(  size=.25, colour="Yellow4" )+ 
  geom_smooth(aes(group=year), method="lm", se=F, formula=y~1)+
  coord_cartesian(ylim=c(30,150))+
  labs(y= "AUD$/MWhr",
       x = NULL,
       title= "NEM spot price",
       subtitle="monthly averages")+
  theme_minimal()+
  geom_hline(yintercept = c(5,10,15)*10, size=.15, linetype=2)+
  #  geom_point(data=head(gas.bm,1), col="white",size=2.5)+
  geom_point(data=tail(merge.byweek.df,2)[2,], col="white", size=3.5) +
  geom_point(data=tail(merge.byweek.df,2)[2,], col="black", size=2.8) 

ggsave("wal-bench_3.png", width=7, height=4)

# +
#    theme( axis.text.y.left = element_text( size = rel(1.)),
#          axis.text.y.right = element_text(color = "red2", size = rel(1.)),
#          axis.title.y.left= element_text(    size = rel(1.) ),
#          #    axis.line.y.right= element_line(colour="red2"),
#          axis.title.y.right= element_text(angle = -90,   color = "red2", size = rel(1.))) 
