pacman::p_load(tidyverse, rtsdata,lubridate)

rts_fortify <-function(df){
  df<- fortify(df)
  n <- names(df) %>% stringr::str_split(".AX.", simplify=T)
  names(df) <- stringr::str_to_lower(c(n[1,1], n[2:7,2]))
  df$name<-n[2,1]
  df %>% dplyr::rename(date=index)
}

 
org.df<- rbind(ds.getSymbol.yahoo("ORG.AX", from = Sys.Date()- years(20), to = Sys.Date()) %>% 
  rts_fortify(),
  ds.getSymbol.yahoo("STO.AX", from = Sys.Date()- years(20), to = Sys.Date()) %>% rts_fortify())

org.df$name[org.df$name=="ORG"] <- "Origin"
org.df$name[org.df$name=="STO"] <- "Santos"

ggplot(data= org.df, aes(date,close, col=name))+
  geom_vline(xintercept = ymd("2015-01-01"), size=.2, linetype=2)+
  geom_hline(data=org.df %>%subset(date ==max(date)),
             aes(yintercept = close, col=name), size=.2, linetype=2)+
geom_line(size=.25)+
     geom_point(data= org.df %>%subset(date ==max(date)),  size=3,col="grey100")+
  geom_point(data=org.df %>%subset(date ==max(date)), size=1.5)+
  hrbrthemes::theme_ipsum_rc()+
   scale_color_manual(values=c("firebrick3", "lightblue4"))+
  theme(legend.position= c(.85,.85),
        legend.title=element_blank())+
labs(subtitle ="ASX", 
y= "closing price - $", x=NULL)

ggsave("figs/ASX.png", width=7, height= 4.7)


