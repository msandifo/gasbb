library(tidyverse)
library(reproscir)
library(lubridate)


#' Title
#'
#' @param update 
#'
#' @return
#' @export
#'
#' @examples
read_aemo<- function(update=T){
  source('~/Dropbox/msandifo/Documents/programming/r/twitter/2018/001/src/functions.R')
last.year <- year(Sys.Date())-1
this.year <- year(Sys.Date())
this.month <- month(Sys.Date()) 
last.month <- this.month - 1 

download_aemo_aggregated (year=2010:last.year, month=1:12)
if (last.month>0) download_aemo_aggregated (year=this.year, month=1:last.month)
download_aemo_current()

l.path=validate_directory(NULL) 
list.files(  l.path)
aemo <-build_aemo() %>% rename_all(tolower)
tz(aemo$settlementdate) <-  "Australia/Brisbane"
aemo  
}

#' Title
#'
#' @param aemo 
#'
#' @return
#' @export
#'
#' @examples
nem_vwp <- function(aemo= read_aemo()){
  aemo%>% 
  mutate(date= as.Date(  settlementdate)) %>%
  group_by( date) %>%
  summarise(vwp = sum(rrp*totaldemand)/sum(totaldemand),
            max.rrp=max(rrp)) 
}
  