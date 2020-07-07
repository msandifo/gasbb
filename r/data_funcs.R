
#' Title
#'
#' @param wall.remote.dir 
#' @param wall.remote.fn 
#'
#' @return
#' @export
#'
#' @examples
download_wall_bm <- function(
  wall.remote.dir ="http://nemweb.com.au/Reports/Current/GSH/Benchmark_Price/",
  wall.remote.fn = paste0("PUBLIC_WALLUMBILLABENCHMARKPRICE_" ,
                          stringr::str_remove_all(Sys.Date()-1, "-"), ".zip" )
){
  
  rem.file<-paste0("http://nemweb.com.au",
                   url_files(wall.remote.dir, selector=wall.remote.fn ) )
  fn ="data/PUBLIC_WALLUMBILLABENCHMARKPRICE.zip"
  
  download.file(rem.file, fn)
}

#' Title
#'
#' @param fn 
#'
#' @return
#' @export
#'
#' @examples
read_wall_bm <- function(fn ="data/PUBLIC_WALLUMBILLABENCHMARKPRICE.zip", hub="WAL")
{ wal.bm.df<-read_csv( fn  , skip=1)  %>%
    subset((str_detect(PRODUCT_LOCATION, "WAL" )  ) & 
             !str_detect(PRODUCT_LOCATION, "WAL-Non-Netted")) %>%
    #select(-BENCHMARK_PRICE) %>%
    rename(gasdate=GAS_DATE, benchmark_price = BENCHMARK_PRICE_1 ) %>%
    select(-BENCHMARK_PRICE,-`1`) %>%
    rename_all(tolower)%>%
    mutate(gasdate= lubridate::ymd_hms(gasdate) %>% as.Date() )

if (hub=="SEQ") {bm.seq.df <- read_csv( fn  , skip=1)  %>%
    subset((str_detect(PRODUCT_LOCATION, "SEQ" )  ) & 
             !str_detect(PRODUCT_LOCATION, "WAL-Non-Netted")) %>%
    #select(-BENCHMARK_PRICE) %>%
    rename(gasdate=GAS_DATE, benchmark_price = BENCHMARK_PRICE_1 ) %>%
    select(-BENCHMARK_PRICE,-`1`) %>%
    rename_all(tolower)%>%
    mutate(gasdate= lubridate::ymd_hms(gasdate) %>% as.Date() )

wal.bm.df[1:(nrow(bm.seq.df)-1),] <- head(bm.seq.df,-1)
}
wal.bm.df
}
#ggplot(wall.bm, aes(gasdate, benchmark_price))+geom_line()



#' Title
#'
#' @param update 
#'
#' @return
#' @export
#'
#' @examples
read_aemo<- function(update=T, return=T){
  source('~/Dropbox/msandifo/Documents/programming/r/twitter/2018/001/src/functions.R')
  last.year <- year(Sys.Date())-1
  this.year <- year(Sys.Date())
  this.month <- month(Sys.Date()) 
  last.month <- this.month - 1 
  print(last.month)
  print(this.year)
  download_aemo_aggregated (year=2010:last.year, month=1:12)
  if (last.month>0) download_aemo_aggregated (year=this.year, month=1:last.month)
  download_aemo_current()
  
  l.path=validate_directory(NULL) 
  list.files(  l.path)
  aemo <-build_aemo() %>% rename_all(tolower)
  tz(aemo$settlementdate) <-  "Australia/Brisbane"
 if (return) aemo  
}

#add to reproscir::download_gasbb()



update_lng <- function(update=F){
  if(update){
    read_lng(update=T, country="Total")
    read_lng(update=T, country="China")
    read_lng(update=T, country="Japan")
    read_lng(update=T, country="India")
    read_lng(update=T, country="Malaysia")
    read_lng(update=T, country="Singapore")
    read_lng(update=T, country="Korea Republic of")
  }  
  
  bind_rows(
    read_lng(update=F, country="Total") %>% mutate(country="Total"),
    read_lng(update=F, country="China") %>% mutate(country="China"),
    read_lng(update=F, country="Japan") %>% mutate(country="Japan"),
    read_lng(update=F, country="India") %>% mutate(country="India"),
    read_lng(update=F, country="Malaysia") %>% mutate(country="Malaysia"),
    read_lng(update=F, country="Singapore") %>% mutate(country="Singapore"),
    read_lng(update=F, country="Korea Republic of") %>% mutate(country="Korea")
  ) 
  
}
#' Title
#'
#' @param update 
#'
#' @return
#' @export
#'
#' @examples
read_lng<- function(update=F, country="Total"){
  gasbb_archived_data_repo<- "https://aemo.com.au/Gas/Gas-Bulletin-Board/Other-Information/Archived-Reports"
  fn <- paste0("data/gladstoneLNG_", country, ".rds")
  last.month <-lubridate::month(Sys.Date())-1
  if (update){
    library(rvest)
    glad.lng <-rbind(purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2015, country=country),
                     purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2016, country=country ),
                     purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2017, country=country ),
                     purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2018, country=country ),
                     purrr::map_df(1:12,    reproscir::read_gladstone_ports, year=2019, country=country ),
                     purrr::map_df(1:last.month,    reproscir::read_gladstone_ports, year=2020, country=country )
    )
    
    glad.lng$TJ <-glad.lng$tonnes * 12 * 8.975/172/365
    
    glad.lng <- rbind(glad.lng[1,] , glad.lng)
    glad.lng$date[1] <-glad.lng$date[1] - months(1)
    glad.lng$tonnes[1] <-0
    glad.lng$TJ[1] <-0
    glad.lng$year[1] <-2014
    glad.lng$month[1] <-12
    saveRDS(glad.lng, file=fn)
  } else  glad.lng <-readRDS(fn)
  
  glad.lng
}

#' 
#' #' Title
#' #'
#' #' @param update 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' read_aemo<- function(update=T){
#'   source('~/Dropbox/msandifo/Documents/programming/r/twitter/2018/001/src/functions.R')
#'   last.year <- year(Sys.Date())-1
#'   this.year <- year(Sys.Date())
#'   this.month <- month(Sys.Date()) 
#'   last.month <- this.month - 1 
#'   
#'   download_aemo_aggregated (year=2010:last.year, month=1:12)
#'   if (last.month>0) download_aemo_aggregated (year=this.year, month=1:last.month)
#'   download_aemo_current()
#'   
#'   l.path=validate_directory(NULL) 
#'   list.files(  l.path)
#'   aemo <-build_aemo() %>% rename_all(tolower)
#'   tz(aemo$settlementdate) <-  "Australia/Brisbane"
#'   aemo  
#' }

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

 
get_fits <- function(df ) {

  fit <-lm(data=df  ,  (vwp)~benchmark_price)
  paste0 ("intercept = ",broom::tidy( fit)[1,2:3] %>% round(2) %>% glue::glue_collapse( sep = c(" "), last="±"), "$/MWhr\n",
          "slope     = ",broom::tidy( fit)[2,2:3] %>% round(2) %>% glue::glue_collapse( sep = c(" "), last="±"), "$/GJ\n",
          "adj. r^2  = ", broom::glance(fit)[2] %>% round(3))
}
