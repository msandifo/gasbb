#library(XML)


url_files <- function(url, selector = "") {
  links <- XML::getHTMLLinks(url)
  links[str_detect(links, selector)]
}

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

read_wall_bm <- function(fn ="data/PUBLIC_WALLUMBILLABENCHMARKPRICE.zip")
  {read_csv( fn  ,skip=1)  %>%
  subset(str_detect(PRODUCT_LOCATION, "WAL") & 
           !str_detect(PRODUCT_LOCATION, "WAL-Non-Netted")) %>%
  #select(-BENCHMARK_PRICE) %>%
  rename(gasdate=GAS_DATE, benchmark_price = BENCHMARK_PRICE_1 ) %>%
  select(-BENCHMARK_PRICE,-`1`) %>%
  rename_all(tolower)%>%
  mutate(gasdate= lubridate::ymd_hms(gasdate) %>% as.Date() )
}
#ggplot(wall.bm, aes(gasdate, benchmark_price))+geom_line()
