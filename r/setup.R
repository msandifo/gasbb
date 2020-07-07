#libraries 

library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)

#---------------
theme_set(  hrbrthemes::theme_ipsum())# base_family = "Roboto Condensed"))
theme_get()

theme_replace(
  panel.grid.minor = element_line(
    size=.1,
    linetype=2,
    colour="grey80"
),
  plot.title= element_text(
    size=13,
    family="Arial Narrow",
    face="bold",
    hjust=0,
    vjust=1,
    margin= unit(c(0,0,8,0), "pt")),
  panel.grid= element_line(
    size=.1,
    colour="grey80"
  )
)


# funcs 
source('~/Dropbox/msandifo/documents/programming/r/2020/gasbb/r/general_funcs.R')
source('~/Dropbox/msandifo/documents/programming/r/2020/gasbb/r/data_funcs.R')
source('~/Dropbox/msandifo/documents/programming/r/twitter/2018/001/src/functions.R')

# diretcories

archived.flows.csv <- "data/archived/ActualFlows.csv"

# current flows need manual updating at 
# https://www.aemo.com.au/energy-systems/gas/gas-bulletin-board-gbb/data-portal
# current.flows.csv <-"data/Actual Flow and Storage_20200304114556.csv"  #this needs manual updating
# current.flows.csv <-"data/Actual Flow and Storage_20200306222127.csv"
# current.flows.csv <-"data/Actual Flow and Storage_20200309062023.csv"
# current.flows.csv <-"data/Actual Flow and Storage_20200318050353.csv"
# current.flows.csv <-"data/Actual Flow and Storage_20200322132204.csv"
# current.flows.csv <-"data/Actual Flow and Storage_20200405180110.csv"
current.flows.csv   <-"data/Actual Flow and Storage_20200416212904.csv"
current.flows.csv   <-"data/Actual Flow and Storage_20200423135038.csv"
# vars

QGP   <- "Queensland Gas Pipeline"  
APLNG <- "APLNG Pipeline"
GLNG  <- "GLNG Gas Transmission Pipeline" 
WGP   <- "Wallumbilla to Gladstone Pipeline" 


# data_prep

update=T

read_aemo()
hub = "SEQ"
if (!exists("flows") | update==T) source('r/read_flows.R')
if (!exists("glad.lng") | update==T) glad.lng <-read_lng()
if (!exists("total.prod.month") | update==T) source("r/lng_prod.R")
if (!exists("nem.month") | update==T) source("r/wall_price.R")
if (!exists("flows.ci") | update==T) source("r/wall_flows.R")

# plots

#extrafont::font_import()

# d <- read.csv(extrafont:::fonttable_file(), stringsAsFactors = FALSE)
# d[grepl("Condensed-Light", d$FontName), ]$FamilyName <-  "Roboto Condensed L"
# write.csv(d, extrafont:::fonttable_file(), row.names = FALSE)
# # 
#extrafont::loadfonts()

#extrafont::loadfonts( quiet = FALSE)

if (!exists("plots")) plots=T
if (!exists("do_save")) do_save=T # do_save=T
width=7; height=4.8
if (plots){
  
source("r/wall_price_plots.R")
source("r/wall_flow_plots.R")
source("r/lng_prod_plots.R")
#source("r/china.lng.R")
}


