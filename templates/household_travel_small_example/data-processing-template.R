library(psrc.travelsurvey)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)
library(odbc)
library(DBI)
library(tidyr)




db.connect <- function(adatabase) {
  elmer_connection <- dbConnect(odbc(),
                                driver = "SQL Server",
                                server = "AWS-PROD-SQL\\SOCKEYE",
                                database = adatabase,
                                trusted_connection = "yes"
  )
}

# read table
read.dt <- function(adatabase, atable) {
  elmer_connection <- db.connect(adatabase)
  dtelm <- dbReadTable(elmer_connection, SQL(atable))
  dbDisconnect(elmer_connection)
  return(dtelm)
}

output_dir<-file.path(getwd(), 'outputs')



if(!dir.exists(output_dir)){ 
  dir.create(output_dir)
}


all_vars<-c('mode_simple')

Walk_bike_data_17_19<- get_hhts("2017_2019", "t", vars=all_vars)%>% mutate(year=ifelse(survey=='2017_2019', '2017/2019', '2021'))

Walk_bike_data_21<- get_hhts("2021", "t", vars=all_vars)%>% mutate(year=ifelse(survey=='2017_2019', '2017/2019', '2021'))




trips_by_mode_17_19<-hhts_count(Walk_bike_data_17_19, group_vars='mode_simple')%>%
  filter(mode_simple!='Total')
trips_by_mode_21<-hhts_count(Walk_bike_data_21, group_vars='mode_simple')%>%filter(mode_simple!='Total')

trips_by_mode_17_19_21<-merge(trips_by_mode_17_19, trips_by_mode_21, by='mode_simple', suffixes=c('17_19', '21'))
trips_by_mode<-rbind(trips_by_mode_17_19, trips_by_mode_21)%>% mutate(year=ifelse(survey=='2017_2019', '2017/2019', '2021'))
trips_by_mode_no_drive<-trips_by_mode%>%filter(!mode_simple %in% c('Drive', 'Other'))

write.csv(trips_by_mode_no_drive, paste0(output_dir, '/trips_by_mode.csv'))
