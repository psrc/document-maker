
library(data.table)
library(leaflet)
library(shiny)
library(tidytext)
library(tidycensus)
library(dplyr)
library(readr)
library(ggplot2)
library(ggiraph)
library(magrittr)
library(usethis)
library(installr)
library(sf)
library(forcats)
library(tidyr)
library(summarytools)
library(sp)
library(gridExtra)
library(ggpubr)

setwd("C:/Coding/CURRENT_REPOS_GITHUB/document-maker/templates/equity_example")

# packages that are from github that host functions for pulling data (do the install in R Gui, not RStudio)

#devtools::install_github("psrc/psrc.travelsurvey", force = TRUE)
#devtools::install_github("psrc/psrccensus", force = TRUE)
#devtools::install_github("psrc/psrcplot", force = TRUE)
#devtools::install_github("psrc/psrctrends", force = TRUE)

# run these after installing github changes through R Gui

library(psrc.travelsurvey)
library(psrccensus)
library(psrcplot)
library(psrctrends)

install_psrc_fonts()

# for Elmer connection

library(odbc)
library(DBI)

elmer_connect <- function(){
  DBI::dbConnect(odbc::odbc(),
                 driver = "ODBC Driver 17 for SQL Server",
                 server = "AWS-PROD-SQL\\Sockeye",
                 database = "Elmer",
                 trusted_connection = "yes",
                 port = 1433)
}

elmer_connection <- elmer_connect()
# views and global variables for days and households (** the days table has been updated to include hh variables)
# surveys a,b,c, and d will be used in functions later in code

# census pulse survey data on unemployment by gender and longitudinally & clean
#census_employ <- read.csv('employ_week53.csv')
#census_employ_new <- census_employ[-c(1:3, 129:178),]
#census_employ_tidy <- census_employ_new[,-c(6:15)]
#census_renamed <- setNames(census_employ_tidy, c("characteristics","count","unemployed", "employed", "no_response"))
#census_employ_new2 <- census_renamed[-c(1:4),]
#write.csv(census_employ_new2, 'census_employ_tidy.csv')
#census_employ <- read.csv('census_employ_tidy.csv')
#census_employ <- census_employ[,-c(1, 3)]

pvars <- c("ESR","SEX", "AGEP")
ftr_int <- function(x){as.integer(as.character(x))} 

pums19 <- get_psrc_pums(1, 2019, "p", pvars) 

pums19 %<>% mutate(
  ESR= factor(
    case_when(grepl("^(Civilian|Armed) ", as.character(ESR)) ~ "Employed",
              !is.na(ESR) ~ "Unemployed")),
  AGE = factor(
    case_when(between(ftr_int(AGEP), 18, 25) ~ '18-25',
              between(ftr_int(AGEP), 26, 35) ~ '26-35',
              between(ftr_int(AGEP), 36, 45) ~ '36-45',
              AGEP >= 46 ~ "46+")))

pums19_all <- psrc_pums_count(pums19, group_vars = c("ESR", "SEX"), incl_na=FALSE)%>%
  filter(ESR != "Total")%>%
  filter(SEX != "Total")%>%
  rename(
    survey = DATA_YEAR
  )

pums21 <- get_psrc_pums(1, 2021, "p", pvars) 

pums21 %<>% mutate(
  ESR= factor(
    case_when(grepl("^(Civilian|Armed) ", as.character(ESR)) ~ "Employed",
              !is.na(ESR) ~ "Unemployed")),
  AGE = factor(
    case_when(between(ftr_int(AGEP), 18, 25) ~ '18-25',
              between(ftr_int(AGEP), 26, 35) ~ '26-35',
              between(ftr_int(AGEP), 36, 45) ~ '36-45',
              AGEP >= 46 ~ "46+")))

pums21_all <- psrc_pums_count(pums21, group_vars = c("ESR", "SEX"),incl_na=FALSE)%>%
  filter(ESR != "Total")%>%
  filter(SEX != "Total")%>%
  rename(
    survey = DATA_YEAR
  )


#gender_data_19_21 <- rbind(pums19_all, pums21_all) %>%
 #filter(ESR != "Total")%>%
 #filter(SEX != "Total")%>%
  #rename(
   #survey = DATA_YEAR
  #)

# unemployment by gender for 2019/2021

employ__gender_chart_19 <- static_column_chart(t=pums19_all, 
                                    x = "SEX",
                                    y = "share",
                                    fill = "ESR",
                                    moe = "share_moe",
                                    color="psrc_pairs",
                                    est ="percent")

employ__gender_chart_19

employ__gender_chart_21 <- static_column_chart(t=pums21_all, 
                                    x = "SEX",
                                    y = "share",
                                    fill = "ESR",
                                    moe = "share_moe",
                                    color="psrc_pairs",
                                    est ="percent")

employ__gender_chart_21

# unemployment by age for 2019/2021

pums19_age <- psrc_pums_count(pums19, group_vars = c("ESR", "AGE"), incl_na=FALSE)%>%
  filter(ESR != "Total")%>%
  filter(AGE != "Total")%>%
  rename(
    survey = DATA_YEAR
  )

pums21_age <- psrc_pums_count(pums21, group_vars = c("ESR", "AGE"),incl_na=FALSE)%>%
  filter(ESR != "Total")%>%
  filter(AGE != "Total")%>%
  rename(
    survey = DATA_YEAR
  )

employ__age_chart_19 <- static_column_chart(t=pums19_age, 
                                               x = "AGE",
                                               y = "share",
                                               fill = "ESR",
                                               moe = "share_moe",
                                               color="psrc_pairs",
                                               est ="percent")

employ__age_chart_19

employ__age_chart_21 <- static_column_chart(t=pums21_age, 
                                               x = "AGE",
                                               y = "share",
                                               fill = "ESR",
                                               moe = "share_moe",
                                               color="psrc_pairs",
                                               est ="percent")

employ__age_chart_21

# unemployment by year for 2019/2021
pums19_year <- psrc_pums_count(pums19, group_vars = c("ESR"), incl_na=FALSE)%>%
  filter(ESR != "Total")%>%
  rename(survey = DATA_YEAR)

pums21_year <- psrc_pums_count(pums21, group_vars = c("ESR"),incl_na=FALSE)%>%
  filter(ESR != "Total")%>%
  rename(survey = DATA_YEAR)

employ_19 <- static_column_chart(t=pums19_year, 
                                            x = "ESR",
                                            y = "share",
                                            fill = "ESR",
                                            moe = "share_moe",
                                            color="psrc_pairs",
                                            est ="percent")

employ_19

employ_21 <- static_column_chart(t=pums21_year, 
                                            x = "ESR",
                                            y = "share",
                                            fill = "ESR",
                                            moe = "share_moe",
                                            color="psrc_pairs",
                                            est ="percent")

employ_21
# ggplot count and share plot

count_and_share_plot <- function(dt, grp_var, num_var, legend_name, tbl_name){
  
  fill_group <- dt[[grp_var]]
  x_axis_grp <- dt[[num_var]]
  
  count_plot <- ggplot(dt,
                       aes(x=x_axis_grp,
                           y=count,
                           fill = survey)) +
    geom_bar(stat="identity",
             position="dodge2") +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          axis.title.x = element_blank()) +
    labs(y = "Number") +
    scale_y_continuous(labels = scales::comma) +
    geom_errorbar(aes(ymin=count-count_moe, ymax=count+count_moe),
                  position = position_dodge2(width = 0.9, preserve = "single", padding = .5))
  
  share_plot <- ggplot(dt,
                       aes(x=x_axis_grp,
                           y=share,
                           fill = survey)) +
    geom_bar(stat="identity",
             position="dodge2") +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
          axis.title.x = element_blank()) +
    labs(y = "Share") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    geom_errorbar(aes(ymin=share-share_moe, ymax=share+share_moe),
                  position = position_dodge2(width = 0.9, preserve = "single", padding = .5))
  
  trips <- ggarrange(count_plot, share_plot, 
                     # ncol = 2, nrow = 1,
                     common.legend = TRUE,
                     legend = "right")
  
  annotate_figure(trips, top = text_grob(tbl_name, 
                                         color = "blue", face = "bold", size = 14))
}

count_and_share_plot(gender_data_19_21,
                     "survey",
                     "ESR",
                     "Number of Unemployed",
                     "Number vs Share of Employment Rates")

# facet chart for employment vs sex

gender_age_facet <- create_facet_bar_chart(t= gender_data_test,
                                             w.x="SEX", w.y="share",
                                             f="DATA_SURVEY", g="ESR",
                                             w.color="psrc_light",
                                             est.type ="percent",
                                             w.interactive="no",
                                             w.dec=2,
                                             w.scales="fixed",
                                             w.facet=6,
                                             w.title="Share of Employment by Gender",
                                             w.sub.title="Male or Female")+ 
  ggplot2::theme(axis.title = ggplot2::element_blank()) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))

# hhts data pull & variables

trip_vars <- c('person_id', 'numadults', 'numchildren', 'age', 'age_category', 'gender', 'employment', 
               'education', 'race_category', 'race_eth_poc','race_eth_apoc', 'commute_freq', 'commute_mode', 
               'commute_dur', 'telecommute_freq', 'telecommute_freq_pre_covid', 'mode_freq_1', 'mode_freq_2', 
               'mode_freq_3', 'mode_freq_4', 'mode_freq_5', 'num_trips_linked')

trip_info <- dbReadTable(elmer_connection, SQL("HHSurvey.v_trips"))

# survey names
survey_a <- list(survey = '2017', label = '2017')
survey_b <- list(survey = '2019', label = '2019')
survey_c <- list(survey = '2021', label = '2021')


person_survey_17 <- get_hhts(survey = survey_a$survey,level = "t", vars = trip_vars) 
person_survey_19 <- get_hhts(survey = survey_b$survey,level = "t", vars = trip_vars) 
person_survey_21 <- get_hhts(survey = survey_c$survey,level = "t", vars = trip_vars)

# removing duplicates
gender_17 <-(person_survey_17[!duplicated(person_survey_17$person_id),])
gender_19 <-(person_survey_19[!duplicated(person_survey_19$person_id),])
gender_21 <-(person_survey_21[!duplicated(person_survey_21$person_id),])

# tidy data
#tidy_person_17 <- smp_delivery_combo(person_survey_17, '2017')
#tidy_person_19 <- smp_delivery_combo(person_survey_19, '2019')
#tidy_person_21 <- smp_delivery_combo(person_survey_21, '2021')

# crosstabs for DICHOTOMOUS gender and number of trips taken by year
# unique(person_survey_17$numtrips)

num_trips_gender_17 <- hhts_count(gender_17, spec_wgt = 'trip_weight_2017', 
                                    group_vars = c('gender', 'telecommute_freq')) %>% 
  filter(telecommute_freq != 'Total') %>%
  filter(telecommute_freq != 'NA')

num_trips_gender_19 <- hhts_count(gender_19, spec_wgt = 'hh_weight_2019', 
                                  group_vars = c('gender', 'commute_freq', 'telecommute_freq')) 

num_trips_gender_21 <- hhts_count(gender_21, spec_wgt = 'combined_adult_weight', 
                                  group_vars = c('gender', 'commute_freq', 'telecommute_freq')) 

# merge dfs for 2017, 2017/2019, and 2021 - alternate approach for crosstabs

num_trips_gender_17_21 <- bind_rows(num_trips_gender_17, num_trips_gender_19, num_trips_gender_21) %>%
  mutate(period = as.factor(survey))

# crosstab for income and food delivery 

food_income_column<- static_column_chart(t= all_food_dichot_17_21,
                                         x="hhincome_dichot", y="share",
                                         f="survey",
                                         moe = "share_moe",
                                         color="psrc_pairs",
                                         est ="percent",
                                         dec=1,
                                         title="Food/Meal Deliveries by Income",
                                         subtitle="(e.g.., pizza/sushi, Grubhub)",
                                         source = "PSRC Regional Household Travel Survey")

food_income_column
