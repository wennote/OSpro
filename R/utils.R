######################################################################################
### 1) library
library('htmlwidgets') # Shiny helper
library('shiny') # Create the shiny app
library('shinydashboard') # Add tabs to shiny apps
library('rmarkdown')
library("knitr")
library('dplyr',warn.conflicts = FALSE) # Dataframe manipulation
library('ggplot2')
library('ggpubr')
library('tidyr')
library('purrr')
library('survival') # Survival analysis, Cox PH
library('xtable') # Present summary tables
library('htmlTable') # Present summary tables
library('forestplot') # For the forest plot
library('DT')
library('shinythemes')
library("reshape2")
library("haven")
library("lattice")
library("binom")
library("Hmisc",warn.conflicts = FALSE)
library("grid")
library("tools")
library("magrittr")
library("gridExtra")
library("survminer")
library("shinydashboard")
library("stargazer")
library("shinyBS")
library("htmltools")
library('clinUtils')
library('diffobj')
library('broom')
library("sasLM")
library("ggh4x")
library('ggprism')
library('ggeasy')
library("GenBinomApps")
library('emmeans')
library('rstatix')
library('readxl')
library('forcats')
library('lubridate')

options(dplyr.summarise.inform=FALSE)

### Functions

linebreaks <- function(n){HTML(strrep(br(), n))}

f_num_date <- function(n) {as.Date(n, origin="1970-01-01")}

### transform file

# f_enroll <- function(accrual,x) {
#
#   accrual$var <- accrual[,x]
#
#   accrual %>%
#     mutate(month=Month -1,
#            across(Month:var,~ .-c(0,lag(.)[-1])),
#            var=var + 0.0000001) %>%
#     select(month,var)
#
# }


### data manupulation

# main.path <- "C:/Users/wzhou/OneDrive - Moderna/Documents/R/temp/"
# main.path2 <- "C:/Users/wzhou/OneDrive - Moderna/Documents/R/ttepro/"
#
# setwd(main.path)
#
# accrual0<-read.csv("Accural inforamtion v2.csv")
#
# save(accrual0,file=paste0(main.path2,"data/","accrual0.rda"))
#
# accrual <- accrual0 %>%
#   bind_cols(1:nrow(.))%>%
#   rename(Month=...14,
#          total=S0total,
#          WT=S0WT,
#          MT=S0MT) %>%
#   mutate(month=Month) %>%
#   select(date,Month,month,total,WT,MT)
#
# #accrual[is.na(accrual)] <- 350
#
# accrual <- accrual %>%
#   mutate_at(c("total","WT","MT","Month"),funs(.-c(0,lag(.)[-1]))) %>%
#   mutate(total=total + 0.0000001,
#          WT=WT +0.0000001,
#          MT=MT + 0.0000001) %>%
#   select(date,month,total,WT,MT)
#
# save(accrual,file=paste0(main.path2,"data/","accrual.rda"))
# load(paste0(main.path2,"data/","accrual0.rda"))
