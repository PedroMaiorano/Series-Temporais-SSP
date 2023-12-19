# Bibliotecas
library(shiny)
library(data.table)
library(shinyjs)
library(future)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(tidyverse)
library(fpp3)
library(readxl)
library(plotly)
library(knitr)
library(kableExtra)
library(readr)
library(ggpubr)
library(forecast)
library(tseries)
library(glue)
library(trend)
library(aTSA)
library(funtimes)
library(seasonal)
library(seastests)
library(future.apply)
library(shinycssloaders)
library(shinyalert)
plan(multisession)

#install.packages("tseries",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("glue",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("trend",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("aTSA",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("forecast",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

#install.packages("funtimes",lib="/home/shiny/R/x86_64-pc-linux-gnu-library/4.0")

