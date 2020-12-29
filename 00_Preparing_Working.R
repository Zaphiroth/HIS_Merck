# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Merck
# Purpose:      HIS data cleaning
# programmer:   Zhe Liu
# Date:         2020-12-25
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


options(java.parameters = "-Xmx2048m",
        stringsAsFactors = FALSE)

##---- loading the required packages ----
suppressPackageStartupMessages({
  require(openxlsx)
  require(readxl)
  require(feather)
  require(plyr)
  require(stringi)
  require(feather)
  require(RODBC)
  require(MASS)
  require(car)
  require(data.table)
  require(plotly)
  require(tidyverse)
  require(plm)
  require(dynlm)
  require(randomForest)
  require(lubridate)
  require(forecast)
  require(kknn)
})

##---- setup the directories ----
system("mkdir 01_Background 02_Inputs 03_Outputs 04_Codes 05_Internal_Review 06_Deliveries")
