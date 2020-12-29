library(openxlsx)
library(tidyverse)



## readin cleaning rule

xnt_cleaning_rule <- 
  read.xlsx("02_Inputs/雪诺同诊断清洗规则.xlsx", startRow = 2, rows = 2:9)

xnt_cleaning_rule_m <- xnt_cleaning_rule %>%
  # dplyr::rowwise(疾病诊断优先顺序) %>%
  # summarise(关键词 = paste(c_cross(关键词1:关键词9), collapse = "|"))
  pivot_longer(关键词1:关键词9, names_to = "tmp", values_to = "关键词") %>%
  filter(!is.na(关键词)) %>%
  group_by(疾病诊断优先顺序) %>%
  summarise(关键词 = paste(关键词, collapse = "|")) %>%
  ungroup() 


szk_cleaning_rule <- read.xlsx("02_Inputs/思则凯诊断清洗规则.xlsx", rows = 1:10)

szk_cleaning_rule_m <- szk_cleaning_rule %>%
  # dplyr::rowwise(疾病诊断优先顺序) %>%
  # summarise(关键词 = paste(c_cross(关键词1:关键词9), collapse = "|"))
  pivot_longer(关键词1:关键词5, names_to = "tmp", values_to = "关键词") %>%
  filter(!is.na(关键词)) %>%
  group_by(疾病诊断优先顺序) %>%
  summarise(关键词 = paste(关键词, collapse = "|")) %>%
  ungroup() 

## readin his data
xnt_his_data <- read.xlsx("02_Inputs/雪诺同HIS原始数据.xlsx")
szk_his_data <- read.xlsx("02_Inputs/思则凯HIS原始数据.xlsx")
  