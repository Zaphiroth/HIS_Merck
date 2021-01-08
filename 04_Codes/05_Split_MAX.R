# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Merck
# Purpose:      Split MAX based on diagnosis
# programmer:   Zhe Liu
# Date:         2021-01-08
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
## MAX sales
max.data <- fread('02_Inputs/same_sales_result.csv') %>% 
  mutate(`分组` = if_else(`分组` == '综合医院', '综合', `分组`), 
         pack.id = stri_pad_left(pack.id, 7, 0))

## sample HIS
xnt_his_data_m <- read.xlsx('02_Inputs/XNT_HIS_Sample.xlsx')
szk_his_data_m <- read.xlsx('02_Inputs/SZK_HIS_Sample.xlsx')

## universe cluster
universe.cluster.xnt <- read.xlsx('02_Inputs/Universe_Cluster_Result.xlsx', sheet = 'XNT')
universe.cluster.szk <- read.xlsx('02_Inputs/Universe_Cluster_Result.xlsx', sheet = 'SZK')


##---- Cluster diagnosis distribution ----
## diagnosis
diag.dist.xnt <- universe.cluster.xnt %>% 
  left_join(xnt_his_data_m, by = 'pha') %>% 
  mutate(quarter = case_when(`月` %in% c(1, 2, 3) ~ 'Q1', 
                             `月` %in% c(4, 5, 6) ~ 'Q2', 
                             `月` %in% c(7, 8, 9) ~ 'Q3', 
                             `月` %in% c(10, 11, 12) ~ 'Q4', 
                             TRUE ~ NA_character_)) %>% 
  group_by(cluster, year = `年`, quarter, dept = `清洗后科室`, diag = `诊断`, 
           packid = Pack.id) %>% 
  summarise(value = sum(`金额`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(value > 0) %>% 
  group_by(cluster, year, quarter, packid) %>% 
  mutate(value_sum = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(prop = value / value_sum, 
         packid = stri_pad_left(packid, 7, 0)) %>% 
  select(cluster, year, quarter, dept, diag, packid, prop)

diag.dist.szk <- universe.cluster.szk %>% 
  left_join(szk_his_data_m, by = 'pha') %>% 
  mutate(quarter = case_when(`月` %in% c(1, 2, 3) ~ 'Q1', 
                             `月` %in% c(4, 5, 6) ~ 'Q2', 
                             `月` %in% c(7, 8, 9) ~ 'Q3', 
                             `月` %in% c(10, 11, 12) ~ 'Q4', 
                             TRUE ~ NA_character_)) %>% 
  group_by(cluster, year = `年`, quarter, dept = `最终科室`, diag = `诊断`, 
           packid = pack.id) %>% 
  summarise(value = sum(`金额`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(value > 0) %>% 
  group_by(cluster, year, quarter, packid) %>% 
  mutate(value_sum = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(prop = value / value_sum, 
         packid = stri_pad_left(packid, 7, 0)) %>% 
  select(cluster, year, quarter, dept, diag, packid, prop)

## MAX clustered
max.cluster.xnt <- universe.cluster.xnt %>% 
  left_join(max.data, by = c('pha' = 'PHA.code')) %>% 
  inner_join(diag.dist.xnt, by = c('cluster', '年' = 'year', '季度' = 'quarter', 
                                   'pack.id' = 'packid')) %>% 
  mutate(value = `金额` * prop, 
         volume = `数量(盒)` * prop)

max.cluster.szk <- universe.cluster.szk %>% 
  left_join(max.data, by = c('pha' = 'PHA.code')) %>% 
  inner_join(diag.dist.szk, by = c('cluster', '年' = 'year', '季度' = 'quarter', 
                                   'pack.id' = 'packid')) %>% 
  mutate(value = `金额` * prop, 
         volume = `数量(盒)` * prop)


##---- Delivery format ----
## sheet 1

## sheet 2
sheet.quarter.value.xnt <- max.cluster.xnt %>% 
  group_by(`年`, `季度`, `诊断` = diag, `科室` = dept, `通用名`, `商品名`, 
           `规格`, `包装`, `剂型`, `pack id` = pack.id) %>% 
  summarise(`金额（RMB)` = sum(value, na.rm = TRUE), 
            `数量（盒）` = sum(volume, na.rm = TRUE)) %>% 
  ungroup()

sheet.quarter.value.szk <- max.cluster.szk %>% 
  group_by(`年`, `季度`, `诊断` = diag, `科室` = dept, `通用名`, `商品名`, 
           `规格`, `包装`, `剂型`, `pack id` = pack.id) %>% 
  summarise(`金额（RMB)` = sum(value, na.rm = TRUE), 
            `数量（盒）` = sum(volume, na.rm = TRUE)) %>% 
  ungroup()

## sheet 3
sheet.quarter.ratio <- max.cluster.xnt %>% 
  group_by(`年`, `季度`, `分组` = group, `诊断` = diag, `科室` = dept, `通用名`, 
           `商品名`, `规格`, `包装`, `剂型`, `pack id` = pack.id) %>% 
  summarise(`金额（RMB)` = sum(value, na.rm = TRUE), 
            `数量（盒）` = sum(volume, na.rm = TRUE)) %>% 
  ungroup()












