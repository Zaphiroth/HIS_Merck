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
## diagnosis ratio
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
  select(cluster, year, quarter, dept, diag, packid, value, value_sum, prop)

diag.2020q4.xnt <- diag.dist.xnt %>% 
  filter(year == 2020, quarter == 'Q3') %>% 
  mutate(quarter = 'Q4')

diag.2021.xnt <- diag.dist.xnt %>% 
  filter(year == 2020) %>% 
  bind_rows(diag.2020q4.xnt) %>% 
  group_by(cluster, dept, diag, packid) %>% 
  mutate(value = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(cluster, packid) %>% 
  mutate(value_sum = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(prop = value / value_sum, 
         year = 2021)

diag.ratio.xnt <- bind_rows(diag.dist.xnt, diag.2020q4.xnt, diag.2021.xnt)

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
  select(cluster, year, quarter, dept, diag, packid, value, value_sum, prop)

diag.2020q4.szk <- diag.dist.szk %>% 
  filter(year == 2020, quarter == 'Q3') %>% 
  mutate(quarter = 'Q4')

diag.2021.szk <- diag.dist.szk %>% 
  filter(year == 2020) %>% 
  bind_rows(diag.2020q4.szk) %>% 
  group_by(cluster, dept, diag, packid) %>% 
  mutate(value = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(cluster, packid) %>% 
  mutate(value_sum = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(prop = value / value_sum, 
         year = 2021)

diag.ratio.szk <- bind_rows(diag.dist.szk, diag.2020q4.szk, diag.2021.szk)

## MAX clustered
max.cluster.xnt <- universe.cluster.xnt %>% 
  left_join(max.data, by = c('pha' = 'PHA.code')) %>% 
  inner_join(diag.ratio.xnt, by = c('cluster', '年' = 'year', '季度' = 'quarter', 
                                    'pack.id' = 'packid')) %>% 
  mutate(value = `金额` * prop, 
         volume = `数量(盒)` * prop)

max.cluster.szk <- universe.cluster.szk %>% 
  left_join(max.data, by = c('pha' = 'PHA.code')) %>% 
  inner_join(diag.ratio.szk, by = c('cluster', '年' = 'year', '季度' = 'quarter', 
                                    'pack.id' = 'packid')) %>% 
  mutate(value = `金额` * prop, 
         volume = `数量(盒)` * prop)


##---- Delivery format ----
## history data
max.xnt <- max.cluster.xnt %>% 
  group_by(cluster, `年`, `季度`, `分组`, `通用名`, `商品名`, `规格`, `包装`, 
           `剂型`, `pack id` = pack.id) %>% 
  summarise(`金额（RMB）` = sum(value, na.rm = TRUE), 
            `数量（盒）` = sum(volume, na.rm = TRUE)) %>% 
  ungroup() %>% 
  unite(quarter, `年`, `季度`, sep = '') %>% 
  pivot_wider(names_from = quarter, 
              values_from = c(`金额（RMB）`, `数量（盒）`), 
              values_fill = 0) %>% 
  pivot_longer(cols = starts_with(c('金额', '数量')), 
               names_to = c('type', 'quarter'), 
               names_pattern = '(.*)_(.*)', 
               values_to = 'value') %>% 
  pivot_wider(names_from = type, 
              values_from = value)

max.szk <- max.cluster.szk %>% 
  group_by(cluster, `年`, `季度`, `分组`, `通用名`, `商品名`, `规格`, `包装`, 
           `剂型`, `pack id` = pack.id) %>% 
  summarise(`金额（RMB）` = sum(value, na.rm = TRUE), 
            `数量（盒）` = sum(volume, na.rm = TRUE)) %>% 
  ungroup() %>% 
  unite(quarter, `年`, `季度`, sep = '') %>% 
  pivot_wider(names_from = quarter, 
              values_from = c(`金额（RMB）`, `数量（盒）`), 
              values_fill = 0) %>% 
  pivot_longer(cols = starts_with(c('金额', '数量')), 
               names_to = c('type', 'quarter'), 
               names_pattern = '(.*)_(.*)', 
               values_to = 'value') %>% 
  pivot_wider(names_from = type, 
              values_from = value)

## price
max.price.xnt <- max.xnt %>% 
  filter(quarter == '2020Q3') %>% 
  select(-quarter) %>% 
  merge(data.frame(quarter = c('2020Q4', '2021Q1', '2021Q2', '2021Q3', '2021Q4'))) %>% 
  bind_rows(max.xnt) %>% 
  mutate(year = as.numeric(stri_sub(quarter, 1, 4)), 
         quarter = stri_sub(quarter, 5, 6), 
         price = `金额（RMB）` / `数量（盒）`) %>% 
  filter(!is.na(price), !is.infinite(price)) %>% 
  select(cluster, `分组`, `通用名`, `商品名`, `规格`, `包装`, `剂型`, 
         `pack id`, year, quarter, price)

max.price.szk <- max.szk %>% 
  filter(quarter == '2020Q3') %>% 
  select(-quarter) %>% 
  merge(data.frame(quarter = c('2020Q4', '2021Q1', '2021Q2', '2021Q3', '2021Q4'))) %>% 
  bind_rows(max.szk) %>% 
  mutate(year = as.numeric(stri_sub(quarter, 1, 4)), 
         quarter = stri_sub(quarter, 5, 6), 
         price = `金额（RMB）` / `数量（盒）`) %>% 
  filter(!is.na(price), !is.infinite(price)) %>% 
  select(cluster, `分组`, `通用名`, `商品名`, `规格`, `包装`, `剂型`, 
         `pack id`, year, quarter, price)

## forecast
source('04_Codes/HISForecast.R', encoding = 'UTF-8')

his.forecast.xnt <- his_forecasting_model_1(data = max.xnt, 
                                            training_window = c('2018-01-01', '2020-07-01'), 
                                            period = 5, 
                                            metric = '金额（RMB）')

his.forecast.szk <- his_forecasting_model_1(data = max.szk, 
                                            training_window = c('2018-01-01', '2020-07-01'), 
                                            period = 5, 
                                            metric = '金额（RMB）')

## sheet 1
sheet.year.ratio.xnt <- his.forecast.xnt %>% 
  select(Date = ds, yhat, group) %>% 
  separate(group, c('cluster', '分组', 'pack id'), sep = '[+]') %>% 
  mutate(quarter = stri_sub(Date, 6, 7), 
         quarter = case_when(quarter %in% c('01', '02', '03') ~ 'Q1', 
                             quarter %in% c('04', '05', '06') ~ 'Q2', 
                             quarter %in% c('07', '08', '09') ~ 'Q3', 
                             quarter %in% c('10', '11', '12') ~ 'Q4', 
                             TRUE ~ NA_character_), 
         year = as.numeric(stri_sub(Date, 1, 4))) %>% 
  filter(stri_paste(year, quarter) %in% c('2020Q4', '2021Q1', '2021Q2', '2021Q3', '2021Q4')) %>% 
  inner_join(diag.ratio.xnt, by = c('cluster', 'year', 'quarter', 'pack id' = 'packid')) %>% 
  inner_join(max.price.xnt, by = c('cluster', 'year', 'quarter', '分组', 'pack id')) %>% 
  mutate(value = yhat * prop, 
         volume = value / price) %>% 
  rename(`年` = year, 
         pack.id = `pack id`) %>% 
  bind_rows(max.cluster.xnt) %>% 
  group_by(`年`, `分组`, `适应症` = diag, `通用名`, `商品名`, `规格`, `包装`, 
           `剂型`, `pack id` = pack.id) %>% 
  summarise(`金额（RMB）` = sum(value, na.rm = TRUE), 
            `数量（盒）` = sum(volume, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(`年`, `分组`, `通用名`, `商品名`, `规格`, `包装`, `剂型`, `pack id`) %>% 
  mutate(value_sum = sum(`金额（RMB）`, na.rm = TRUE), 
         volume_sum = sum(`数量（盒）`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(`金额（Ratio）` = `金额（RMB）` / value_sum, 
         `数量（Ratio）` = `数量（盒）` / volume_sum) %>% 
  select(`年`, `分组`, `适应症`, `通用名`, `商品名`, `规格`, `包装`, `剂型`, 
         `pack id`, `金额（Ratio）`, `数量（Ratio）`)

sheet.year.ratio.szk <- his.forecast.szk %>% 
  select(Date = ds, yhat, group) %>% 
  separate(group, c('cluster', '分组', 'pack id'), sep = '[+]') %>% 
  mutate(quarter = stri_sub(Date, 6, 7), 
         quarter = case_when(quarter %in% c('01', '02', '03') ~ 'Q1', 
                             quarter %in% c('04', '05', '06') ~ 'Q2', 
                             quarter %in% c('07', '08', '09') ~ 'Q3', 
                             quarter %in% c('10', '11', '12') ~ 'Q4', 
                             TRUE ~ NA_character_), 
         year = as.numeric(stri_sub(Date, 1, 4))) %>% 
  filter(stri_paste(year, quarter) %in% c('2020Q4', '2021Q1', '2021Q2', '2021Q3', '2021Q4')) %>% 
  inner_join(diag.ratio.szk, by = c('cluster', 'year', 'quarter', 'pack id' = 'packid')) %>% 
  inner_join(max.price.szk, by = c('cluster', 'year', 'quarter', '分组', 'pack id')) %>% 
  mutate(value = yhat * prop, 
         volume = value / price) %>% 
  rename(`年` = year, 
         pack.id = `pack id`) %>% 
  bind_rows(max.cluster.szk) %>% 
  group_by(`年`, `分组`, `适应症` = diag, `通用名`, `商品名`, `规格`, `包装`, 
           `剂型`, `pack id` = pack.id) %>% 
  summarise(`金额（RMB）` = sum(value, na.rm = TRUE), 
            `数量（盒）` = sum(volume, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(`年`, `分组`, `通用名`, `商品名`, `规格`, `包装`, `剂型`, `pack id`) %>% 
  mutate(value_sum = sum(`金额（RMB）`, na.rm = TRUE), 
         volume_sum = sum(`数量（盒）`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(`金额（Ratio）` = `金额（RMB）` / value_sum, 
         `数量（Ratio）` = `数量（盒）` / volume_sum) %>% 
  select(`年`, `分组`, `适应症`, `通用名`, `商品名`, `规格`, `包装`, `剂型`, 
         `pack id`, `金额（Ratio）`, `数量（Ratio）`)

## sheet 2
sheet.quarter.value.xnt <- max.cluster.xnt %>% 
  group_by(`年`, `季度`, `诊断` = diag, `科室` = dept, `通用名`, `商品名`, 
           `规格`, `包装`, `剂型`, `pack id` = pack.id) %>% 
  summarise(`金额（RMB）` = sum(value, na.rm = TRUE), 
            `数量（盒）` = sum(volume, na.rm = TRUE)) %>% 
  ungroup()

sheet.quarter.value.szk <- max.cluster.szk %>% 
  group_by(`年`, `季度`, `诊断` = diag, `科室` = dept, `通用名`, `商品名`, 
           `规格`, `包装`, `剂型`, `pack id` = pack.id) %>% 
  summarise(`金额（RMB）` = sum(value, na.rm = TRUE), 
            `数量（盒）` = sum(volume, na.rm = TRUE)) %>% 
  ungroup()

## sheet 3
sheet.quarter.ratio.xnt <- max.cluster.xnt %>% 
  group_by(`年`, `季度`, `分组` = group, `诊断` = diag, `科室` = dept, `通用名`, 
           `商品名`, `规格`, `包装`, `剂型`, `pack id` = pack.id) %>% 
  summarise(`金额（RMB）` = sum(value, na.rm = TRUE), 
            `数量（盒）` = sum(volume, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(`年`, `季度`, `分组`, `诊断`, `科室`) %>% 
  mutate(value_sum = sum(`金额（RMB）`, na.rm = TRUE), 
         volume_sum = sum(`数量（盒）`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(`金额（Ratio)` = `金额（RMB）` / value_sum, 
         `数量（Ratio）` = `数量（盒）` / volume_sum) %>% 
  select(-`金额（RMB）`, -`数量（盒）`, -value_sum, -volume_sum)

sheet.quarter.ratio.szk <- max.cluster.szk %>% 
  group_by(`年`, `季度`, `分组` = group, `诊断` = diag, `科室` = dept, `通用名`, 
           `商品名`, `规格`, `包装`, `剂型`, `pack id` = pack.id) %>% 
  summarise(`金额（RMB）` = sum(value, na.rm = TRUE), 
            `数量（盒）` = sum(volume, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(`年`, `季度`, `分组`, `诊断`, `科室`) %>% 
  mutate(value_sum = sum(`金额（RMB）`, na.rm = TRUE), 
         volume_sum = sum(`数量（盒）`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(`金额（Ratio)` = `金额（RMB）` / value_sum, 
         `数量（Ratio）` = `数量（盒）` / volume_sum) %>% 
  select(-`金额（RMB）`, -`数量（盒）`, -value_sum, -volume_sum)


##---- Write out ----
wb <- createWorkbook()
addWorksheet(wb, 'Sheet1_XNT')
addWorksheet(wb, 'Sheet1_SZK')
addWorksheet(wb, 'Sheet2_XNT')
addWorksheet(wb, 'Sheet2_SZK')
addWorksheet(wb, 'Sheet3_XNT')
addWorksheet(wb, 'Sheet3_SZK')
writeDataTable(wb, 'Sheet1_XNT', sheet.year.ratio.xnt)
writeDataTable(wb, 'Sheet1_SZK', sheet.year.ratio.szk)
writeDataTable(wb, 'Sheet2_XNT', sheet.quarter.value.xnt)
writeDataTable(wb, 'Sheet2_SZK', sheet.quarter.value.szk)
writeDataTable(wb, 'Sheet3_XNT', sheet.quarter.ratio.xnt)
writeDataTable(wb, 'Sheet3_SZK', sheet.quarter.ratio.szk)
saveWorkbook(wb, '03_Outputs/Merk_HIS_Delivery.xlsx', overwrite = TRUE)





