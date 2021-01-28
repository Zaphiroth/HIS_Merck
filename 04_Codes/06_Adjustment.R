# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Merck
# Purpose:      Adjustment
# programmer:   Zhe Liu
# Date:         2021-01-28
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
## MAX sales
max.sum <- fread('02_Inputs/same_sales_result.csv') %>% 
  mutate(`分组` = if_else(`分组` %in% c('综合医院', '其他'), '综合', `分组`), 
         pack.id = stri_pad_left(pack.id, 7, 0)) %>% 
  group_by(`年`, `季度`, `分组`, `通用名`, `商品名`, `规格`, `包装`, `剂型`, `pack.id`) %>% 
  summarise(`金额（RMB）` = sum(`金额`, na.rm = TRUE), 
            `数量（盒）` = sum(`数量(盒)`, na.rm = TRUE)) %>% 
  ungroup()

## XNT ratio
xnt.ratio.data <- read.xlsx('02_Inputs/Sheet1_XNT_Origin.xlsx')

## SZK ratio
szk.ratio.data <- read.xlsx('02_Inputs/Sheet1_SZK_Origin - 降调节.xlsx')


##---- Prediction ----
max.pred.xnt <- his.forecast.xnt %>% 
  mutate(date = stri_sub(ds, 6, 10), 
         flag = if_else(date == '10-01' & `National Day` == 0, 1, 0)) %>% 
  group_by(date, group) %>% 
  mutate(yhat = if_else(flag == 1, yhat + median(`National Day`), yhat)) %>% 
  ungroup() %>% 
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
  inner_join(max.price.xnt, by = c('cluster', 'year', 'quarter', '分组', 'pack id')) %>% 
  mutate(value = yhat, 
         volume = value / price) %>% 
  select(`年` = year, `季度` = quarter, `分组`, `通用名`, `商品名`, `规格`, 
         `包装`, `剂型`, pack.id = `pack id`, `金额（RMB）` = value, 
         `数量（盒）` = volume)

max.pred.szk <- his.forecast.szk %>% 
  mutate(date = stri_sub(ds, 6, 10), 
         flag = if_else(date == '10-01' & `National Day` == 0, 1, 0)) %>% 
  group_by(date, group) %>% 
  mutate(yhat = if_else(flag == 1, yhat + median(`National Day`), yhat)) %>% 
  ungroup() %>% 
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
  inner_join(max.price.szk, by = c('cluster', 'year', 'quarter', '分组', 'pack id')) %>% 
  mutate(value = yhat, 
         volume = value / price) %>% 
  select(`年` = year, `季度` = quarter, `分组`, `通用名`, `商品名`, `规格`, 
         `包装`, `剂型`, pack.id = `pack id`, `金额（RMB）` = value, 
         `数量（盒）` = volume)


##---- Ratio ----
## pack
xnt.ratio <- xnt.ratio.data %>% 
  group_by(`年`, `季度`, `分组`, pack.id) %>% 
  mutate(ratio_sales = `金额（RMB）` / sum(`金额（RMB）`, na.rm = TRUE), 
         ratio_units = `数量（盒）` / sum(`数量（盒）`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(`年`, `季度`, `分组`, `科室`, `适应症`, pack.id, ratio_sales, ratio_units)

szk.ratio <- szk.ratio.data %>% 
  group_by(`年`, `季度`, `分组`, `科室`, `适应症`, pack.id) %>% 
  summarise(`金额（RMB）` = sum(`金额（RMB）`, na.rm = TRUE), 
            `数量（盒）` = sum(`数量（盒）`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(`年`, `季度`, `分组`, pack.id) %>% 
  mutate(ratio_sales = `金额（RMB）` / sum(`金额（RMB）`, na.rm = TRUE), 
         ratio_units = `数量（盒）` / sum(`数量（盒）`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(`年`, `季度`, `分组`, `科室`, `适应症`, pack.id, ratio_sales, ratio_units)

## base
xnt.ratio.year <- xnt.ratio.data %>% 
  group_by(`年`, `科室`, `适应症`, `商品名`) %>% 
  summarise(`金额（RMB）` = sum(`金额（RMB）`, na.rm = TRUE), 
            `数量（盒）` = sum(`数量（盒）`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(`年`, `商品名`) %>% 
  mutate(ratio_sales = `金额（RMB）` / sum(`金额（RMB）`, na.rm = TRUE), 
         ratio_units = `数量（盒）` / sum(`数量（盒）`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(`年`, `商品名`, `科室`, `适应症`, ratio_sales, ratio_units)

szk.ratio.year <- szk.ratio.data %>% 
  group_by(`年`, `科室`, `适应症`, 商品名) %>% 
  summarise(`金额（RMB）` = sum(`金额（RMB）`, na.rm = TRUE), 
            `数量（盒）` = sum(`数量（盒）`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(`年`, `商品名`) %>% 
  mutate(ratio_sales = `金额（RMB）` / sum(`金额（RMB）`, na.rm = TRUE), 
         ratio_units = `数量（盒）` / sum(`数量（盒）`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(`年`, `商品名`, `科室`, `适应症`, ratio_sales, ratio_units)


##---- Split MAX ----
## XNT
xnt.split1 <- bind_rows(max.sum, max.pred.xnt) %>% 
  filter(`通用名` %in% c('地屈孕酮', '黄体酮')) %>% 
  left_join(xnt.ratio, by = c('年', '季度', '分组', 'pack.id'))

xnt.split2 <- xnt.split1 %>% 
  filter(is.na(ratio_sales)) %>% 
  select(-`科室`, -`适应症`, -ratio_sales, -ratio_units) %>% 
  left_join(xnt.ratio.year, by = c('年', '商品名'))

xnt.split <- xnt.split1 %>% 
  filter(!is.na(ratio_sales)) %>% 
  bind_rows(xnt.split2) %>% 
  mutate(`金额（RMB）` = `金额（RMB）` * ratio_sales, 
         `数量（盒）` = `数量（盒）` * ratio_units) %>% 
  select(`年`, `季度`, `分组`, `科室`, `适应症`, `通用名`, `商品名`, `规格`, 
         `包装`, `剂型`, pack.id, `金额（RMB）`, `数量（盒）`)

write.xlsx(xnt.split, '03_Outputs/XNT_MAX_Pred_Split.xlsx')

## SZK
szk.split1 <- bind_rows(max.sum, max.pred.szk) %>% 
  filter(`通用名` %in% c('戈舍瑞林', '亮丙瑞林', '曲普瑞林')) %>% 
  left_join(szk.ratio, by = c('年', '季度', '分组', 'pack.id'))

szk.split2 <- szk.split1 %>% 
  filter(is.na(ratio_sales)) %>% 
  select(-`科室`, -`适应症`, -ratio_sales, -ratio_units) %>% 
  left_join(szk.ratio.year, by = c('年', '商品名'))

szk.split <- szk.split1 %>% 
  filter(!is.na(ratio_sales)) %>% 
  bind_rows(szk.split2) %>% 
  mutate(`金额（RMB）` = `金额（RMB）` * ratio_sales, 
         `数量（盒）` = `数量（盒）` * ratio_units) %>% 
  select(`年`, `季度`, `分组`, `科室`, `适应症`, `通用名`, `商品名`, `规格`, 
         `包装`, `剂型`, pack.id, `金额（RMB）`, `数量（盒）`)

write.xlsx(szk.split, '03_Outputs/SZK_MAX_Pred_Split.xlsx')




