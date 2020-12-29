# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Merck
# Purpose:      Clustering
# programmer:   Zhe Liu
# Date:         2020-12-29
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Variable selection ----
xnt.var <- xnt_his_data_m %>% 
  distinct(dept = `清洗后科室`, diag = `诊断`) %>% 
  arrange(dept, diag) %>% 
  mutate(var_name = paste0('Var', row_number()))

szk.var <- szk_his_data_m %>% 
  distinct(dept = `最终科室`, diag = `诊断`) %>% 
  arrange(dept, diag) %>% 
  mutate(var_name = paste0('Var', row_number()))


##---- Sample data ----
xnt.sample <- xnt_his_data_m %>% 
  group_by(pha, rp, dept = `清洗后科室`, diag = `诊断`) %>% 
  summarise(value = sum(`金额`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(pha) %>% 
  mutate(value_sum = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(xnt.var, by = c('dept', 'diag')) %>% 
  mutate(value_prop = value / value_sum) %>% 
  pivot_wider(id_cols = c(pha, rp), 
              names_from = var_name, 
              values_from = value_prop, 
              values_fill = 0)

szk.sample



