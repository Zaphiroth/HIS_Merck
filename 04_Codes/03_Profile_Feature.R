# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Merck
# Purpose:      Feature extraction
# programmer:   Zhe Liu
# Date:         2021-01-04
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Hospital profile ----
hosp.pf.raw <- read.xlsx('02_Inputs/2020年Universe更新维护_20200729.xlsx', 
                          sheet = 'Universe2020', check.names = TRUE)

# hosp.pf <- hosp.pf.raw %>% 
#   select(pha = `新版ID...8`, level = Hosp_level, property = `性质`, 
#          region = Region, location, province = Province, city = City, 
#          district = District, type = Type, top = `百强县`, county = `county HP`, 
#          tier = `City Tier 2010`, specialty1 = `Specialty_1_标准化`, 
#          specialty2 = `Specialty_2_标准化`, est = Est_DrugIncome_RMB, 
#          doctors = `医生数...40`, beds = `床位数`, general_beds = `全科床位数`, 
#          internal_beds = `内科床位数`, surgery_beds = `外科床位数`, 
#          ophth_beds = `眼科床位数`, visits = `年诊疗人次...47`, 
#          outpatient_visits = `门诊诊次...48`, internal_visits = `内科诊次`, 
#          surgery_visits = `外科诊次`, admissions = `入院人数`, 
#          surgery_adm = `住院病人手术人次数`, income = `医疗收入`, 
#          outpatient_income = `门诊收入`)

hosp.pf <- hosp.pf.raw %>% 
  mutate(`开放床位数` = as.numeric(`开放床位数`), 
         `年诊疗人次.1` = as.numeric(`年诊疗人次.1`), 
         `医生数`  = ifelse(is.na(`医生数`) & `医生数.1` > 0, 
                         `医生数.1`, `医生数`), 
         `医生数` = if_else(`医生数` == 0 & `医生数.1` > 0, 
                         `医生数.1`, `医生数`), 
         `床位数` = if_else(is.na(`床位数`) & `开放床位数` > 0, 
                         `开放床位数`, `床位数`), 
         `床位数` = if_else(`床位数` == 0 & `开放床位数` > 0, 
                         `开放床位数`, `床位数`), 
         `年诊疗人次` = if_else(is.na(`年诊疗人次`) & `年诊疗人次.1` > 0, 
                           `年诊疗人次.1`, `年诊疗人次`), 
         `年诊疗人次` = if_else(`年诊疗人次` == 0 & `年诊疗人次.1` > 0, 
                           `年诊疗人次.1`, `年诊疗人次`), 
         `门诊诊次` = if_else(is.na(`门诊诊次`) & `门诊诊次.1` > 0, 
                          `门诊诊次.1`, `门诊诊次`), 
         `门诊诊次` = if_else(`门诊诊次` == 0 & `门诊诊次.1` > 0, 
                          `门诊诊次.1`, `门诊诊次`)) %>% 
  select(pha = `新版ID`, Hosp_level, `性质`, Region, location, Province, 
         City, District, Type, `百强县`, `county.HP`, `City.Tier.2010`, 
         `Specialty_1_标准化`, `Specialty_2_标准化`, Est_DrugIncome_RMB, 
         `医生数`, `床位数`, `全科床位数`, `内科床位数`, `外科床位数`, 
         `眼科床位数`, `年诊疗人次`, `门诊诊次`, `内科诊次`, `外科诊次`, 
         `入院人数`, `住院病人手术人次数`, `医疗收入`, `门诊收入`, `门诊治疗收入`, 
         `门诊手术收入`, `住院收入`, `住院床位收入`, `住院治疗收入`, 
         `住院手术收入`, `药品收入`, `门诊药品收入`, `门诊西药收入`, 
         `住院药品收入`, `住院西药收入`) %>% 
  group_by(pha) %>% 
  summarise_all(function(x) {
    first(na.omit(x))
  }) %>% 
  ungroup()

## sample profile
xnt.pf <- xnt.cluster %>% 
  select(cluster, pha, rp, obs, onc) %>% 
  left_join(hosp.pf, by = 'pha')

szk.pf <- szk.cluster %>% 
  select(cluster, pha, rp, obs, onc) %>% 
  left_join(hosp.pf, by = 'pha')

write.xlsx(xnt.pf, '03_Outputs/XNT_Sample_Profile.xlsx')
write.xlsx(szk.pf, '03_Outputs/SZK_Sample_Profile.xlsx')


##---- MAX sales ----
## sales data
max.sales <- fread('02_Inputs/same_sales_result.csv')

xnt.max <- filter(max.sales, PHA.code %in% xnt.cluster2$pha)
szk.max <- filter(max.sales, PHA.code %in% szk.cluster2$pha)

# write.xlsx(xnt.max, '03_Outputs/XNT_Sales.xlsx')
# write.xlsx(szk.max, '03_Outputs/SZK_Sales.xlsx')

## sales proportion
xnt.sales <- xnt.cluster2 %>% 
  select(cluster, pha, rp, obs, onc) %>% 
  left_join(xnt.max, by = c('pha' = 'PHA.code'))
# %>% 
#   mutate(xnt = if_else(`通用名` == '地屈孕酮', `金额`, 0)) %>% 
#   group_by(cluster, pha) %>% 
#   summarise(market = sum(`金额`, na.rm = TRUE), 
#             xnt = sum(xnt, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   mutate(prop = xnt / market)

szk.sales <- szk.cluster2 %>% 
  select(cluster, pha, rp, obs, onc) %>% 
  left_join(szk.max, by = c('pha' = 'PHA.code'))
# %>% 
#   mutate(szk = if_else(`商品名` == '雪诺同', `金额`, 0)) %>% 
#   group_by(cluster, pha) %>% 
#   summarise(market = sum(`金额`, na.rm = TRUE), 
#             szk = sum(szk, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   mutate(prop = szk / market)

write.xlsx(xnt.sales, '03_Outputs/XNT_Sales.xlsx')
write.xlsx(szk.sales, '03_Outputs/SZK_Sales.xlsx')











