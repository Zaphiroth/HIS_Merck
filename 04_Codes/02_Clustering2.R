# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Merck
# Purpose:      Clustering2
# programmer:   Zhe Liu
# Date:         2021-01-04
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Variable selection ----
xnt.var2 <- xnt_his_data_m %>% 
  distinct(diag = `诊断`) %>% 
  arrange(diag) %>% 
  mutate(var_name = paste0('Var', row_number()))

szk.var2 <- szk_his_data_m %>% 
  distinct(diag = `诊断`) %>% 
  arrange(diag) %>% 
  mutate(var_name = paste0('Var', row_number()))


##---- Sample data ----
xnt.sample2 <- xnt_his_data_m %>% 
  group_by(pha, diag = `诊断`) %>% 
  summarise(value = sum(`金额`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(pha) %>% 
  mutate(value_sum = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(value_prop = value / value_sum) %>% 
  left_join(xnt.var2, by = c('diag')) %>% 
  pivot_wider(id_cols = c(pha), 
              names_from = var_name, 
              values_from = value_prop, 
              values_fill = 0)

szk.sample2 <- szk_his_data_m %>% 
  group_by(pha, diag = `诊断`) %>%
  summarise(value = sum(`金额`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(pha) %>% 
  mutate(value_sum = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(value_prop = value / value_sum) %>% 
  left_join(szk.var2, by = c('diag')) %>% 
  pivot_wider(id_cols = c(pha), 
              names_from = var_name, 
              values_from = value_prop, 
              values_fill = 0)


##---- XNT Clustering ----
## number of clusters
xnt.nb.silhouette2 <- fviz_nbclust(xnt.sample2[, -1],
                                   kmeans,
                                   method = 'silhouette',
                                   k.max = 10,
                                   nboot = 1000,
                                   verbose = TRUE,
                                   print.summary = TRUE)

xnt.nb.wss2 <- fviz_nbclust(xnt.sample2[, -1],
                            kmeans,
                            method = 'wss',
                            k.max = 10,
                            nboot = 1000,
                            verbose = TRUE,
                            print.summary = TRUE)

xnt.nb.gap2 <- fviz_nbclust(xnt.sample2[, -1],
                            kmeans,
                            method = 'gap_stat',
                            k.max = 10,
                            nboot = 1000,
                            verbose = TRUE,
                            print.summary = TRUE)

## number of clusters
k <- 9

## mclust
xnt_mclust2 <- Mclust(xnt.sample2[, -1], k)
fviz_cluster(xnt_mclust2, xnt.sample2[, -1])

## kmeans
xnt_kmeans2 <- kmeans(xnt.sample2[, -1], k, iter.max = 1000L, nstart = 50L, 
                      algorithm = "Hartigan-Wong", trace = FALSE)
fviz_cluster(xnt_kmeans2, xnt.sample2[, -1])

## 选择kmeans
xnt.cluster2 <- data.frame(cluster = xnt_kmeans2$cluster) %>% 
  bind_cols(xnt.sample2) %>% 
  left_join(hosp.code, by = 'pha') %>% 
  select(cluster, pha, rp, obs, onc, starts_with('Var'))


wb <- createWorkbook()
addWorksheet(wb, 'Cluster')
addWorksheet(wb, 'Var')
writeDataTable(wb, 'Cluster', xnt.cluster2)
writeDataTable(wb, 'Var', xnt.var2)
saveWorkbook(wb, '03_Outputs/XNT_Cluster2.xlsx', overwrite = TRUE)


##---- SZK Clustering ----
## number of clusters
szk.nb.silhouette2 <- fviz_nbclust(szk.sample2[, -1], 
                                   kmeans, 
                                   method = 'silhouette', 
                                   k.max = 20, 
                                   verbose = TRUE, 
                                   print.summary = TRUE)

szk.nb.wss2 <- fviz_nbclust(szk.sample2[, -1], 
                            kmeans, 
                            method = 'wss', 
                            k.max = 20, 
                            verbose = TRUE, 
                            print.summary = TRUE)

szk.nb.gap2 <- fviz_nbclust(szk.sample2[, -1], 
                            kmeans, 
                            method = 'gap_stat', 
                            k.max = 20, 
                            nboot = 1000, 
                            verbose = TRUE, 
                            print.summary = TRUE)

## k-means
# k = 9
szk.kmeans2 <- kmeans(szk.sample[, -1], 
                      centers = 9, 
                      iter.max = 10, 
                      nstart = 30, 
                      algorithm = 'Hartigan-Wong', 
                      trace = FALSE)

fviz_cluster(szk.kmeans2, szk.sample2[, -1], 
             ellipse = TRUE, 
             stand = FALSE, repel = FALSE) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_vline(aes(xintercept = 0), linetype = "dashed")

szk.cluster2 <- data.frame(cluster = szk.kmeans2$cluster) %>% 
  bind_cols(szk.sample2) %>% 
  left_join(hosp.code, by = 'pha') %>% 
  select(cluster, pha, rp, obs, onc, starts_with('Var'))

## result
wb <- createWorkbook()
addWorksheet(wb, 'Cluster')
addWorksheet(wb, 'Var')
writeDataTable(wb, 'Cluster', szk.cluster2)
writeDataTable(wb, 'Var', szk.var2)
saveWorkbook(wb, '03_Outputs/SZK_Cluster2.xlsx', overwrite = TRUE)

## GMM
# szk.bic2 <- mclustBIC(szk.sample2[, -1])
# plot(szk.bic2)
# summary(szk.bic2)
# 
# szk.icl2 <- mclustICL(szk.sample2[, -1])
# plot(szk.icl2)
# summary(szk.icl2)
# 
# szk.gmm2 <- Mclust(szk.sample2[, -1], 9)
# 
# fviz_cluster(szk.gmm2, szk.sample2[, -1])
# 
# szk.cluster2 <- data.frame(cluster = szk.gmm2$classification) %>% 
#   bind_cols(szk.sample2) %>% 
#   left_join(hosp.code, by = 'pha') %>% 
#   select(cluster, pha, rp, obs, onc, starts_with('Var'))


##---- Profile ----
hosp.pf.raw <- read.xlsx('02_Inputs/2020年Universe更新维护_20200729.xlsx', 
                         sheet = 'Universe2020', check.names = TRUE)

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
xnt.pf2 <- read.xlsx('03_Outputs/XNT_Cluster2.xlsx') %>% 
  select(cluster, pha, rp, obs, onc) %>% 
  left_join(hosp.pf, by = 'pha')

szk.pf2 <- read.xlsx('03_Outputs/SZK_Cluster2.xlsx') %>% 
  select(cluster, pha, rp, obs, onc) %>% 
  left_join(hosp.pf, by = 'pha')

write.xlsx(xnt.pf2, '03_Outputs/XNT_Sample_Profile2.xlsx')
write.xlsx(szk.pf2, '03_Outputs/SZK_Sample_Profile2.xlsx')


##---- Cluster result ----
xnt.group <- read.xlsx('03_Outputs/XNT_Cluster2.xlsx') %>% 
  select(cluster, pha) %>% 
  left_join(xnt_his_data_m, by = 'pha') %>% 
  group_by(Cluster = cluster, `医院Code` = pha, `诊断`, `科室` = `清洗后科室`, 
           `通用名`, `产品`, `规格`, `包装`, `Pack ID` = Pack.id) %>% 
  summarise(`金额` = sum(`金额`, na.rm = TRUE), 
            `数量` = sum(`数量`, na.rm = TRUE)) %>% 
  ungroup()

szk.group <- read.xlsx('03_Outputs/SZK_Cluster2.xlsx') %>% 
  select(cluster, pha) %>% 
  left_join(szk_his_data_m, by = 'pha') %>% 
  group_by(Cluster = cluster, `医院Code` = pha, `诊断`, `科室` = `最终科室`, 
           `通用名`, `产品` = `商品名`, `规格`, `包装`, `Pack ID` = pack.id) %>% 
  summarise(`金额` = sum(`金额`, na.rm = TRUE), 
            `数量` = sum(`数量`, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(xnt.group, '03_Outputs/XNT_Cluster_Summary.xlsx')
write.xlsx(szk.group, '03_Outputs/SZK_Cluster_Summary.xlsx')








