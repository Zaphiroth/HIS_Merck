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
  group_by(pha, dept = `清洗后科室`, diag = `诊断`) %>% 
  summarise(value = sum(`金额`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(pha) %>% 
  mutate(value_sum = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(value_prop = value / value_sum) %>% 
  left_join(xnt.var, by = c('dept', 'diag')) %>% 
  # mutate(value_prop = round(value_prop, 4)) %>% 
  add_count(var_name) %>% 
<<<<<<< HEAD
  filter(n>12) %>% 
=======
  filter(n >= 4) %>% 
>>>>>>> main
  pivot_wider(id_cols = c(pha), 
              names_from = var_name, 
              values_from = value_prop, 
              values_fill = 0)

szk.sample <- szk_his_data_m %>% 
  group_by(pha, dept = `最终科室`, diag = `诊断`) %>%
  summarise(value = sum(`金额`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(pha) %>% 
  mutate(value_sum = sum(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(value_prop = value / value_sum) %>% 
  left_join(szk.var, by = c('dept', 'diag')) %>% 
  # mutate(value_prop = round(value_prop, 4)) %>% 
  add_count(var_name) %>% 
  filter(n >= 4) %>% 
  pivot_wider(id_cols = c(pha), 
              names_from = var_name, 
              values_from = value_prop, 
              values_fill = 0)


<<<<<<< HEAD
##---- XNT Clustering ----
## number of clusters
x <- xnt.sample[,-1]
xnt.nb.silhouette <- fviz_nbclust(x,
                                  kmeans,
                                  method = 'silhouette',
                                  k.max = 10,
                                  nboot = 1000,
                                  verbose = TRUE,
                                  print.summary = TRUE)

xnt.nb.wss <- fviz_nbclust(x,
                           kmeans,
                           method = 'wss',
                           k.max = 10,
                           nboot = 1000,
                           verbose = TRUE,
                           print.summary = TRUE)

xnt.nb.gap <- fviz_nbclust(x,
                           kmeans,
                           method = 'gap_stat',
                           k.max = 10,
                           nboot = 1000,
                           verbose = TRUE,
                           print.summary = TRUE)

k=5
## mclust
xnt_mclust <- Mclust(x, k)
xnt1 <- data.frame(xnt.sample[,1],xnt_mclust$classification)
fviz_cluster(xnt_mclust, x)

## kmeans
xnt_kmeans <- kmeans(x, k, iter.max = 1000L, nstart = 50L, algorithm = "Hartigan-Wong", trace = FALSE)
fviz_cluster(xnt_kmeans, x)

## 选择mclust
xnt.cluster <- data.frame(cluster = xnt_mclust$classification) %>% 
  bind_cols(xnt.sample) %>% 
  left_join(hosp.code, by = 'pha') %>% 
  select(cluster, pha, rp, obs, onc, starts_with('Var'))

table(xnt.cluster$rp, xnt.cluster$cluster)

## PCA
xnt.pca <- princomp(xnt.sample[, -1]) # 前7个主成分
summary(xnt.pca)

Scores <- bind_cols(xnt1,as.data.frame(xnt.pca$scores)) %>% 
  rename(cluster = xnt_mclust.classification)

wb <- createWorkbook()
addWorksheet(wb, 'Cluster')
addWorksheet(wb, 'Loadings')
addWorksheet(wb, 'Scores')
addWorksheet(wb, 'Var')
writeDataTable(wb, 'Cluster', xnt.cluster)
writeDataTable(wb, 'Loadings', 
               as.data.frame(xnt.pca$loadings[1:41, ], 
                             row.names = rownames(xnt.pca$loadings[1:41, ])), 
               rowNames = TRUE)
writeDataTable(wb, 'Scores', Scores)
writeDataTable(wb, 'Var',xnt.var)
saveWorkbook(wb, '03_Outputs/XNT_Cluster.xlsx', overwrite = TRUE)



=======
>>>>>>> main
##---- SZK Clustering ----
## number of clusters
szk.nb.silhouette <- fviz_nbclust(szk.sample[, -1], 
                                  kmeans, 
                                  method = 'silhouette', 
                                  k.max = 20, 
                                  verbose = TRUE, 
                                  print.summary = TRUE)

szk.nb.wss <- fviz_nbclust(szk.sample[, -1], 
                           kmeans, 
                           method = 'wss', 
                           k.max = 20, 
                           verbose = TRUE, 
                           print.summary = TRUE)

szk.nb.gap <- fviz_nbclust(szk.sample[, -1], 
                           kmeans, 
                           method = 'gap_stat', 
                           k.max = 20, 
                           nboot = 1000, 
                           verbose = TRUE, 
                           print.summary = TRUE)

## k-means
szk.kmeans <- kmeans(szk.sample[, -1], 
                     centers = 5, 
                     iter.max = 10, 
                     nstart = 30, 
                     algorithm = 'Hartigan-Wong', 
                     trace = FALSE)

szk.kmeans.plot <- fviz_cluster(szk.kmeans, szk.sample[, -1], 
                                ellipse = FALSE, 
                                stand = FALSE, repel = FALSE) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_vline(aes(xintercept = 0), linetype = "dashed")
print(szk.kmeans.plot)

szk.cluster <- data.frame(cluster = szk.kmeans$cluster) %>% 
  bind_cols(szk.sample) %>% 
  left_join(hosp.code, by = 'pha') %>% 
  select(cluster, pha, rp, obs, onc, starts_with('Var'))

<<<<<<< HEAD
## PCA
szk.pca <- princomp(szk.sample[, -1]) # 前8个主成分

## result
=======
table(szk.cluster$rp, szk.cluster$cluster)

write.xlsx(szk.cluster, '03_Outputs/SZK_Cluster.xlsx')

szk.pca <- princomp(szk.sample[, -1]) # 前8个主成分

>>>>>>> main
wb <- createWorkbook()
addWorksheet(wb, 'Cluster')
addWorksheet(wb, 'Loadings')
addWorksheet(wb, 'Scores')
<<<<<<< HEAD
addWorksheet(wb, 'Var')
=======
>>>>>>> main
writeDataTable(wb, 'Cluster', szk.cluster)
writeDataTable(wb, 'Loadings', 
               as.data.frame(szk.pca$loadings[1:54, ], 
                             row.names = rownames(szk.pca$loadings[1:54, ])), 
               rowNames = TRUE)
writeDataTable(wb, 'Scores', 
<<<<<<< HEAD
               bind_cols(szk.cluster[,c("pha","cluster")],as.data.frame(szk.pca$scores)))
writeDataTable(wb, 'Var',szk.var)
=======
               as.data.frame(szk.pca$scores))
>>>>>>> main
saveWorkbook(wb, '03_Outputs/SZK_Cluster.xlsx', overwrite = TRUE)


## GMM
# szk.bic <- mclustBIC(szk.sample[, -1])
# plot(szk.bic)
# summary(szk.bic)

szk.icl <- mclustICL(szk.sample[, -1])
plot(szk.icl)
summary(szk.icl)
<<<<<<< HEAD

szk.gmm <- Mclust(szk.sample[, -1], 
                  x = szk.bic)

szk.cluster <- data.frame(cluster = szk.gmm$classification) %>% 
  bind_cols(szk.sample) %>% 
  left_join(hosp.code, by = 'pha') %>% 
  select(cluster, pha, rp, obs, onc, starts_with('Var'))
=======

szk.gmm <- Mclust(szk.sample[, -1], 
                  x = szk.bic)

szk.cluster <- data.frame(cluster = szk.gmm$classification) %>% 
  bind_cols(szk.sample) %>% 
  left_join(hosp.code, by = 'pha') %>% 
  select(cluster, pha, rp, obs, onc, starts_with('Var'))

table(szk.cluster$rp, szk.cluster$cluster)
>>>>>>> main

table(szk.cluster$rp, szk.cluster$cluster)