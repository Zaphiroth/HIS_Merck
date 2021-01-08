# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Merck
# Purpose:      Classification model
# programmer:   Zhe Liu
# Date:         2021-01-06
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Model sample -----
## cluster result
xnt.cluster2 <- read.xlsx('02_Inputs/XNT_Cluster2.xlsx', sheet = 'Cluster')
szk.cluster2 <- read.xlsx('02_Inputs/SZK_Cluster2.xlsx', sheet = 'Cluster')

## profile
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
  select(pha = `新版ID`, level = Hosp_level, property = `性质`, region = Region, 
         location, Province, City, District, type = Type, `百强县`, `county.HP`, 
         tier = `City.Tier.2010`, spec1 = `Specialty_1_标准化`, 
         spec2 = `Specialty_2_标准化`, est = Est_DrugIncome_RMB, `医生数`, 
         beds = `床位数`, `全科床位数`, `内科床位数`, `外科床位数`, `眼科床位数`, 
         `年诊疗人次`, `门诊诊次`, `内科诊次`, `外科诊次`, `入院人数`, 
         `住院病人手术人次数`, `医疗收入`, `门诊收入`, `门诊治疗收入`, 
         `门诊手术收入`, `住院收入`, `住院床位收入`, `住院治疗收入`, 
         `住院手术收入`, `药品收入`, `门诊药品收入`, `门诊西药收入`, 
         `住院药品收入`, `住院西药收入`) %>% 
  group_by(pha) %>% 
  summarise_all(function(x) {
    first(na.omit(x))
  }) %>% 
  ungroup()

## MAX sales
max.sales <- fread('02_Inputs/same_sales_result.csv') %>% 
  mutate(`分组` = if_else(`分组` == '综合医院', '综合', `分组`)) %>% 
  group_by(group = `分组`, pha = PHA.code) %>% 
  summarise(sales = sum(`金额`, na.rm = TRUE)) %>% 
  ungroup()

## sample data
rf.sample.xnt <- xnt.cluster2 %>% 
  select(cluster, pha) %>% 
  left_join(max.sales, by = 'pha') %>% 
  left_join(hosp.pf, by = 'pha') %>% 
  filter(!is.na(group), !is.na(est)) %>% 
  mutate(cluster = factor(cluster), 
         group = factor(group), 
         region = factor(region), 
         tier = factor(tier)) %>% 
  select(cluster, pha, group, sales, region, tier, est, beds)

rf.sample.szk <- szk.cluster2 %>% 
  select(cluster, pha) %>% 
  left_join(max.sales, by = 'pha') %>% 
  left_join(hosp.pf, by = 'pha') %>% 
  filter(!is.na(group), !is.na(est)) %>% 
  mutate(cluster = factor(cluster), 
         group = factor(group), 
         region = factor(region), 
         tier = factor(tier)) %>% 
  select(cluster, pha, group, sales, region, tier, est, beds)


##---- Random forest XNT ----
## divide data set
set.seed(111)
train.xnt <- createDataPartition(1:nrow(rf.sample.xnt), p = 0.8, list = FALSE)

rf.train.xnt <- rf.sample.xnt[c(train.xnt), ]
rf.test.xnt <- rf.sample.xnt[-c(train.xnt), ]

## train control
train.control <- trainControl(method = 'cv', 
                              number = 10, 
                              search = 'grid')

## best mtry
set.seed(222)
tune.grid.init <- expand.grid(.mtry = 1:(ncol(rf.train.xnt)-2))
rf.mtry.xnt <- train(cluster ~ ., 
                     data = rf.train.xnt[, -2], 
                     method = 'rf', 
                     metric = 'Accuracy', 
                     tuneGrid = tune.grid.init, 
                     trControl = train.control, 
                     importance = TRUE)

print(rf.mtry.xnt)
best.mtry.xnt <- rf.mtry.xnt$bestTune$mtry
tune.grid.xnt <- expand.grid(.mtry = best.mtry.xnt)

## best maxnodes
store.maxnodes.xnt <- list()
for (i in 2:25) {
  set.seed(333)
  rf.maxnodes <- train(cluster ~ ., 
                       data = rf.train.xnt[, -2], 
                       method = 'rf', 
                       metric = 'Accuracy', 
                       tuneGrid = tune.grid.xnt, 
                       trControl = train.control, 
                       importance = TRUE, 
                       maxnodes = i)
  key <- toString(i)
  store.maxnodes.xnt[[key]] <- rf.maxnodes
}

summary(resamples(store.maxnodes.xnt))
best.maxnodes.xnt <- 11

## best ntree
store.ntree.xnt <- list()
for (i in seq(100, 5000, 100)) {
  set.seed(444)
  rf.ntree <- train(cluster ~ ., 
                    data = rf.train.xnt[, -2], 
                    method = 'rf', 
                    metric = 'Accuracy', 
                    tuneGrid = tune.grid.xnt, 
                    trControl = train.control, 
                    importance = TRUE, 
                    maxnodes = best.maxnodes.xnt, 
                    ntree = i)
  key <- toString(i)
  store.ntree.xnt[[key]] <- rf.ntree
}

summary(resamples(store.ntree.xnt))
best.ntree.xnt <- 2100

## fit model
set.seed(555)
rf.fit.xnt <- train(cluster ~ ., 
                    data = rf.train.xnt[, -2], 
                    method = 'rf', 
                    metric = 'Accuracy', 
                    tuneGrid = tune.grid.xnt, 
                    trControl = train.control, 
                    importance = TRUE, 
                    ntree = best.ntree.xnt, 
                    maxnodes = best.maxnodes.xnt)

## evaluate
rf.pred.xnt <- predict(rf.fit.xnt, rf.test.xnt[, -c(1, 2)])
confusionMatrix(rf.pred.xnt, rf.test.xnt$cluster) # accuracy = 0.35

## final model
set.seed(666)
rf.model.xnt <- train(cluster ~ ., 
                      data = rf.sample.xnt[, -2], 
                      method = 'rf', 
                      metric = 'Accuracy', 
                      tuneGrid = tune.grid.xnt, 
                      trControl = train.control, 
                      importance = TRUE, 
                      ntree = best.ntree.xnt, 
                      maxnodes = best.maxnodes.xnt)

varImp(rf.model.xnt)


##---- Random forest SZK ----
## divide data set
set.seed(1111)
train.szk <- createDataPartition(1:nrow(rf.sample.szk), p = 0.8, list = FALSE)

rf.train.szk <- rf.sample.szk[c(train.szk), ]
rf.test.szk <- rf.sample.szk[-c(train.szk), ]

## train control
train.control <- trainControl(method = 'cv', 
                              number = 10, 
                              search = 'grid')

## best mtry
set.seed(2222)
tune.grid.init <- expand.grid(.mtry = 1:(ncol(rf.train.szk)-2))
rf.mtry.szk <- train(cluster ~ ., 
                     data = rf.train.szk[, -2], 
                     method = 'rf', 
                     metric = 'Accuracy', 
                     tuneGrid = tune.grid.init, 
                     trControl = train.control, 
                     importance = TRUE)

print(rf.mtry.szk)
best.mtry.szk <- rf.mtry.szk$bestTune$mtry
tune.grid.szk <- expand.grid(.mtry = best.mtry.szk)

## best maxnodes
store.maxnodes.szk <- list()
for (i in 2:25) {
  set.seed(3333)
  rf.maxnodes <- train(cluster ~ ., 
                       data = rf.train.szk[, -2], 
                       method = 'rf', 
                       metric = 'Accuracy', 
                       tuneGrid = tune.grid.szk, 
                       trControl = train.control, 
                       importance = TRUE, 
                       maxnodes = i)
  key <- toString(i)
  store.maxnodes.szk[[key]] <- rf.maxnodes
}

summary(resamples(store.maxnodes.szk))
best.maxnodes.szk <- 16

## best ntree
store.ntree.szk <- list()
for (i in seq(100, 5000, 100)) {
  set.seed(4444)
  rf.ntree <- train(cluster ~ ., 
                    data = rf.train.szk[, -2], 
                    method = 'rf', 
                    metric = 'Accuracy', 
                    tuneGrid = tune.grid.szk, 
                    trControl = train.control, 
                    importance = TRUE, 
                    maxnodes = best.maxnodes.szk, 
                    ntree = i)
  key <- toString(i)
  store.ntree.szk[[key]] <- rf.ntree
}

summary(resamples(store.ntree.szk))
best.ntree.szk <- 900

## fit model
set.seed(5555)
rf.fit.szk <- train(cluster ~ ., 
                    data = rf.train.szk[, -2], 
                    method = 'rf', 
                    metric = 'Accuracy', 
                    tuneGrid = tune.grid.szk, 
                    trControl = train.control, 
                    importance = TRUE, 
                    ntree = best.ntree.szk, 
                    maxnodes = best.maxnodes.szk)

## evaluate
rf.pred.szk <- predict(rf.fit.szk, rf.test.szk[, -c(1, 2)])
confusionMatrix(rf.pred.szk, rf.test.szk$cluster) # accuracy = 0.2

## final model
set.seed(6666)
rf.model.szk <- train(cluster ~ ., 
                      data = rf.sample.szk[, -2], 
                      method = 'rf', 
                      metric = 'Accuracy', 
                      tuneGrid = tune.grid.szk, 
                      trControl = train.control, 
                      importance = TRUE, 
                      ntree = best.ntree.szk, 
                      maxnodes = best.maxnodes.szk)

varImp(rf.model.szk)


##---- Prediction ----
## unsample data
rf.unsample.xnt <- max.sales %>% 
  filter(!(pha %in% rf.sample.xnt$pha)) %>% 
  left_join(hosp.pf, by = 'pha') %>% 
  filter(!is.na(est)) %>% 
  mutate(group = factor(group), 
         region = factor(region), 
         tier = factor(tier)) %>% 
  select(pha, group, sales, region, tier, est, beds)

rf.unsample.szk <- max.sales %>% 
  filter(!(pha %in% rf.sample.szk$pha)) %>% 
  left_join(hosp.pf, by = 'pha') %>% 
  filter(!is.na(est)) %>% 
  mutate(group = factor(group), 
         region = factor(region), 
         tier = factor(tier)) %>% 
  select(pha, group, sales, region, tier, est, beds)

## prediction
rf.pred.xnt <- predict(rf.model.xnt, rf.unsample.xnt[, -1])
rf.pred.szk <- predict(rf.model.szk, rf.unsample.szk[, -1])

## result
universe.cluster.xnt <- rf.unsample.xnt %>% 
  mutate(cluster = rf.pred.xnt) %>% 
  bind_rows(rf.sample.xnt)

universe.cluster.szk <- rf.unsample.szk %>% 
  mutate(cluster = rf.pred.szk) %>% 
  bind_rows(rf.sample.szk)

wb <- createWorkbook()
addWorksheet(wb, 'XNT')
addWorksheet(wb, 'SZK')
writeDataTable(wb, 'XNT', universe.cluster.xnt)
writeDataTable(wb, 'SZK', universe.cluster.szk)
saveWorkbook(wb, '03_Outputs/Universe_Cluster_Result.xlsx', overwrite = TRUE)
