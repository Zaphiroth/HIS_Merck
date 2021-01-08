# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Merck
# Purpose:      HIS data cleaning
# programmer:   Zhe Liu
# Date:         2020-12-29
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
## 雪诺同
xnt_cleaning_rule <-  read.xlsx("02_Inputs/雪诺同诊断清洗规则.xlsx", 
                                startRow = 2, rows = 2:9)

xnt_cleaning_rule_m <- xnt_cleaning_rule %>%
  # dplyr::rowwise(疾病诊断优先顺序) %>%
  # summarise(关键词 = paste(c_cross(关键词1:关键词9), collapse = "|"))
  pivot_longer(关键词1:关键词9, names_to = "tmp", values_to = "关键词") %>%
  filter(!is.na(关键词)) %>%
  group_by(疾病诊断优先顺序) %>%
  summarise(关键词 = paste(关键词, collapse = "|")) %>%
  ungroup()

## 思则凯
szk_cleaning_rule <- read.xlsx("02_Inputs/思则凯诊断清洗规则.xlsx", rows = 1:10)

szk_cleaning_rule_m <- szk_cleaning_rule %>%
  # dplyr::rowwise(疾病诊断优先顺序) %>%
  # summarise(关键词 = paste(c_cross(关键词1:关键词9), collapse = "|"))
  pivot_longer(关键词1:关键词5, names_to = "tmp", values_to = "关键词") %>%
  filter(!is.na(关键词)) %>%
  group_by(疾病诊断优先顺序) %>%
  summarise(关键词 = paste(关键词, collapse = "|")) %>%
  ungroup()

## his data
xnt_his_data <- read.xlsx("02_Inputs/雪诺同HIS原始数据.xlsx")
szk_his_data <- read.xlsx("02_Inputs/思则凯HIS原始数据.xlsx")

## hospital code
hosp.code <- read.xlsx('02_Inputs/FB2020016_医院编码_3.xlsx') %>% 
  filter(!is.na(PHA.code), PHA.code != '0') %>% 
  select(`医院编码`, pha = PHA.code, rp = `是否在生殖中心医院list`, 
         obs = `妇产科医生数`, onc = `肿瘤科医生数`)


##---- Diagnosis cleaning ----
## 雪诺同
xnt_his_data_m <- xnt_his_data %>% 
  inner_join(hosp.code, by = '医院编码') %>% 
  mutate(`原始诊断` = toupper(`原始诊断`), 
         `原始诊断` = gsub('[[:punct:]]', ' ', `原始诊断`), 
         `原始诊断` = trimws(`原始诊断`)) %>% 
  filter(!is.na(`清洗后科室`), !is.na(`原始诊断`), nchar(`原始诊断`) > 0) %>% 
  mutate(
    `诊断` = case_when(
      grepl('移植|IVF|FET|试管|取卵|人工授精|OPU|ET|IUI', `原始诊断`) ~ '试管婴儿补充黄体酮',
      grepl('不孕|不育', `原始诊断`) ~ '不孕不育',
      grepl('早产',  `原始诊断`) ~ '早产',
      grepl('早孕|APS|流产|保胎|妊娠',  `原始诊断`) ~ '早孕保胎',
      grepl('月经|闭经|停经|痛经|绝经|卵巢早衰|经期',  `原始诊断`) ~ '月经不调',
      grepl('子宫|出血|息肉|增生|增厚|内膜炎|肌瘤|腺肌',  `原始诊断`) ~ '子宫病变',
      TRUE ~ '其他'
    )
  )

write.xlsx(xnt_his_data_m, '03_Outputs/XNT_HIS_Sample.xlsx')

## 思则凯
szk_his_data_m <- szk_his_data %>%
  inner_join(hosp.code, by = '医院编码') %>% 
  mutate(`原始诊断` = toupper(`原始诊断`), 
         `原始诊断` = gsub('[[:punct:]]', ' ', `原始诊断`), 
         `原始诊断` = trimws(`原始诊断`)) %>% 
  filter(!is.na(`最终科室`), !is.na(`原始诊断`), nchar(`原始诊断`) > 0) %>% 
  mutate(
    `诊断` = case_when(
      grepl('前列腺癌|PCA|前列腺CA|前列腺肿瘤|前列腺增生', `原始诊断`) ~ '前列腺癌或增生',
      grepl('乳腺癌|乳癌|乳腺CA|乳腺肿瘤',  `原始诊断`) ~ '乳腺癌',
      grepl('FET|内膜准备',  `原始诊断`) ~ '内膜准备',
      grepl('不孕|不育|IVF', `原始诊断`) ~ '不孕症，IVF准备或周期中',
      grepl('子宫内膜|异位|巧克力|巧囊|DIE',  `原始诊断`) ~ '子宫内膜异位症',
      grepl('子宫腺肌|子宫肌腺',  `原始诊断`) ~ '子宫肌腺',
      grepl('子宫肌瘤',  `原始诊断`) ~ '子宫肌瘤',
      grepl('性早熟',  `原始诊断`) ~ '性早熟',
      TRUE ~ '其他'
    )
  )

write.xlsx(szk_his_data_m, '03_Outputs/SZK_HIS_Sample.xlsx')
