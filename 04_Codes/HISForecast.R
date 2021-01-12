# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  HIS Merck
# Purpose:      Forecast function
# programmer:   Zhe Liu
# Date:         2021-01-11
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


his_forecasting_model_1 <- function(data, training_window, period, metric) {
  # parameters:
  # @ data: the data forecasting based on;
  # @ training_window: how long the model will take to train
  # @ period: the forecasting period;
  # @ metric: which var will be forecasted
  
  # data = max.xnt
  # training_window = c('2018-01-01', '2020-07-01')
  # period = 5
  # metric = "金额（RMB）"
  
  
  # quarter.month <- data.frame(quarter = c(rep('Q1', 3), rep('Q2', 3), 
  #                                         rep('Q3', 3), rep('Q4', 3)), 
  #                             month = stri_pad_left(1:12, 2, 0))
  
  data_m <- data %>% 
    # left_join(quarter.month, by = c('季度' = 'quarter')) %>% 
    # mutate(`金额` = `金额` / 3, 
    #        `数量(盒)` = `数量(盒)` / 3, 
    #        Date = as.Date(stri_paste(`年`, month, '01', sep = '-'))) %>% 
    mutate(Date = yq(quarter)) %>% 
    filter(Date >= as.Date(training_window[1]),
           Date <= as.Date(training_window[2])) %>%
    unite('group', cluster, `分组`, `pack id`, sep = '+') %>% 
    select(Date, as.symbol(metric), group) %>%
    rename(ds = Date, y = as.symbol(metric))
  
  # data <- data[, c("Date", metric)]
  # data$Date <- as.Date(paste(substr(data$Date, 1, 4), 
  #                            substr(data$Date, 5, 6),
  #                            "01",
  #                            sep = "-"))
  # 
  # data_m <- data %>%
  #   filter(Date >= as.Date(training_window[1]),
  #          Date <= as.Date(training_window[2])) %>%
  #   select(Date, as.symbol(metric)) %>%
  #   rename(ds = Date, y = as.symbol(metric))
  # %>%
  # ## add the cap
  # mutate(cap = max(y) + 0.5 * max(y))
  
  forecast.total <- NULL
  for (i in unique(data_m$group)) {
    data_g <- data_m %>% 
      filter(group == i) %>% 
      arrange(ds) %>% 
      select(ds, y)
    
    m <- prophet()
    m <- add_country_holidays(m, country_name = "CN")
    m <- fit.prophet(m, data_g)
    future <- make_future_dataframe(m, periods = period, freq = 'quarter')    # %>%
    forecast <- predict(m, future) %>% 
      mutate(group = i)
    forecast.total <- bind_rows(forecast.total, forecast)
  }
  # mutate(cap = unique(data_m$cap))
  
  # plot(m, forecast)
  
  return(forecast.total)
}
