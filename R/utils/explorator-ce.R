# AED CEASA -----
library(pacman)
pacman::p_load(readxl,# Open Data in Xl (Excel)
               dplyr,
               zoo,
               DT,
               plotly, # Making dynamic graphs
               modeltime, # Times model classics 
               tidyverse, # All tidy for manipulation
               timetk, # Making a tables and graph's of timeseries
               lubridate, # Working if date
               easystats, # making a spells on the datas.
               imputeTS)

############
EDA <- function(product_id){ 
  dt <- data %>%
    dplyr::group_by(id) %>%
    dplyr::filter(id == product_id)
  
  graph <-  dt %>%
    timetk::plot_time_series( .date_var    = date,
                             .value       = value)
  
  table <- dt %>%  datawizard::describe_distribution()

  #### ESSE  
  tb <- dt %>%  dplyr::mutate(my = zoo::as.yearmon(date)) %>% 
    tidyr::pivot_wider(id_cols = "Produto", names_from = my, 
                       values_from = value ,values_fn = list) %>% describe_distribution()
  
  anomaly <- dt %>% plot_anomaly_diagnostics(date,
                                             value)
  

  acf <- dt %>% plot_acf_diagnostics(
    date, value,               # ACF & PACF
    .lags = "30 days",          # 7-Days of hourly lags
    .interactive = FALSE
  )
  
  seasonal <- dt %>% plot_seasonal_diagnostics(date, value, .interactive = T)
  
  
  stl <- dt %>% plot_stl_diagnostics(  date, value,
                                       .feature_set = c("observed","season","trend","remainder"),
                                       .interactive = T)
  
  return(list(
    table <- table,
    graph <- graph ,
    anomaly <- anomaly ,
    acf <- acf,
    seasonal <- seasonal,
    stl <- stl,
    tb_2 <- tb
  ))  
  
}

tbl <- data %>% dplyr::mutate(my = zoo::as.yearmon(date)) %>% 
  dplyr::group_by(my,Produto) %>% 
  dplyr::summarise(value = round(mean(value),2)) %>% drop_na() %>% 
  tidyr::pivot_wider(id_cols = "Produto", names_from = my, 
                     values_from = value)    %>%  datatable(filter = 'top', options = list(
                       pageLength = 10, autoWidth = TRUE))



