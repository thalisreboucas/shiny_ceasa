##########################################

nested_data_tbl <- data %>%
  extend_timeseries(
    .id_var        = id,
    .date_var      = date,
    .length_future = 60
  ) %>%
  
  nest_timeseries(
    .id_var        = id,
    .length_future = 60
  ) %>%
  split_nested_timeseries(
    .length_test = 60
  )




##################################################

rec_complete <- recipe(value ~ . , 
                       extract_nested_train_split(nested_data_tbl))  %>% 
  timetk::step_timeseries_signature(date) %>% 
  step_rm() %>%
  step_zv(all_predictors())

rec_nnar <- recipe(value ~ date  , extract_nested_train_split(nested_data_tbl)) %>% 
  timetk::step_timeseries_signature(date)
###########################

wflw_prophet <- workflow() %>%
  add_model(prophet_boost(seasonality_daily = F,
                          seasonality_weekly = T,
                          seasonality_yearly = T,
                          growth = 'linear',
                          learn_rate = 0.01,
                          mtry = 5,
                          trees = 14,
                          tree_depth =  1000
  ) %>%
    set_engine("prophet_xgboost")
  ) %>%
  add_recipe(rec_complete)



wflw_arima <- workflow() %>%
  add_model(arima_boost(learn_rate = 0.01,
                        mtry = 5,
                        trees = 14,
                        tree_depth = 1000
  ) %>%
    set_engine("auto_arima_xgboost")  
  ) %>%
  add_recipe(rec_complete)


wflw_nnetar <- workflow() %>%
  add_model(nnetar_reg(epochs = 15,
                       num_networks = 500)
  %>%
  set_engine("nnetar") )%>% 
  add_recipe(rec_nnar) 


#################################### training models

remove( rec_complete,
        rec_nnar,
        wflw_prophet,
        wflw_arima,
        wflw_nnetar)



######################## END OF CODES

#### see the traing ## // Para o shiny //

See_the_traning <- function(product_id){
nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
  group_by(id) %>%
  filter(id == product_id) %>% 
  plot_modeltime_forecast(  )
}


# see the forecast
See_the_Forecast <- function(product_id){
nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  group_by(id) %>%
  filter(id == product_id ) %>% 
  plot_modeltime_forecast( .conf_interval_show = T,
                           .plotly_slider = T )
}

See_the_Forecast(1)
# see the resuduals

See_the_Results <- function(product_id){
  nested_modeltime_tbl %>% 
    extract_nested_test_accuracy() %>%
    group_by(id) %>%
    filter(id == product_id ) 
  
  
  
}


See_the_traning(1)
See_the_Forecast(1) 
See_the_Results(1)




