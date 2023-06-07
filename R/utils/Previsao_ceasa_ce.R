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

wflw_prophet_01 <- workflow() %>%
  add_model(prophet_boost(seasonality_daily = F,
                          seasonality_weekly = T,
                          seasonality_yearly = T,
                          growth = 'linear',
                          learn_rate = 0.3,
                          mtry = 5,
                          trees = 12,
                          tree_depth =  1000
  ) %>%
    set_engine("prophet_xgboost")
  ) %>%
  add_recipe(rec_complete)


wflw_prophet_02 <- workflow() %>%
  add_model(prophet_boost(seasonality_daily = F,
                          seasonality_weekly = T,
                          seasonality_yearly = F,
                          growth = 'linear',
                          learn_rate = 0.8,
                          mtry = 5,
                          trees = 12,
                          tree_depth =  1000
  ) %>%
    set_engine("prophet_xgboost")
  ) %>%
  add_recipe(rec_complete)

wflw_prophet_03 <- workflow() %>%
  add_model(prophet_boost(seasonality_daily = F,
                          seasonality_weekly = T,
                          seasonality_yearly = F,
                          growth = 'linear',
                          learn_rate = 0.25,
                          mtry = 5,
                          trees = 12,
                          tree_depth =  1000
  ) %>%
    set_engine("prophet_xgboost")
  ) %>%
  add_recipe(rec_complete)


wflw_arima_01 <- workflow() %>%
  add_model(arima_boost(learn_rate = 0.3,
                        mtry = 5,
                        trees = 12,
                        tree_depth = 1000
  ) %>%
    set_engine("auto_arima_xgboost")  
  ) %>%
  add_recipe(rec_complete)

wflw_arima_02 <- workflow() %>%
  add_model(arima_boost(learn_rate = 0.5,
                        mtry = 5,
                        trees = 12,
                        tree_depth = 1000
  ) %>%
    set_engine("auto_arima_xgboost")  
  ) %>%
  add_recipe(rec_complete)

wflw_nnetar <- workflow() %>%
  add_model(nnetar_reg(epochs = 15,
                       num_networks = 100)
  %>%
  set_engine("nnetar") )%>% 
  add_recipe(rec_nnar) 


#################################### training models
 
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested data 
  nested_data = nested_data_tbl,
  
  # Add workflows
  wflw_prophet_01,
  wflw_prophet_02,
  wflw_prophet_03,
  wflw_arima_01,
  wflw_arima_02,
  wflw_nnetar
)

remove( rec_complete,
        rec_nnar,
        wflw_prophet_01,
        wflw_prophet_02,
        wflw_prophet_03,
        wflw_arima_01,
        wflw_arima_02,
        wflw_nnetar)

###################### Prediction models #################

nested_modeltime_refit_tbl <-   modeltime_nested_refit(object = nested_modeltime_tbl,
    control = control_nested_refit(verbose = TRUE)
  )


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



residuals_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_residuals()

