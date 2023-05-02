#########################################
library(pacman)
pacman::p_load(plotly, # Making dynamic graphs
               modeltime, # Times model classics 
               tidyverse, # All tidy for manipulation
               timetk, # Making a tables and graph's of timeseries
               lubridate, # Working if date
               easystats, # making a spells on the datas.
               imputeTS)
##########################################


nested_data_tbl <- data %>%
  extend_timeseries(
    .id_var        = id,
    .date_var      = date,
    .length_future = 180
  ) %>%
  
  nest_timeseries(
    .id_var        = id,
    .length_future = 180,
    .length_actual = 180*2
  ) %>%
  split_nested_timeseries(
    .length_test = 180
  )


##################################################

rec_complete <- recipe(value ~ . , 
                       extract_nested_train_split(nested_data_tbl))  %>% 
  timetk::step_timeseries_signature(date) %>% 
  step_rm() %>%
  step_zv(all_predictors())

rec_nnar <- recipe(value ~ date , extract_nested_train_split(nested_data_tbl)) 
###########################

wflw_prophet_01 <- workflow() %>%
  add_model(prophet_boost(seasonality_daily = F,
                          seasonality_weekly = F,
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

wflw_prophet_03 <- workflow() %>%
  add_model(prophet_boost(seasonality_daily = F,
                          seasonality_weekly = T,
                          seasonality_yearly = T,
                          growth = 'linear',
                          learn_rate = 0.1,
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
    set_engine("arima_xgboost")  
  ) %>%
  add_recipe(rec_complete)

wflw_arima_02 <- workflow() %>%
  add_model(arima_boost(learn_rate = 0.1,
                        mtry = 5,
                        trees = 12,
                        tree_depth = 1000
  ) %>%
    set_engine("arima_xgboost")  
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



###################### Prediction models

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
  plot_modeltime_forecast(  )
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


