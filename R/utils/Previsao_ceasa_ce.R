train <- function(data){

nested_data_tbl <- data |> 
  extend_timeseries(
    .id_var        = id,
    .date_var      = date,
    .length_future = 60
  )|>
  
  nest_timeseries(
    .id_var        = id,
    .length_future = 60
  )|>
  split_nested_timeseries(
    .length_test = 60
  )


rec_complete <- recipe(value ~ . , 
                       extract_nested_train_split(nested_data_tbl)) |> 
  timetk::step_timeseries_signature(date)|> 
  step_rm()|>
  step_zv(all_predictors())

rec_nnar <- recipe(value ~ date  , extract_nested_train_split(nested_data_tbl))|> 
  timetk::step_timeseries_signature(date)
###########################

wflw_prophet <- workflow()|>
  add_model(prophet_boost(seasonality_daily = F,
                          seasonality_weekly = T,
                          seasonality_yearly = T,
                          growth = 'linear',
                          learn_rate = 0.01,
                          mtry = 5,
                          trees = 14,
                          tree_depth =  1000
  )|>
    set_engine("prophet_xgboost")
  )|>
  add_recipe(rec_complete)



wflw_arima <- workflow()|>
  add_model(arima_boost(learn_rate = 0.01,
                        mtry = 5,
                        trees = 14,
                        tree_depth = 1000
  )|>
    set_engine("auto_arima_xgboost")  
  )|>
  add_recipe(rec_complete)


wflw_nnetar <- workflow()|>
  add_model(nnetar_reg(epochs = 15,
                       num_networks = 500)
 |>
  set_engine("nnetar") )%>% 
  add_recipe(rec_nnar) 

nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested data 
  nested_data = nested_data_tbl,
  
  # Add workflows
  wflw_prophet,
  wflw_arima,
  wflw_nnetar
) |> extract_nested_test_forecast()

data_traing <- nested_modeltime_tbl|> 
  group_by(id)|> 
  dplyr::mutate ( Name_models =
                    dplyr::case_when(
                      .model_id == 1 ~ "Prophet XG",
                      .model_id == 2 ~ "Arima XG",
                      .model_id == 3 ~ "NNAR"
                    ))

return(data_traing) }



res <- function(data_traing){
  
actual <- data_traing|> 
  dplyr::filter(.key == "actual",.index >= min(data_traing |> 
                                          dplyr::filter(.key == "prediction") |> 
                                          pull(.index))) |> 
  dplyr::select(-.model_id,-.model_desc,-.conf_lo,-.conf_hi,-Name_models) 

prediction <- data_traing|> 
  dplyr::filter(.key != "actual", .index >= min(data_traing |> 
                                           filter(.key == "prediction") |> 
                                           pull(.index))) |> 
  dplyr::mutate (pred_value = .value)|> 
  dplyr::select(-.conf_lo,-.conf_hi) |> 
  tidyr::pivot_wider(names_from = .key, values_from = .value)

residual <- dplyr::inner_join(actual,prediction,by = c("id",".index"))  |> 
  dplyr::group_by(id,.index,Name_models) |> 
  dplyr::mutate( residual = pred_value-.value,.groups = 'drop') |> 
  dplyr::select(-prediction,-.groups,-.model_desc)

return(residual)
}


