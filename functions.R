aggregate_time_series <-
function(data, time_unit = "month") {
    output_tbl <- data %>% 
    mutate(date = floor_date(ORDERDATE, unit = time_unit)) %>% 
        group_by(date) %>% 
        summarise(total_sales = sum(SALES)) %>% 
        ungroup() %>% 
        mutate(label_text = str_glue("Datum: {date}
                                 Prihodi: {scales::dollar(total_sales)}"))
    return(output_tbl)
}
plot_time_series <-
function(data){
    g <- data %>% 
        ggplot(aes(date, total_sales)) +
        geom_line(color = "#6D0D19") +
        geom_point(aes(text = label_text), color = "#6D0D19", size = 0.1) +
        geom_smooth(method = "loess", span = 0.2, color = "#38383A") +
        
        theme_tq() +
        expand_limits(y = 0) +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(x = "", y = "")
    
    ggplotly(g, tooltip = "text")
}
generate_forecast <-
function(data, n_future = 12, seed = NULL){
    train_tbl <- data %>% 
        tk_augment_timeseries_signature()
    
    
    future_data_tbl <- data %>% 
        mutate(date = as_date(date)) %>% 
        tk_index() %>% 
        tk_make_future_timeseries(n_future = n_future, inspect_weekdays = TRUE, 
                                  inspect_months = TRUE) %>% 
        tk_get_timeseries_signature()
    
    time_scale <- data %>% 
        tk_index() %>% 
        tk_get_timeseries_summary() %>% 
        pull(scale)
    
    # If year use Linear regression, else use XGBoost
    if(time_scale == "year") {
        model <- linear_reg(mode = "regression") %>% 
            set_engine(engine = "lm") %>% 
            fit.model_spec(total_sales ~ ., 
                           data = train_tbl %>% select(total_sales, index.num))
    } else {
        seed <- 123
        set.seed(seed)
        model <- boost_tree(
            mode = "regression",
            mtry = 20,
            trees = 500,
            min_n = 3,
            tree_depth = 8,
            learn_rate = 0.01,
            loss_reduction = 0.01) %>% 
            set_engine(engine = "xgboost") %>% 
            fit.model_spec(total_sales ~ ., 
                           data = train_tbl %>% select(-date, -label_text, -diff)
            )
        
    }
    
    prediction_tbl <- predict(model, new_data = future_data_tbl) %>% 
        bind_cols(future_data_tbl) %>% 
        select(.pred, index) %>% 
        rename(total_sales = .pred,
               date = index) %>% 
        mutate(label_text = str_glue("Datum: {date}
                                 Prihodi: {scales::dollar(total_sales)}")) %>% 
        add_column(key = "Predikcija")
    
    output_tbl <- data %>% 
        add_column(key = "Stvarni") %>% 
        bind_rows(prediction_tbl)
    
    return(output_tbl)
    
}
plot_forecast <-
function(data) {
    
    # Year LM
    time_scale <- data %>% 
        tk_index() %>% 
        tk_get_timeseries_summary() %>% 
        pull(scale)
    
    # Only one prediction - points
    n_predictions <- data %>% 
        filter(key == "Prediction") %>% 
        nrow()
    
    g <- data %>% 
        ggplot(aes(date, total_sales, color = key)) +
        geom_line() +
        #geom_point(aes(text = label_text), size = 0.01) +
        #geom_smooth(method = "loess", span = 0.2) +
        
        theme_tq() +
        scale_color_tq() +
        scale_y_continuous(labels = scales::dollar_format()) +
        expand_limits(y = 0) +
        labs(x = "", y = "", color = NULL) +
        scale_color_manual(values = c("#6D0D19", "#38383A"))
    
    # Year LM smoother / other time units loess
    if(time_scale == "year") {
        g <- g + geom_smooth(method = "lm")
    } else {
        g <- g + geom_smooth(method = "loess", span = 0.2)
    }
    
    # Only 1 prediction
    if(n_predictions == 1) {
        g <- g + geom_point(aes(text = label_text), size = 1)
    } else {
        g <- g + geom_point(aes(text = label_text), size = 0.01)
    }
    
    ggplotly(g, tooltip = "text")
        
    
}
