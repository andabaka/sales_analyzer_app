# SALES PREDICTION OF A COMPANY WHICH SELL PRODUCTS ONLINE

# 1.0 Reading libraries ----
# Core
library(tidyverse)
library(tidyquant)
library(skimr) 


# Interactive Visualizations
library(plotly)

# Modeling 
library(parsnip)
library(xgboost)
library(timetk)

# 2.0. Importing data ----
sales_raw_tbl <- read.csv("sales_data_sample.csv")


# Data summary
skim(sales_raw_tbl)
sales_raw_tbl %>% glimpse()

# 3.0 Data manipulation
# Selecting important variables
sales_tbl <- sales_raw_tbl %>% 
    select(ORDERDATE, PRODUCTLINE, CUSTOMERNAME, COUNTRY, ORDERNUMBER,
           ORDERLINENUMBER, SALES)
sales_tbl %>% glimpse()

# Converting to date-time class
sales_tbl <- sales_tbl %>% 
    mutate(ORDERDATE = mdy_hm(ORDERDATE))
          
# 3.1 Time series aggregation function ----
aggregate_time_series <- function(data, time_unit = "month") {
    output_tbl <- data %>% 
    mutate(date = floor_date(ORDERDATE, unit = time_unit)) %>% 
        group_by(date) %>% 
        summarise(total_sales = sum(SALES)) %>% 
        ungroup() %>% 
        mutate(label_text = str_glue("Datum: {date}
                                 Prihodi: {scales::dollar(total_sales)}"))
    return(output_tbl)
}

sales_tbl %>% 
    aggregate_time_series(time_unit = "day")

# 3.2 Time series plot function ----

plot_time_series <- function(data){
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

sales_tbl %>% 
    aggregate_time_series(time_unit = "year") %>% 
    plot_time_series()

# 4.0 Forecasting ----
# 4.1 Training data and future data ----

# Training data
data <- sales_tbl %>% 
    aggregate_time_series("year")

train_tbl <- data %>% 
    tk_augment_timeseries_signature()

# Future data
future_data_tbl <- data %>% 
    mutate(date = as_date(date)) %>% 
    tk_index() %>% 
    tk_make_future_timeseries(n_future = 12, inspect_weekdays = TRUE, 
                              inspect_months = TRUE) %>% 
    tk_get_timeseries_signature()

# 4.2 Machine learning ----
# XGBoost
seed <- 123
set.seed(seed)

xgboost_model <- boost_tree(
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

# 4.3 Making predictions ----
prediction_tbl <- predict(xgboost_model, new_data = future_data_tbl) %>% 
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

output_tbl

# Making generate_forecast function ()
n_future <- 2
seed <- 123

generate_forecast <- function(data, n_future = 12, seed = NULL){
    train_tbl <- data %>% 
        tk_augment_timeseries_signature()
    
    
    future_data_tbl <- data %>% 
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

sales_tbl %>% 
    aggregate_time_series(time_unit = "year") %>% 
    generate_forecast(n_future = 2, seed = 123)

# 5.0 Plotting forecast ----

data <- sales_tbl %>% 
    aggregate_time_series(time_unit = "month") %>% 
    generate_forecast(n_future = 12, seed = 123)

g <- data %>% 
    ggplot(aes(date, total_sales, color = key)) +
    geom_line() +
    geom_point(aes(text = label_text), size = 0.01) +
    geom_smooth(method = "loess", span = 0.2) +
    
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "", y = "", color = NULL) +
    scale_color_manual(values = c("#6D0D19", "#38383A"))

ggplotly(g, tooltip = "text")

# 5.1 Making plotting forecast function ----
data <- sales_tbl %>% 
    aggregate_time_series(time_unit = "year") %>% 
    generate_forecast(n_future = 1, seed = 123)

plot_forecast <- function(data) {
    
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

sales_tbl %>% 
    aggregate_time_series(time_unit = "year") %>% 
    generate_forecast(n_future = 1, seed = 123) %>% 
    plot_forecast()

# 6.0 Saving functions
dump(c("aggregate_time_series", "plot_time_series", "generate_forecast",
       "plot_forecast"), file = "functions.R")

    





