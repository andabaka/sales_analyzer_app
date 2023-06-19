# SALES FORECASTING SHINY APP

# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)

library(plotly)
library(tidyverse)
library(tidyquant)


library(parsnip)
library(xgboost)
library(timetk)

source(file = "functions.R")

# LOAD DATA ----
sales_raw_tbl <- read.csv("sales_data_sample.csv")
sales_raw_tbl

# Selecting important variables
sales_tbl <- sales_raw_tbl %>% 
    select(ORDERDATE, PRODUCTLINE, CUSTOMERNAME, COUNTRY, ORDERNUMBER,
           ORDERLINENUMBER, SALES, STATUS)

# Converting to date-time class
sales_tbl <- sales_tbl %>% 
    mutate(ORDERDATE = mdy_hm(ORDERDATE))

# USER INTERFACE ----
ui <- navbarPage(
    title = "Analyzer App",
    inverse = FALSE,
    collapsible = TRUE,
    theme = shinytheme("paper"),
    tabPanel(
        title = "Prodaja"
    ),
    
    # CSS ----
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    # JS ----
    shinyjs::useShinyjs(),
    
    # 1.0 HEADER ----
    div(
        class = "container",
        id = "header",
        h1(class = "page-header", "Analyzer App", tags$small("by marijana andabaka")),
        p(class = "lead", "Projekt je napravljen u svrhe izrade osobnog",
        a(href = "https://mandabaka.netlify.app/", target = "_blank", "portfolia"), 
        "te su u njemu primijenjenja znanja i tehnike sa",
        a(href = "https://www.business-science.io/university/2019/05/16/course-launch-shiny-web-application-level-1.html",
          target = "_blank", "Business Science University DS4B-102 tečaja."), "U Analyzer App-u je prikazana analiza i 
          predikcija prodaje izvršenih online transakcija u periodu od 2003-2005 za tvrtku koja prodaje dijelove za 
        vozila na svjetskom tržištu.")
   ),
   
   # 2.0 INFO CARDS ----
   
   div(
       class = "container hidden-sm hidden-xs",
       id = "cards",
       div(
           class = "",
           column(
               width = 12,
               h5(class = "pull-left", "Info kartice"),
               actionButton(inputId = "cards_toogle", "Prikaži/Sakrij", class = "pull-right")
           )
       ),
       div(
           class = "row",
           id = "card_section",
           uiOutput(outputId = "info_cards", class = "container")
       )
       
        
    
    ),
   
   # 3.0 APPLICATION UI ----
   div(
       class = "container",
       id = "application_ui",
       br(),
       # 3.1 USER INPUTS ----
       column(
           width = 4,
           wellPanel(
               div(
                   id = "input_main",
                   # 3.1.1 Date input ----
                   shiny::dateRangeInput(
                       inputId = "date_range",
                       label = "Datum",
                       start = "2003-01-06",
                       end = "2005-05-31",
                       min = "2003-01-06",
                       max = "2005-05-31",
                       format = "dd-mm-yyyy",
                       language = "hr",
                       separator = "-",
                       autoclose = TRUE
                   ),
                   br(),
                   # 3.1.2 Country input ----
                       pickerInput(
                       inputId = "picker_country",
                       label = "Država",
                       choices = sort(unique(sales_tbl$COUNTRY)),
                       selected = unique(sales_tbl$COUNTRY),
                       multiple = TRUE,
                       options = pickerOptions(
                           actionsBox = TRUE,
                           liveSearch = TRUE,
                           size = 10
                       )
                   ),
                   br(),
                   # 3.1.3 Customer input----
                   pickerInput(
                       inputId = "picker_customer",
                       label = "Kupac",
                       choices = sort(unique(sales_tbl$CUSTOMERNAME)),
                       selected = sort(unique(sales_tbl$CUSTOMERNAME)),
                       multiple = TRUE,
                       options = pickerOptions(
                           actionsBox = TRUE,
                           liveSearch = TRUE,
                           size = 10
                       )
                   ),
                   br(),
                   # 3.1.4 Product input ----
                   pickerInput(
                       inputId = "product_picker",
                       label = "Linija proizvoda",
                       choices = sort(unique(sales_tbl$PRODUCTLINE)),
                       selected = sort(unique(sales_tbl$PRODUCTLINE)),
                       multiple = TRUE,
                       options = pickerOptions(
                           actionsBox = TRUE,
                           liveSearch = TRUE,
                           size = 10
                       )
                   ),
                   br(),
                   # 3.1.5 Status input ----
                   pickerInput(
                       inputId = "status_picker",
                       label = "Status",
                       choices = sort(unique(sales_tbl$STATUS)),
                       selected = sort(unique(sales_tbl$STATUS)),
                       multiple = TRUE,
                       options = pickerOptions(
                           actionsBox = TRUE,
                           liveSearch = TRUE,
                           size = 10)
                       ),
               br(),
               # 3.1.6 Forecast button ----
               materialSwitch(
                   inputId = "forecast",
                   label = "Predikcija",
                   right = TRUE
               ),
               disabled(sliderInput(inputId = "forecast_horizon",
                    label = "Vremenski horizont",
                    value = 12,
                    min = 1,
                    max = 100
                )),
               # 3.1.7 Apply&Reset buttons ----
                   actionButton(
                       inputId = "apply",
                       label = "Primijeni",
                       icon = icon("check-double")
                   ),
                   actionButton(
                       inputId = "reset",
                       label = "Poništi",
                       icon = icon("trash-can")
                   )
                )
             ) 
       ),
       # 4.0 PLOTS ----
       # 4.1 RadioGroupButtons ----
       column(
           width = 8,
           radioGroupButtons(
               inputId = "time_unit",
               choices = c("Dan"="day", "Tjedan"="week", "Mjesec"="month",
                           "Kvartal"="quarter", "Godina"="year"),
               selected = "month",
               justified = TRUE
           ),
           # 4.2 Time-series plot ----
           div(
               class= "panel",
               div(
                   class = "panel-header",
                h5("Prodaja tijekom vremena")),
                div(
                   class = "panel-body bg-primary",
                   plotlyOutput(outputId = "plotly_1")))
           
       ),
       # 4.3 Country plot ----
       column(
           width = 8,
           div(
               class = "panel",
               div(
                    class = "panel-header",
                    h5("Prodaja po državama")),
               div(
                   class = "panel-body bg-primary",
                   plotlyOutput(outputId = "plotly_2"))
           )
       )
       
   )
   
)


# SERVER ----
server <- function(input, output, session){
  
    # 5.0 UPDATE INPUT SETTINGS AFTER RESET ----
    observeEvent(eventExpr = input$reset,
                 handlerExpr = {
                     # 5.1 Update date range input ----
                     updateDateRangeInput(
                         session = session,
                         inputId = "date_range",
                         start = "2003-01-06",
                         end = "2005-05-31",
                         min = "2003-01-06",
                         max = "2005-05-31")
                     # 5.2 Update country input ----
                     updatePickerInput(
                         session = session,
                         inputId = "picker_country",
                         selected = unique(sales_tbl$COUNTRY))
                    # 5.3 Update customer input ---- 
                    updatePickerInput(
                         session = session,
                         inputId = "picker_customer",
                         selected = sort(unique(sales_tbl$CUSTOMERNAME)))
                    # 5.4 Update product input ----
                    updatePickerInput(
                        session = session,
                        inputId = "product_picker",
                        selected = sort(unique(sales_tbl$PRODUCTLINE)))
                    # 5.5 Update status input ----
                    updatePickerInput(
                        session = session,
                        inputId = "status_picker",
                        selected = sort(unique(sales_tbl$STATUS))
                    )
                    # 5.6 Update time-series plot ----
                    updateRadioGroupButtons(
                        session = session,
                        inputId = "time_unit",
                        selected = "month"
                    )
                    
                    # 5.7 Update materialSwitch ----
                    updateMaterialSwitch(
                        session = session,
                        inputId = "forecast",
                        value = FALSE
                    )
                    # 5.8 Update sliderInput ----
                    updateSliderInput(
                        session = session,
                        inputId = "forecast_horizon",
                        value   = 12)
                     
                 })
                    
    # 6.0 RUN REACTIVE CODE WHEN APPLY BUTTON IS CLICKED ----
        sales_filtered_tbl <- eventReactive(
            eventExpr = input$apply,valueExpr = {
                             sales_tbl %>% 
                     # Date filter
                      filter(ORDERDATE %>% between(left = as_datetime(input$date_range[1]),
                                                              right = as_datetime(input$date_range[2]))) %>% 
                     # Country filter
                      filter(COUNTRY %in% input$picker_country) %>% 
                     # Product filter
                      filter(PRODUCTLINE %in% input$product_picker) %>% 
                     # Customer filter
                      filter(CUSTOMERNAME %in% input$picker_customer) %>% 
                     # Status filter
                     filter(STATUS %in% input$status_picker)
                },
                    ignoreNULL = FALSE # run code when app loads
            )
                     
    # Delay click on apply button
                shinyjs::delay(ms = 300, expr = {
                        shinyjs::click(id = "apply")
                    })
          
    
    # Enable sliderInput
    observeEvent(input$forecast, {
        if(input$forecast == TRUE) {
            enable("forecast_horizon")
        }else{disable("forecast_horizon")}
    })
    
    # When time radio group button is pressed act like apply button
    observeEvent(eventExpr = input$time_unit, {
        if(input$forecast) {
            delay(300, click(id = "apply"))
        }
    })
    
    # When forecast is switched act like apply but only once
    observeEvent(eventExpr = input$forecast, {
        delay(300, click(id = "apply"))
    }, once = TRUE)
    
    
    # 7.0 INFO CARDS ----
    # 7.1 Show/Hide Cards ----
    observeEvent(input$cards_toogle, {
        shinyjs::toggle(id = "info_cards", anim = TRUE, animType = "slide")
    })
    # 7.2 Reactive values ----
    summary_tbl <- reactive({
        sales_filtered_tbl() %>% 
            summarise(
                orders = unique(ORDERNUMBER) %>% length(),
                total_sales = sum(SALES),
                shipped = sum(str_detect(STATUS, "Shipped")) /
                    (length(STATUS) + 0.0001)
            )
        })
    output$info_cards<-renderUI({
        # 7.3 Card Total orders ----
        cards <- div(
            class = "container",
            column(
            width = 3,
            div(
                class = "panel panel-default",
                div(
                    class = "panel-body bg-default",
                    p(class = "pull-right",icon(class = "fa-2x","file-import"),
                    icon(class = "fa-4x", "info"),
                    h5(class = "text-left", "Narudžbe"),
                    h5(class= "text-left",summary_tbl()$orders,
                    icon(case_when(summary_tbl()$orders<50 ~ "bolt", 
                                   summary_tbl()$orders<100 ~ "triangle-exclamation",
                                   TRUE~"thumbs-up")),
                    class = case_when(summary_tbl()$orders<50 ~ "text-danger", 
                                      summary_tbl()$orders<100 ~ "text-warning",
                                      TRUE~"text-success"))
                    )
                )
            )  
        ),
        # 7.4 Card Total Sales ----
        column(
            width = 3,
            div(
                class = "panel panel-default",
                div(
                    class = "panel-body bg-default",
                    p(class = "pull-right",icon(class = "fa-2x","money-bill-1-wave"),
                      icon(class = "fa-4x", "info"),
                      h5(class = "text-left", "Prodaja"),
                      h5(class= "text-left",summary_tbl()$total_sales %>% scales::dollar(scale = 1e-6, suffix="M", accuracy = 0.01),
                         icon(case_when(summary_tbl()$total_sales<500000 ~ "bolt", 
                                        summary_tbl()$total_sales<1.5e6 ~ "triangle-exclamation",
                                        TRUE~"thumbs-up")),
                         class = case_when(summary_tbl()$total_sales<500000 ~ "text-danger", 
                                           summary_tbl()$total_sales<1.5e6 ~ "text-warning",
                                           TRUE~"text-success"))
                    )
                )
            )  
        ),
        # 7.5 Card Total delivery ----
        column(
            width = 3,
            div(
                class = "panel panel-default",
                div(
                    class = "panel-body bg-default",
                    p(class = "pull-right",icon(class = "fa-2x","truck"),
                      icon(class = "fa-4x", "info"),
                      h5(class = "text-left", "Isporučeno"),
                      h5(class= "text-left",summary_tbl()$shipped %>% scales::percent(),
                         icon(case_when(summary_tbl()$shipped<0.9 ~ "bolt", 
                                        summary_tbl()$shipped<0.95 ~ "triangle-exclamation",
                                        TRUE~"thumbs-up")),
                         class = case_when(summary_tbl()$shipped<0.9 ~ "text-danger", 
                                           summary_tbl()$shipped<0.95 ~ "text-warning",
                                           TRUE~"text-success"))
                    )
                )
            )  
        )
    )
})
    
    # 8.0 Time-series plot ----
    # 8.1 Reactive event for forecast switch----
    time_plot_tbl <- reactive({
        
        sales_filtered_tbl() %>%
            aggregate_time_series(time_unit = input$time_unit)
        
    })
   
    time_plot_predictions_tbl <- eventReactive(eventExpr = input$apply, {
        
        if (input$forecast) {
            time_plot_tbl() %>%
                generate_forecast(n_future = input$forecast_horizon, seed = 123)
        }
        
    })
    
    # 8.2 Plotly Output 1 ----
    output$plotly_1 <- renderPlotly({
        
        if (input$forecast) {
            p <- time_plot_predictions_tbl() %>%
                plot_forecast()
        } else {
            p <- time_plot_tbl() %>%
                plot_time_series()
        }
        
    })
    
    # 9.0 Sales by Country ----
    # 9.1 Reactive expression based on input----
    geo_plot_tbl <- reactive({
        sales_filtered_tbl() %>% 
            group_by(COUNTRY) %>% 
            summarise(total_country_sales = sum(SALES)) %>% 
            ungroup() %>% 
            mutate(label_text = str_glue("Država: {COUNTRY}
                                         Prihodi: {scales::dollar(total_country_sales)}")
            )
    })
    
    # 9.2 Output Geo plotly ----
    output$plotly_2 <- renderPlotly(expr = {
        
        geo_plot_tbl() %>%
            
            plot_geo(locationmode = "country names") %>%
            
            add_trace(z = ~total_country_sales,
                      locations = ~COUNTRY,
                      color = ~total_country_sales,
                      text = ~label_text,
                      marker = list(line = list(
                          color = toRGB("black"), width = 0.8)), 
                      colors = c("#e8d9db","#414a37"),
                      hoverinfo = "text") %>%
            
            colorbar(title = 'Prihodi',
                     tickprefix = '$',
                     x = 1, y = 0.8) %>% # Color Gradient
            
            layout(
                showlegend = FALSE,
                geo = list(  # Specify map options
                    scope = "world",
                    bgcolor = toRGB("white", alpha = 0),
                    countrycolor = toRGB("gray"),
                    showcountries = TRUE,
                    showframe = FALSE,
                    showcoastlines = FALSE,
                    coastlinecolor = toRGB("#ECEFF1"),
                    projection = list(type = 'Equirectangular')))
    })

}

shinyApp(ui, server)
