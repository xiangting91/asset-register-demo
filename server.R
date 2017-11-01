library(shiny)
library(tidyverse)
library(lubridate)
library(readxl)
library(plotly)
library(ggplot2)
library(stringr)
library(dygraphs)
library(DT)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    set.seed(42)
    
    df <- expand.grid(
        ministry = c(
            "MCCY", "MCI", "MEWR", "MFA", "MHA", "MINLAW", "MND", "MOE", "MOF",
            "MOH", "MOM", "MOT", "MSF", "MTI"
            ), repl_fy = 2017:2217) %>% 
        tbl_df() %>% 
        mutate(repl_cost_real = abs(rnorm(2814) * 100000),
               months_to_inflate = 500:3313,
               asset_name = str_c("Asset ", round(abs(rnorm(2814) * 10000))),
               lat = sample(seq(from = 1.31, to = 1.41, by = 0.0000001), 2814),
               lng = sample(seq(from = 103.678, to = 103.91, by = 0.0000001), 2814))
    
    dat <- reactive({
        inflation_rates <- input$inflation_rates %>% 
            str_split(", ?") %>% 
            unlist() %>% 
            as.numeric() %>% 
            sort()
        inflation_rates <- (1 + (inflation_rates / 100))^(1/12)
        if (length(inflation_rates) == 1) {
            inflation_rate_base <- inflation_rates
            inflation_rate_best <- inflation_rates
            inflation_rate_worst <- inflation_rates
        } else if (length(inflation_rates) == 3) {
            inflation_rate_base <- inflation_rates[2]
            inflation_rate_best <- inflation_rates[1]
            inflation_rate_worst <- inflation_rates[3]
        } 
        
        df %>% 
            mutate(
                repl_cost_base = repl_cost_real * (inflation_rate_base)^months_to_inflate,
                repl_cost_best = repl_cost_real * (inflation_rate_best)^months_to_inflate,
                repl_cost_worst = repl_cost_real * (inflation_rate_worst)^months_to_inflate
            )
    })

    output$repl_cost_brekadown_plot <- renderDygraph({
        dat() %>% 
            filter(repl_fy <= year(today()) + input$years_to_project) %>% 
            select(ministry, repl_fy, repl_cost_base) %>% 
            spread(ministry, repl_cost_base) %>% 
            dygraph(xlab = "FY", ylab = "Replacement Cost", periodicity = "yearly") %>% 
            dyOptions(stackedGraph = TRUE) %>% 
            dyRangeSelector() %>% 
            dyAnnotation("2054", "Major project", width = 60, height = 40, attachAtBottom = TRUE)
    })
    
    output$repl_cost_inflation_plot <- renderDygraph({
        df <- dat() %>% 
            filter(repl_fy <= year(today()) + input$years_to_project) %>% 
            group_by(repl_fy) %>% 
            summarise(
                repl_cost_base = sum(repl_cost_base),
                repl_cost_best = sum(repl_cost_best),
                repl_cost_worst = sum(repl_cost_worst)
            ) %>% 
            ungroup()
        df %>% 
            dygraph(xlab = "FY", ylab = "Replacement Cost") %>% 
            dySeries("repl_cost_base", label = "Base Case") %>% 
            dySeries("repl_cost_best", strokePattern = "dashed", label = "Best Case") %>% 
            dySeries("repl_cost_worst", strokePattern = "dashed", label = "Worst Case") %>% 
            dyRangeSelector()
    })
    
    output$repl_cost_tbl <- renderDataTable({
        dat() %>% 
            select(
                `Ministry` = ministry,
                `FY` = repl_fy,
                `Asset` = asset_name,
                `Real Replacement Cost` = repl_cost_real,
                `Nominal Replacement Cost (Base Case)` = repl_cost_base,
                `Nominal Replacement Cost (Best Case)` = repl_cost_best,
                `Nominal Replacement Cost (Worst Case)` = repl_cost_worst,
                `Latitude` = lat,
                `Longitude` = lng
            )
    }, filter = "top")
    
    output$repl_map <- renderLeaflet({
        
        
        df <- dat() %>% 
            mutate(
                popup = str_c(
                    "<b>", asset_name, "</b>",
                    "<br><b>Replacement Cost:</b> ", repl_cost_base,
                    "<br><b>Replacement Year:</b> ", repl_fy),
                `Years from Today` = repl_fy - year(now())) 
        
        pal <- colorNumeric("OrRd", df$`Years from Today`, reverse = TRUE)
        
        df %>% 
            sample_n(100) %>% 
            leaflet() %>% 
            addTiles(urlTemplate = "http://maps-{s}.onemap.sg/v2/Grey/{z}/{x}/{y}.png") %>% 
            addCircleMarkers(
                lng = ~lng, lat = ~lat, 
                radius = ~log(repl_cost_base), 
                color = "black",
                weight = 2,
                fillColor = ~pal(`Years from Today`),
                fillOpacity = 0.5,
                # clusterOptions = markerClusterOptions(),
                popup = ~popup
            ) %>% 
            addLegend("bottomright", pal = pal, values = ~`Years from Today`,
                      title = "Years from Today",
                      opacity = 0.9
            )
    })
})
