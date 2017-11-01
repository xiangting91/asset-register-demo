library(shiny)
library(shinyBS)
library(shinythemes)
library(dygraphs)
library(DT)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(navbarPage(
    "Asset Register",
    theme = shinytheme("united"),
    tabPanel(
        "Replacement Costs",
        fluidRow(
            column(
                width = 3,
                selectInput(
                    "breakdown_level",
                    "Breakdown Level:",
                    choices = c(
                        "WOG", "WOG - Asset Category",
                        "Ministry", "Ministry - Asset Category",
                        "Ministry - Agency", "Ministry - Agency - Asset Category"
                    ),
                    selected = "Ministry"
                )
            ),
            column(
                width = 2,
                selectInput(
                    "chart_type",
                    "Chart Type:",
                    choices = c(
                        "Clustered Bar", "Clustered Bar (100%)",
                        "Area"
                    ),
                    selected = "Area"
                )
            ),
            column(
                width = 2,
                textInput(
                    "inflation_rates",
                    "Inflation Rate/s:",
                    value = "2, 4, 5"
                ),
                bsTooltip(
                    "inflation_rates",
                    "Type in inflation rate/s. Either type in 1 inflation rate,
                    or 3 (base case, worst case, best case). If using 3 inflation 
                    rates, split them by comma."
                )
                ),
            column(
                width = 2,
                sliderInput(
                    "years_to_project",
                    "Years to project:",
                    min = 1,
                    max = 200,
                    value = 50
                )
            ),
            column(
                width = 3,
                selectInput(
                    "y_axis_metric",
                    "Metric:",
                    choices = c(
                        "Real Replacement Cost", "Nominal Replacement Cost",
                        "Nominal Replacement Cost (% of GDP)",
                        "Size of Asset due for Replacement"
                    ),
                    selected = "Nominal Replacement Cost"
                )
            )
        ),
        
        fluidRow(
            column(
                width = 6,
                dygraphOutput("repl_cost_brekadown_plot")
            ),
            column(
                width = 6,
                dygraphOutput("repl_cost_inflation_plot")
            )
        ),
        
        dataTableOutput("repl_cost_tbl")
    ),
    
    tabPanel(
        "Asset Heatmap",
        selectInput(
            "asset_heatmap_col_by",
            "Colour By:",
            choices = c("Years to Next Replacement", "Age of Asset"),
            selected = "Years to Next Replacement"
        ),
        leafletOutput("repl_map")
    )
)))
