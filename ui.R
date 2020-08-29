library(shiny)
library(plotly)
library(shinyWidgets)
library(DT)
library(leaflet)
library(shinydashboard)
library(shinyjs)

dashboardPage(
  
  dashboardHeader(title = "Retirement Calculator"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Settings", tabName = "Settings", icon = icon("chart-bar")),
      menuItem("FIRE", tabName = "FIRE", icon = icon("map-marker")),
      menuItem("Retirement", tabName = "Retirement", icon = icon("question")),
      menuItem("About", tabName = "About", icon = icon("question"))
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    
    # https://stackoverflow.com/questions/44043475/adjust-size-of-shiny-progress-bar-and-center-it
    # center Shiny progress bar in middle of screen
    tags$head(
      tags$style(
        HTML(".shiny-notification {
              height: 100px;
              width: 800px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 400px);;
            }
           "
        )
      )
    ),
    
    tabItems(
      tabItem(tabName = "Settings",
        numericInput("age", "Current Age:", 36, min = 1, max = 78, step = 1),
        fixedRow(
          column(3,
            numericInput("brokerage_amount", "Brokerage Investments ($):", value = 10000, min = 0, step = 1)
          ),
          column(3,
            uiOutput("stock_slider")
          ),
          column(3,
            uiOutput("bond_slider")
          )
        ),
        fixedRow(
          column(3,
            numericInput("income", "Income ($):", 60000, min = 1, step = 1)
          ),
          column(3,
            numericInput("spending", "Pre-Retirement Spending ($):", 40000, min = 1, step = 1)
          ),
          column(3,
            uiOutput("savings")
          )
        ),
        numericInput("income_growth_percentage", "Income Growth Percentage:", 1, min = 0, max = 100, step = 0.1),
        numericInput("retirement_spending", "Retirement Spending ($):", 40000, min = 1, step = 1),
        numericInput("target_withdrawl_percentage", "Target Withdrawl Rate Percentage:", 4, min = 0.1, step = 0.1),
        numericInput("avg_tax_rate_percentage", "Average Tax Rate Percentage:", 7, min = 0.1, max = 100, step = 0.1),
        uiOutput("fire_target_ui"),
        fixedRow(
          column(3,
            numericInput("avg_stock_return_percentage", "Average Stock Return Percentage:", 8.1, min = 0.1, max = 100, step = 0.1)
          ),
          column(3,
            numericInput("stock_stddev", "Stock Return Percentage Standard Deviation:", 17, min = 0, max = 75, step = 0.1)
          )
        ),
        plotlyOutput("stock_histogram"),
        fixedRow(
          column(3,
            numericInput("avg_bond_return_percentage", "Average Bond Return Percentage:", 2.4, min = 0.1, max = 100, step = 0.1)
          ),
          column(3,
            numericInput("bond_stddev", "Bond Return Percentage Standard Deviation:", 7, min = 0, max = 75, step = 0.1)
          )
        ),
        plotlyOutput("bond_histogram"),
        fixedRow(
          column(3,
            numericInput("avg_inflation_percentage", "Average Inflation Percentage:", 2, min = 0.1, max = 100, step = 0.1)
          ),
          column(3,
            numericInput("inflation_stddev", "Inflation Percentage Standard Deviation:", 4, min = 0, max = 75, step = 0.1)
          )
        ),
        plotlyOutput("inflation_histogram"),
        tags$p("BUTTON HERE to FIRE")
      ),
      tabItem(tabName = "FIRE",
        DT::dataTableOutput("montecarlo_table"),
        plotlyOutput("retirement_graph")
      ),
      tabItem(tabName = "Retirement",
      ),
      tabItem(tabName = "About",
      )
    )
  )
)