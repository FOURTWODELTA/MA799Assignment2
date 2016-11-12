## app.R ##
library(shinydashboard)
library(wbstats)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# To do: Use your own list of indicator codes here:
indicator.codes = c("SP.POP.TOTL","AG.LND.TOTL.K2","EN.ATM.NOXE.KT.CE","EN.ATM.METH.KT.CE","EN.ATM.GHGT.KT.CE", "EN.ATM.GHGO.KT.CE","EN.ATM.CO2E.KT","SP.DYN.TO65.FE.ZS","SP.DYN.TO65.MA.ZS","NY.GDP.PCAP.CD")

# To do: create a vector of names of the factor indicator codes 
# that you created using the `make.ntile` function

# Retrieve a "long" table with data for 
# the chosen countries and indicator codes 
# over the chosen date range 
wb.long.df = wb(indicator = indicator.codes, # a vector of indicator codes
                country   = c("USA","CHN","RUS","IND"),           # a vector of country codes
                startdate = 1980,            # see documentation for format
                enddate   = 2015,            # of months and quarters
                freq      = "Y",             # options: "Y", "Q", "M"
                POSIXct   = TRUE             # return dates as POSIXct type
)

# Notice the dataframe is in "long" format
# We need to use the spread function to create
# indicator code variables/columns
# Create indicator variables from the `indicatorID` and `value` variables
wb.long.df %>%
  spread(., indicatorID, value) %>%
  select(., -indicator) %>%
  {.} -> wb.df

# Join the data in the `wb_cachelist$countries` dataset
# to the data in the `wb.df` dataset 
wb.df %>%
  left_join(x=., 
            y=wb_cachelist$countries) %>%
            {.} -> wb.df

# Store the sorted list of iso3c country codes
iso3c.codes = sort(unique(wb.df$iso3c))

# Create the UI element
ui <- 
  dashboardPage(
    skin="blue",
    title="change this",
    header=dashboardHeader(title="change this"),
    ## Sidebar content
    sidebar=dashboardSidebar(
      sidebarMenu(
        menuItem("Home",        tabName="home_tab",   icon=icon(name="home",       lib="glyphicon")),
        menuItem("Scatter Plot", tabName="first_tab",  icon=icon(name="menu-right", lib="glyphicon")),
        menuItem("change this", tabName="second_tab", icon=icon(name="menu-right", lib="glyphicon"))
      )
    ),
    ## Body content
    body=dashboardBody(
      tabItems(
        # Tab content for "home_tab"
        tabItem(tabName="home_tab",
                fluidRow(
                  box(width=4, background="blue", title="Introduction","change this"),
                  box(width=4, background="blue", title="Topics"      ,"change this"),
                  box(width=4, background="blue", title="Objectives"  ,"change this")
                )
        ),
        # To do: arrange the input and output items so that make a good 
        # presentation
        
        # Tab content for "first_tab"
        tabItem(tabName="first_tab",
                fluidRow(
                  box(width=12, background="blue", title="Scatter Plot"),
                  selectInput("first_iso3c", "Choose country: ", 
                                            multiple=FALSE, selected="USA",
                                            choices=iso3c.codes), 
                  # To do: create another box to select a indicator code from 
                  # the `indicator.codes` vector. You will use this selected value
                  selectInput("indicator.codes", "Choose indicator: ", 
                                            multiple=FALSE, selected="SP.POP.TOTL",
                                            choices=indicator.codes),
                  
                  sidebarPanel(sliderInput("range","Year Range:",min=1981,max=2015,value=c(200,500)))
                ), 
                mainPanel(
                  tableOutput("CountryEmissions"),
                  tableOutput("values"),
                  plotlyOutput("ScatterPlot")
                )
        ),
        
        # tab content for "second_tab" 
        tabItem(tabName="second_tab",
                fluidRow(
                  # Add input and output elements here
                )
        )
        # You may want to create additional tabs to 
        # organize your input and output elements
      )
    )
  )

server <- function(input, output) {
  
  # To do: modify this code to create a scatter plot 
  # for any pair of indicators as the x and y axes.
  # Use two calls to the selectInput function to choose 
  # the indicator codes for the X and Y variables.
  # All you need to change below are the strings 
  # "Sepal.Length" and "Sepal.Width".
  
  
  tableValues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Country", 
               "Indicator"),
      Value = as.character(c(input$iso3c.codes, 
                             input$indicator.codes), 
      stringsAsFactors=FALSE)
    )
  })
  output$values <- renderTable({
    tableValues()
  })
  
  CountryEmissions <- reactive({
    
  output$CountryEmissions <- renderTable({ 
    wb.df %>%
      input$iso3c.codes
    })
  })
  output$CountryEmissions <- renderTable({
    CountryEmissions()
  })
  
  ScatterPlot <- reactive({
    
    output$ScatterPlot <- renderPlotly({
      
      x.vec = input$range
      
      y.vec = input$CountryEmissions
      
      plot_ly(x = x.vec, y = y.vec, type="scatter") %>%
        layout(title="Scatter Plot",
               xaxis=list(title="Country"), # change this string to your chosen X indicator
               yaxis=list(title="Value")) # change this string to your chosen Y indicator
    })
  })
  output$ScatterPlot <- renderPlotly({
    ScatterPlot()
})
  }

shinyApp(ui, server)