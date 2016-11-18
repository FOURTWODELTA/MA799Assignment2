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
                startdate = 1981,            # see documentation for format
                enddate   = 2014,            # of months and quarters
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
    title="World Development Emissions Comparison",
    header=dashboardHeader(title="Global Emissions"),
    ## Sidebar content
    sidebar=dashboardSidebar(
      sidebarMenu(
        menuItem("Introduction",tabName="home_tab",   icon=icon(name="home",lib="glyphicon")),
        menuItem("Scatter Plot", tabName="first_tab",  icon=icon(name="menu-right", lib="glyphicon")),
        menuItem("Histogram", tabName="second_tab", icon=icon(name="menu-right", lib="glyphicon"))
      )
    ),
    ## Body content
    body=dashboardBody(
      tabItems(
        # Tab content for "home_tab"
        tabItem(tabName="home_tab",
                fluidPage(
                  box(width=4, background="blue", title="Introduction",
                      "The following interactive apps are methods of visualizing
                      the emissions data in the World Bank development indicators.
                      The four largest nations: the United States (USA), China
                      (CHN), India (IND), and the Russian Federation (RUS) are
                      included, and comparisons cna be made over the time range
                      of 1981 to 2014, reported annually."),
                  
                  box(width=7, background="blue", title="Indicators used",
                      "Total Population (persons);
                      Land Area (sq.km.);
                      Nitrous oxide emissions (kt CO2 equivalent);
                      Methane emissions (kt CO2 equivalent);
                      Total greenhouse gas emissions (kt CO2 equivalent);
                      Other greenhouse gas emissions, HFC, PFC and SF6 (kt CO2 equivalent);
                      CO2 emissions(kilotons);
                      Survival to age 65, female (% of cohort);
                      Survival to age 65, male (% of cohort);
                      GDP per capita (current US $)"),
                  box(width=4, background="blue", title="Directions",
                      "Use user-input fields to select data in each chart")
                )
        ),
    
        # Tab content for "first_tab"
        tabItem(tabName="first_tab",
                fluidRow(
                  box(width=12, background="blue", title="Description",
                      "The scatter plot below shows the trend of the
                      selected variable for the selected country over
                      the selected year range"),
                  box(width=12, background="blue", title="Indicator Key",
                      "SP.POP.TOTL = Population; AG.LND.TOTL.K2 = Land Area;
                      EN.ATM.NOXE.KT.CE = Nitrous oxide emissions;
                      EN.ATM.METH.KT.CE = Methane emissions;
                      EN.ATM.GHGT.KT.CE = Total greenhouse gas emissions;
                      EN.ATM.GHGO.KT.CE = Other greenhouse gas emissions;
                      EN.ATM.CO2E.KT = CO2 emissions;
                      SP.DYN.TO65.FE.ZS = Female survival rate to 65
                      SP.DYN.TO65.MA.ZS = Male survival rate to 65
                      NY.GDP.PCAP.CD = Income per capita"),
                  selectInput("first_iso3c_scatter", "Choose country: ", 
                                            multiple=FALSE, selected="USA",
                                            choices=iso3c.codes), 
                  selectInput("indicator.codes", "Choose indicator: ", 
                                            multiple=FALSE, selected="SP.POP.TOTL",
                                            choices=indicator.codes)),
                  
                  sidebarPanel(sliderInput("first_range","Year Range:",min=1981,max=2014,value=c(200,500)))
                , 
                mainPanel(
                  plotlyOutput("ScatterPlot")
                )
        ),
        
        # tab content for "second_tab" 
        tabItem(tabName="second_tab",
                fluidPage(
                  box(width=12, background="blue", title="Description",
                      "The histogram below shows the frequency of the
                      emissions output levels for the selected variable 
                      for the selected country over the selected year range"),
                  box(width=12, background="blue", title="Indicator Key",
                      "SP.POP.TOTL = Population; AG.LND.TOTL.K2 = Land Area;
                      EN.ATM.NOXE.KT.CE = Nitrous oxide emissions;
                      EN.ATM.METH.KT.CE = Methane emissions;
                      EN.ATM.GHGT.KT.CE = Total greenhouse gas emissions;
                      EN.ATM.GHGO.KT.CE = Other greenhouse gas emissions;
                      EN.ATM.CO2E.KT = CO2 emissions;
                      SP.DYN.TO65.FE.ZS = Female survival rate to 65
                      SP.DYN.TO65.MA.ZS = Male survival rate to 65
                      NY.GDP.PCAP.CD = Income per capita"),
                  selectInput("first_iso3c", "Choose country: ", 
                              multiple=FALSE, selected="USA",
                              choices=iso3c.codes),
                  selectInput("variable","Choose indicator:",
                    multiple = FALSE,choices=indicator.codes
                  ),
                sidebarPanel(
                  sliderInput("range", "Year Range:",
                              min = 1981, max = 2014, value = c(200:500)),
                  sliderInput("binwidth","Bin Width",
                              min = 100000, max = 300000, value=5000)
                           )
                 ),
                mainPanel(
                  plotlyOutput("histogram")
                )
        )
        # You may want to create additional tabs to 
        # organize your input and output elements
      )
    )
  )

server <- function(input, output) {
  
    
  output$ScatterPlot <- renderPlotly({
      
  range = seq(from =input$first_range[1],to =input$first_range[2],by=1)

  wb.df %>%
    filter(iso3c %in% input$first_iso3c_scatter) %>%
    filter(date %in% range) -> wb.df1
    
     x.vec = wb.df1[,'date']
     
     y.vec = wb.df1[,input$indicator.codes]
      
      plot_ly(x = x.vec, y = y.vec, type="scatter") %>%
        layout(title="Scatter Plot",
               xaxis=list(title="Years"),
               yaxis=list(title="Value")) 
      }
      )
  
  output$histogram <- renderPlotly({
    
    range = seq(from =input$range[1],to=input$range[2],by=1)
    
    wb.df %>%
      filter(iso3c %in% input$first_iso3c)%>% 
      filter(date %in% range)%>%                                               
      ggplot() + 
      aes_string(x=input$variable) +
      geom_histogram(binwidth = input$binwidth) %>%
      
      {.} -> p               
    ggplotly(p)
  })
}

shinyApp(ui, server)
