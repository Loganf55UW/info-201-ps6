#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# First tidy up and join together data
government_spending <- read_delim("./governmentspending.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      title = "Information",
      
      p("This app uses government spending data from the World Bank."),
      p("The dataset contains ",
        strong(nrow(government_spending)),
        " observations, and ",
        strong(ncol(government_spending)),
        " variables."
        ),
      p("This is a small random sample of some of the data:"),
      tableOutput("sample_table")
    ),
    
    tabPanel(
      # Application title
      title = "Plot",
      
      # Sidebar with indicator for response variables 
      sidebarLayout(
        sidebarPanel(
          textOutput("plot_info"),
          checkboxInput("show_lines",
                        "Display lines?"),
          selectInput("country_selection",
                      "Countries to View:",
                      government_spending$Country_Name,
                      multiple = TRUE,)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("spendingPlot"),
          textOutput("plot_summary")
        )
    )),
    
    tabPanel(
      title = "Table",
      
      sidebarLayout(
        sidebarPanel(
          p("This panel displays aggregate information about 
            the dataset. You can choose what kind of aggregate information
            to display, and whether it should be aggregated by year, or by country."
            ),
          radioButtons("aggregate_by",
                       "Aggregate by:",
                       c("Country", "Year")
                       ),
          checkboxGroupInput("aggregates",
                             "Aggregate calculations:",
                             choiceNames = c("Minimum", "Maximum", "Median", "Mean"),
                             choiceValues = c("min", "max", "median", "mean"),
                             selected = c("min", "max", "median", "mean")
                             )
        ),
        
        mainPanel(
          textOutput("table_summary"),
          tableOutput("table_table")
        )
      )
    )
  )

    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    data <- reactive({
      government_spending %>%
        filter(Country_Name %in% input$country_selection) %>%
        pivot_longer(
          cols = matches("[0-9]{4}"),
          names_to = "year",
          values_to = "spending"
        ) %>%
        filter(!is.na(spending))
    })
    
    output$sample_table <- renderTable({
      government_spending %>%
        slice_sample(n = 5) %>%
        select(Country_Name, Country_Code, "2000":"2008")
    })

    output$spendingPlot <- renderPlot({
      # Show based on country in input
       data() %>%
        ggplot(aes(x = year, y = spending, color = Country_Name)) +
          (if (input$show_lines) geom_path(aes(group = Country_Name)) else geom_point()) +
          labs(x = "Year",
               y = "Domestic General Government Health Expenditure P.C., PPP",
               color = "Countries")
    })
    
    output$plot_info <- renderText("Here, you can see general domestic government spending per capita on health for different countries over time.")
    output$plot_summary <- renderText({
      if (length(input$country_selection) == 0) {
        "No countries selected."
      } else {
        average_spending <- data() %>%
          group_by(Country_Name) %>%
          summarize(average = mean(spending)) %>%
          arrange(desc(average))
        paste(sep = "",
              "The country with the highest average spending is ",
              average_spending$Country_Name[1],
              ", with an average of ",
              average_spending$average[1],
              ".")
      }
    })
    
    output$table_table <- renderTable({
      if (input$aggregate_by == "Country") {
        government_spending %>%
          select(Country_Name, "2000":"2019") %>%
          pivot_longer(
            cols = matches("[0-9]{4}"),
            names_to = "year",
            values_to = "spending"
          ) %>%
          filter(!is.na(spending) & is.double(spending)) %>%
          select(Country_Name, year, spending) %>%
          group_by(Country_Name) %>%
          summarize_at(c("spending"), funs(min, max, median, mean), na.rm = TRUE) %>%
          select(any_of(c("Country_Name", input$aggregates)))
      } else if (input$aggregate_by == "Year") {
        government_spending %>%
          select("1960":"2021") %>%
          pivot_longer(
            names_to = "year",
            values_to = "spending",
            cols = everything()
          ) %>%
          group_by(year) %>%
          filter(!is.na(spending)) %>%
          summarize_at(c("spending"), funs(min, max, median, mean), na.rm = TRUE) %>%
          select(any_of(c("year", input$aggregates)))
      }
    })
    
    output$table_summary <- renderText({
      if (input$aggregate_by == "Country") {
        paste(
          sep = "",
          "There are ",
          nrow(government_spending %>%
                 select(Country_Name, "2000":"2019") %>%
                 distinct()
               ),
          " valid countries in the aggregations."
        )
      } else {
        "There are 20 valid years in the aggregations."
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
