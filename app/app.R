library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Population of Metro Areas (2015-2022)", windowTitle = "Population of Metro Areas"),  # Center title
  sidebarLayout(
    sidebarPanel(
      helpText("Select a metro area to view its population from 2015 to 2022."),
      selectInput("metroArea", "Choose a Metro Area:", choices = NULL),  # Dropdown for metro areas
      
      # displaying metadata
      em("Census data from IPUMS NHGIS, University of Minnesota, www.nhgis.org", style="font-size:10px;")
    ),
    mainPanel(
      plotOutput("populationPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Read the CSV data
  population_data <- read.csv("metro_population.csv")
  
  # Update the choices for the select input based on the unique metro area names
  updateSelectInput(session, "metroArea", choices = unique(population_data$NAME))
  
  # Render the plot
  output$populationPlot <- renderPlot({
    req(input$metroArea)  # Ensure that a metro area is selected
    
    # Prepare data for the selected metro area
    selected_data <- population_data[population_data$NAME == input$metroArea, ]
    years <- as.numeric(c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))  # Convert years to numeric
    populations <- as.numeric(selected_data[1, 3:10])
    
    # Create the scatter plot with a line of best fit
    ggplot(data.frame(Year = years, Population = populations), aes(x = Year, y = Population)) +
      geom_smooth(method = "lm", se = FALSE, color = "#AAAAAA", linetype = "dashed") +  # Line of best fit
      geom_point(color = "#d95e07", size = 4) +  # Larger points with specified color
      labs(title = paste("Population of", input$metroArea, "(2015-2022)"),
           x = "Year",
           y = "Population") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14),  # Increase numeric label size
        axis.title = element_text(size = 16, vjust = 1.5),  # Offset axis titles
        axis.title.x = element_text(vjust = -0.5),  # Offset x-axis title
        axis.title.y = element_text(vjust = 3)    # Offset y-axis title
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
