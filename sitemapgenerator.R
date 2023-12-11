library(shiny)
library(readr)
library(readxl)
library(leaflet)

# Sample data
sample_data <- data.frame(
  lat = c(-37.8136, -33.8688, -31.9505),
  lon = c(144.9631, 151.2093, 115.8605),
  sitename = c("Melbourne", "Sydney", "Perth")
)

# Define UI
ui <- fluidPage(
  titlePanel("Site Map Generator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV/Excel File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  "text/tab-separated-values",
                  ".tsv",
                  ".xlsx"
                )
      ),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      actionButton("loadSample", "Load Sample Data"),
      uiOutput("siteSelection") # Dynamic UI for site selection
    ),
    mainPanel(
      tableOutput("contents"),
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive value to store the uploaded data
  uploadedData <- reactiveVal(NULL)
  
  # Reactive value to store the sample data
  sampleData <- reactiveVal(sample_data)
  
  # Load user data when a file is uploaded
  observeEvent(input$file1, {
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    
    uploadedData(switch(ext,
                        csv = read_csv(input$file1$datapath, col_names = input$header),
                        tsv = read_tsv(input$file1$datapath, col_names = input$header),
                        xlsx = read_excel(input$file1$datapath),
                        stop("Invalid file format")
    ))
  })
  
  # Load sample data when the button is clicked
  observeEvent(input$loadSample, {
    uploadedData(sampleData())
  })
  
  # Update site selection UI based on the uploaded data
  output$siteSelection <- renderUI({
    if (is.null(uploadedData())) return(NULL)
    checkboxGroupInput("selectedSites", "Choose Sites", choices = unique(uploadedData()$sitename))
  })
  
  # Render the table
  output$contents <- renderTable({
    if (is.null(uploadedData())) return()
    uploadedData()
  })
  
  # Render the map
  output$map <- renderLeaflet({
    if (is.null(uploadedData())) return()
    
    # Filter data based on selected sites
    map_data <- uploadedData()
    if (!is.null(input$selectedSites)) {
      map_data <- map_data[map_data$sitename %in% input$selectedSites, ]
    }
    
    leaflet(map_data) %>% 
      addTiles() %>% 
      addMarkers(lng = ~lon, lat = ~lat, popup = ~sitename)
  })
}

# Run the app
shinyApp(ui, server)

