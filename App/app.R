library(shiny)
library(leaflet)
library(shinyjs)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(shinythemes)
library(plotly)

#Token 
rsconnect::setAccountInfo(name='damaris24', token='A52B8448D04C73A35CF905CA8D6D12EF', secret='yrkzSmbR4yugstk1C+7tptpZRxo+IGmqWJmwnLKe')

# Load database
load("data.RData")

# Load country data
world <- ne_countries(scale = "medium", returnclass = "sf")
country <- world %>% filter(admin == "Poland") #

# UI
ui <- fluidPage(theme = shinytheme("superhero"),
                headerPanel(h1(strong("Biodiversity in Poland"),align="center")),
                useShinyjs(),
                titlePanel("Select a species"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Common name
                    
                    selectInput(
                      inputId = "nombreV",
                      label = h4("Common name:"),
                      choices = unique(data$vernacularName),
                      selected = NULL
                    ),
                    
                    hr(),
                    # Sci name
                    
                    div(id = "nombreS_div",
                        selectInput(
                          inputId = "nombreS",
                          label = h4("Scientific name:"),
                          choices = unique(data$scientificName),
                          selected = NULL
                        )
                    )
                  ),
                  
                  mainPanel(
                    leafletOutput("map", width = "90%", height = "300px"),
                    hr(),
                    hr(),
                    plotlyOutput("timeline", width = "90%", height = "300px")
                  )
                )
)

# Server

server <- function(input, output, session) {
  # Hide Scientific name list
  shinyjs::hide(id = "nombreS_div")
  
  # Store filtered data
  val <- reactiveValues(
    selectedData = data,
    selected = NULL
  )
  
  # Link scientific name and common name
  observeEvent(input$nombreV, {
    if (!is.null(input$nombreV) && input$nombreV != "") {
      val$selected <- data[data$vernacularName == input$nombreV, ]
      updateSelectInput(session, "nombreS", selected = val$selected$scientificName)
      shinyjs::show(id = "nombreS_div")  
    } else {
      shinyjs::hide(id = "nombreS_div")  # Hide the list of scientific name options
    }
  })
  
  # Link common name and scientific name
  observeEvent(input$nombreS, {
    if (!is.null(input$nombreS) && input$nombreS != "") {
      val$selected <- data[data$scientificName == input$nombreS, ]
      updateSelectInput(session, "nombreV", selected = val$selected$vernacularName)
    }
  })
  
  # Map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = country, color = "blue", weight = 2, opacity = 1, fillOpacity = 0.1) %>%
      fitBounds(
        min(val$selectedData$longitudeDecimal), min(val$selectedData$latitudeDecimal),
        max(val$selectedData$longitudeDecimal), max(val$selectedData$latitudeDecimal)
      )
  })
  
  # Update marks on the map according to the selected species
  observe({
    leafletProxy("map", data = val$selected) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitudeDecimal, lat = ~latitudeDecimal,
        radius = 5,
        color = "green"
      )
  })
  
  # Timeline
  output$timeline <- renderPlotly({
    if (!is.null(val$selected)) {
      plot_ly(val$selected, x = ~eventDate, y = ~individualCount, type = 'bar') %>%
        layout(title = "Observations Timeline",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Individual Count"))
    } else {
      plot_ly()
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)