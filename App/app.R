library(shiny)


ui <- fluidPage(
    
    fluidRow(
        column(2,titlePanel("Felicidad")),
        column(2,img(src = "imagen.jpg", height = 80, width = 150,position='lefth'))
             ),

    tabsetPanel(
        tabPanel("Bivariada", 
              
        ),
        tabPanel("Univariada"),
        tabPanel("Tabla"),
        tabPanel("Serie temporal"),
        tabPanel("Mapa")
    )
)

    # Sidebar with a slider input for number of bins 
    

# Define server logic required to draw a histogram
server <- function(input, output) {


}

# Run the application 
shinyApp(ui = ui, server = server)
