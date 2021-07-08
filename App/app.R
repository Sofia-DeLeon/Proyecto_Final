library(shiny)
library(tidyverse)
felicidad <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-07/felicidad.csv")

ui <- fluidPage(
    
    fluidRow(
        column(2,titlePanel("Felicidad")),
        column(2,img(src = "imagen.jpg", height = 80, width = 150,position='lefth'))
             ),

    tabsetPanel(
        tabPanel("Bivariada",),
        tabPanel("Univariada",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("var",
                                     "Variable de interes",
                                     c("Felicidad","Calidad soporte social","Expectativa de vida","Libertad","Generosidad","Corrupción","Confianza en el gobierno","Calidad de la democracia","Gini")
                                     ),
                         selectInput("anio",
                                     "año",
                                     c(2005:2018)
                                     ),
                         checkboxGroupInput("pais", 
                                            "Paises", 
                                            c("Uruguay", "Argentina", "Chile", "Brasil", "Ecuador", "Bolivia", "Paraguay", "Perú", "Colombia", "Venezuela")
                                            ),
                         ),
                     mainPanel(plotOutput("biv")
                     )     
                 )
                 ),
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
