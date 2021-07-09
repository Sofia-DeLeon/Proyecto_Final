library(shiny)
library(tidyverse)
library(stringi)
library(maps)


#Carga de datos
felicidad <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-07/felicidad.csv")

#Base con dato de paises y continentes
paises <- readr::read_csv("https://gist.githubusercontent.com/kintero/7d1db891401f56256c79/raw/a61f6d0dda82c3f04d2e6e76c3870552ef6cf0c6/paises.csv")
paises <- select(paises,c("nombre","continente","name"))

#Quitar tildes de la base
paises$continente<- stri_trans_general(paises$continente,"Latin-ASCII")
felicidad$pais <- stri_trans_general(felicidad$pais,"Latin-ASCII")

#join de ambas bases
x <- inner_join(felicidad, paises, by = c("pais"="nombre"))



#Definimos regiones

America <- c("Uruguay", "Argentina", "Chile", "Brazil", "Ecuador", "Bolivia", "Paraguay", "Peru", "Colombia", "Venezuela")



ui <- fluidPage(
#Titulo e imagen    
    fluidRow(
        column(2,titlePanel("Felicidad")),
        column(2,img(src = "imagen.jpg", height = 80, width = 150,position='lefth'))
    ),
#Paneles    
    tabsetPanel(
#Panel bivariada        
        tabPanel("Bivariada",),
#Panel Univariada        
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
                                            c("Uruguay", "Argentina", "Chile", "Brasil", "Ecuador", "Bolivia", "Paraguay", "Peru", "Colombia", "Venezuela")
                         ),
                     ),
                     mainPanel(plotOutput("biv")
                     )     
                 )
        ),
#Panel de tabla
        tabPanel("Tabla"),
#Panel de serie temporal
        tabPanel("Serie temporal",
                sidebarLayout(
                    sidebarPanel(
                        checkboxGroupInput("conti",
                                           "Continente",
                                           c("Europa","Africa","America","Australia y Oceania","Asia")
                                           ),
                    ),
                    mainPanel(plotOutput("serplot"))     
                )
        ),
#Panel de mapa
        tabPanel("Mapa",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("mapa",
                                     "Mapa",
                                     c("Europa","Africa","America","Oceania","Asia")
                         ),
                         selectInput("var",
                                     "Variable de interes",
                                     c("Felicidad","Calidad soporte social","Expectativa de vida","Libertad","Generosidad","Corrupción","Confianza en el gobierno","Calidad de la democracia","Gini")
                         ),
                         selectInput("anio",
                                     "año",
                                     c(2005:2018)
                         ),
                     ),
                     mainPanel(plotOutput("mapplot"))     
                 )
                 )
    )
)


server <- function(input, output) {
    
    
    output$serplot <- renderPlot({
       
        x %>% group_by(anio,continente) %>% 
            summarise(escalera_vida=mean(escalera_vida)) %>% 
            filter(continente==input$conti) %>% 
            ggplot(aes(x = anio, y = escalera_vida,group=input$conti,color=input$conti))+
            geom_point() + 
            geom_line() + 
            labs(x = "Año", y = "Puntaje de Felicidad")
        })
    
    #x %>%
       # group_by(anio,continente) %>% 
        #summarise(escalera_vida=mean(escalera_vida)) %>% 
        #ggplot(aes(x = anio, y = escalera_vida,group=continente,color=continente)) + geom_point() + geom_line() + labs(x = "Año", y = "Puntaje de Felicidad")
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)




