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

#Modificamos el nombre del continente

paises <- paises %>% 
    mutate(continente = recode(continente, "Australia y Oceanía" = "Oceania"))

#join de ambas bases
x <- inner_join(felicidad, paises, by = c("pais"="nombre"))


#Definimos regiones

America <- c("Uruguay", "Argentina", "Chile", "Brazil", "Ecuador", "Bolivia", "Paraguay", "Peru", "Colombia", "Venezuela")

#Renombarmos las variables del conjunto de datos


colnames(x) <- c("Pais","Ano","Felicidad","PIB","Calidad soporte social","Expectativa de vida","Libertad","Generosidad","Corrupción",
                 "Afecto Positivo","Afecto Negativo","Confianza en el gobierno","Calidad de la democracia","Calidad Servicios",
                 "de_escalera_pais_anio","Gini","gini_banco_mundial_promedio","continente","name")


#Datos para mapa
mapa <- inner_join(felicidad, paises, by = c("pais" = "name"))

mapa <- mapa %>%
    rename(region = pais)

mapa_mundo <- map_data("world")

mapa_felicidad <- left_join(mapa_mundo, mapa, by = "region")


ui <- fluidPage(
#Titulo e imagen    
    fluidRow(
        column(2,titlePanel("Felicidad")),
        column(2,img(src = "imagen.jpg", height = 80, width = 150,position='lefth'))
    ),
#Paneles    
    tabsetPanel(
#Panel bivariada        
        tabPanel("Bivariada",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("varx",
                                     "Variable X",
                                     c("Felicidad","PIB","Calidad soporte social","Expectativa de vida","Libertad","Generosidad","Corrupción",
                                       "Afecto Positivo","Afecto Negativo","Confianza en el gobierno","Calidad de la democracia","Calidad Servicios",
                                       "de_escalera_pais_anio","Gini")),
                         selectInput("vary",
                                     "Variable Y",
                                     c("Felicidad","PIB","Calidad soporte social","Expectativa de vida","Libertad","Generosidad","Corrupción",
                                       "Afecto Positivo","Afecto Negativo","Confianza en el gobierno","Calidad de la democracia","Calidad Servicios",
                                       "de_escalera_pais_anio","Gini")),
                         selectInput("contib",
                                     "Continente",
                                     c("Europa","Africa","America","Oceania","Asia")
                         ),
                         selectInput("anio",
                                     "año",
                                     c(2005:2018)
                         ),
                     ),
                     mainPanel(plotOutput("biv")
                     )     
                 )
                 ),
#Panel Univariada        
        tabPanel("Univariada",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("var",
                                     "Variable de interes",
                                     c("Felicidad","PIB","Calidad soporte social","Expectativa de vida","Libertad","Generosidad","Corrupción",
                                       "Afecto Positivo","Afecto Negativo","Confianza en el gobierno","Calidad de la democracia","Calidad Servicios",
                                       "de_escalera_pais_anio","Gini")),
                         selectInput("contiu",
                                     "Continente",
                                     c("Europa","Africa","America","Oceania","Asia")
                         ),
                        
                     ),
                     mainPanel(plotOutput("Uni", width = "100%")
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
                                           c("Europa","Africa","America","Oceania","Asia")
                                           ),
                    ),
                    mainPanel(plotOutput("serplot"))     
                )
        ),
#Panel de mapa
        tabPanel("Mapa",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("var",
                                     "Variable de interes",
                                     c("escalera_vida","soporte_social","expectativa_vida","libertad","generosidad","percepcion_corrupcion","confianza","calidad_democracia","gini_banco_mundial")
                         )
                     ),
                     mainPanel(plotOutput("mapplot"))     
                 )
                 )
    )
)


server <- function(input, output) {
    

#Grafico de Bivariado    
    
    ploteob <- reactive( if (input$anio == 2018){
            scat <- x %>% filter(Ano==2018) 
            gg1 <- scat %>%
                ggplot() +
                geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]]))
            print(gg1)
    })
    
    
#Grafico de univariado    
    ploteou <- reactive( 
        if (input$contiu == "America") {
            box <- x %>% filter(continente=="America") 
            gg <- box %>%
                ggplot() +
                geom_boxplot(data=box,aes(x =reorder(box$Pais,.data[[input$var]]), y = .data[[input$var]]))+
                labs(y=input$var, x="Pais") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 90,    
                                                 size = 12))+
                theme(aspect.ratio = 0.5)
            print(gg)
        }else if(input$contiu == "Europa"){
            box <- x %>% filter(continente=="Europa") 
            gg <- box %>%
                ggplot() +
                geom_boxplot(data=box,aes(x =reorder(box$Pais,.data[[input$var]]), y = .data[[input$var]]))+
                labs(y=input$var, x="Pais") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 90,    
                                                 size = 12))+
                theme(aspect.ratio = 0.5)
            print(gg)
        }else if(input$contiu == "Africa"){
            box <- x %>% filter(continente=="Africa") 
            gg <- box %>%
                ggplot() +
                geom_boxplot(data=box,aes(x =reorder(box$Pais,.data[[input$var]]), y = .data[[input$var]]))+
                labs(y=input$var, x="Pais") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 90,    
                                                 size = 12))+
                theme(aspect.ratio = 0.5)
            print(gg)
        }else if(input$contiu == "Asia"){
            box <- x %>% filter(continente=="Asia") 
            gg <- box %>%
                ggplot() +
                geom_boxplot(data=box,aes(x =reorder(box$Pais,.data[[input$var]]), y = .data[[input$var]]))+
                labs(y=input$var, x="Pais") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 90,    
                                                 size = 12))+
                theme(aspect.ratio = 0.5)
            print(gg)
        }else{
            box <- x %>% filter(continente=="Australia y Oceania") 
            gg <- box %>%
                ggplot() +
                geom_boxplot(data=box,aes(x =reorder(box$Pais,.data[[input$var]]), y = .data[[input$var]]))+
                labs(y=input$var, x="Pais") +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 90,    
                                                 size = 12))+
                theme(aspect.ratio = 0.5)
            print(gg)
        })
    
    
    
    
    output$serplot <- renderPlot({
       
        x %>% group_by(Ano, continente) %>% 
            summarise(Promedio_felicidad = mean(Felicidad)) %>% 
            ggplot(aes(x = Ano, y = Promedio_felicidad, group = input$conti)) +
            geom_point() + 
            geom_line() +
            labs(x = "Año", y = "Puntaje promedio de Felicidad")
        })
    
    
    output$mapplot <- renderPlot({
        
        ggplot(mapa_felicidad, aes(long, lat, group = group))+
            geom_polygon(aes(fill = .data[[input$var]]), color = "white") +
            scale_fill_viridis_c() + labs(x = "Longitud", y = "Latitud")
        
    })
    
    output$Uni <- renderPlot({ploteou()}, height = 800, width = 1200)
    output$biv <- renderPlot({ploteob()})
    
}

# Run the application 
shinyApp(ui = ui, server = server)




