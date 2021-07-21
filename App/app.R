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


colnames(x) <- c("Pais","Ano","Felicidad","PIB","Calidad soporte social","Expectativa_de_vida","Libertad","Generosidad","Corrupción",
                 "Afecto Positivo","Afecto Negativo","Confianza en el gobierno","Calidad de la democracia","Calidad Servicios",
                 "de_escalera_pais_anio","Gini","gini_banco_mundial_promedio","continente","name")


#Datos para mapa
mapa <- inner_join(felicidad, paises, by = c("pais" = "name"))

mapa <- mapa %>%
    rename(region = pais)

mapa_mundo <- map_data("world")

mapa_felicidad <- left_join(mapa_mundo, mapa, by = "region")

colnames(mapa_felicidad) <- c("long",                        "lat",                         "group",                       "order",                      
                               "region",                      "subregion",                   "Ano",                        "Felicidad",              
                               "PIB",                     "soporte_social",              "Expectativa_de_vida",            "libertad",                   
                               "generosidad",                 "percepcion_corrupcion",       "afecto_positivo",             "afecto_negativo",            
                               "confianza",                   "calidad_democracia",          "calidad_entrega",             "de_escalera_pais_anio",      
                               "gini_banco_mundial",          "gini_banco_mundial_promedio", "nombre",                      "continente")

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
                 h2("Diagrama de dispersión", align = "center"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("varx",
                                     "Variable X",
                                     c("Felicidad","PIB","Calidad soporte social","Expectativa_de_vida","Libertad","Generosidad","Corrupción",
                                       "Afecto Positivo","Afecto Negativo","Confianza en el gobierno","Calidad de la democracia","Calidad Servicios",
                                       "de_escalera_pais_anio","Gini")),
                         selectInput("vary",
                                     "Variable Y",
                                     c("PIB","Felicidad","Calidad soporte social","Expectativa_de_vida","Libertad","Generosidad","Corrupción",
                                       "Afecto Positivo","Afecto Negativo","Confianza en el gobierno","Calidad de la democracia","Calidad Servicios",
                                       "de_escalera_pais_anio","Gini")),
                         selectInput("anio",
                                     "año",
                                     c(2018:2005)
                         ),
                     ),
                     mainPanel(plotOutput("biv", width = "100%")
                     )     
                 )
                 ),
#Panel Univariada        
        tabPanel("Univariada",
                 h2("Diagrama de caja", align = "center"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("var",
                                     "Variable de interes",
                                     c("Felicidad","PIB","Calidad soporte social","Expectativa_de_vida","Libertad","Generosidad","Corrupción",
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
        tabPanel("Tabla",
               sidebarLayout(
                    sidebarPanel(
                        selectInput('varcolor', 'Variable de interes', 
                                  c("Pais", "Ano", "continente")),
                    selectInput("digitos", "Elegir decimales", 
                             c(0, 1, 2))
                    ),
               mainPanel(DTOutput("tab1")),
               )
        ),
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
                                     c("Felicidad","soporte_social","Expectativa_de_vida","libertad","generosidad","percepcion_corrupcion",
                                       "confianza","calidad_democracia","gini_banco_mundial")
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
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
        print(gg1)
    }else if(input$anio == 2017){
        scat <- x %>% filter(Ano==2017) 
        gg1 <- scat %>%
            ggplot() +
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
        print(gg1)
    }else if(input$anio == 2016){ 
        scat <- x %>% filter(Ano==2016) 
        gg1 <- scat %>%
            ggplot() +
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
        print(gg1)
    }else if(input$anio == 2015){
        scat <- x %>% filter(Ano==2015) 
        gg1 <- scat %>%
            ggplot() +
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
        print(gg1)
    }else if(input$anio == 2014){
        scat <- x %>% filter(Ano==2014) 
        gg1 <- scat %>%
            ggplot() +
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
        print(gg1)
    }else if(input$anio == 2013){ 
        scat <- x %>% filter(Ano==2013) 
        gg1 <- scat %>%
            ggplot() +
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
        print(gg1)
    }else if(input$anio == 2012){ 
        scat <- x %>% filter(Ano==2012) 
        gg1 <- scat %>%
            ggplot() +
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
        print(gg1)
    }else if(input$anio == 2011){ 
        scat <- x %>% filter(Ano==2011) 
        gg1 <- scat %>%
            ggplot() +
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
        print(gg1)
    }else if(input$anio == 2010){ 
        scat <- x %>% filter(Ano==2010) 
        gg1 <- scat %>%
            ggplot() +
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
        print(gg1)
    }else if(input$anio == 2009){ 
        scat <- x %>% filter(Ano==2009) 
        gg1 <- scat %>%
            ggplot() +
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
        print(gg1)
    }else if(input$anio == 2008){ 
        scat <- x %>% filter(Ano==2008) 
        gg1 <- scat %>%
            ggplot() +
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
        print(gg1)
    }else if(input$anio == 2007){ 
        scat <- x %>% filter(Ano==2007) 
        gg1 <- scat %>%
            ggplot() +
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
        print(gg1)
    }else if(input$anio == 2006){ 
        scat <- x %>% filter(Ano==2006) 
        gg1 <- scat %>%
            ggplot() +
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
        print(gg1)
    }else if(input$anio == 2005){ 
        scat <- x %>% filter(Ano==2005) 
        gg1 <- scat %>%
            ggplot() +
            geom_point(data=scat,aes(x =.data[[input$varx]], y = .data[[input$vary]],colour=continente))+
            theme(aspect.ratio = 1)
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
    
    
#Tabla
    tabla <- reactive({
        x %>%
            group_by(.data[[input$varcolor]]) %>%
            summarise(mean_Felicidad = round(mean(Felicidad),3),
                      max_Felicidad = round(max(Felicidad),3),
                      min_Felicidad = round(min(Felicidad),3),
                      mean_Expectativa = round(mean(Expectativa_de_vida),3)) %>% 
            summarise(across(.cols = where(is.numeric), .fns = round, digits = as.integer(input$digitos)))
    })
    
    output$tab1 <- renderDT({
        tabla()
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
    output$biv <- renderPlot({ploteob()}, height = 600, width = 1000)
    
}

# Run the application 
shinyApp(ui = ui, server = server)




