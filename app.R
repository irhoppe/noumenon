
library(tidyverse)
library(sf)
library(leaflet)
library(shiny)

totitle <- function(x){
    x <- stringr::str_to_title(x)
    gsub("Usa", "USA", x)
}

mapkey <- read.csv("data/key.csv", colClasses="character", na.strings="") %>% 
    mutate(SRC=case_when(!is.na(XC) ~ sprintf("<audio src=\"%s - %s.mp3\" type=\"audio/mp3\" autoplay controls></audio>", XC, NOUMENON), 
                         is.na(XC) ~ NA_character_) )

eco <- read_sf("data/na_cec_eco_l2") %>% st_transform(4326) %>% 
    mutate( NOUMENON=mapkey$NOUMENON[match(NA_L2CODE,mapkey$NA_L2CODE)], 
            AUDIO=mapkey$SRC[match(NA_L2CODE,mapkey$NA_L2CODE)], 
            XC=mapkey$XC[match(NA_L2CODE,mapkey$NA_L2CODE)], 
            RECORDIST=mapkey$RECORDIST[match(NA_L2CODE,mapkey$NA_L2CODE)], 
            LABEL=case_when(
                !is.na(AUDIO) ~ sprintf(
                    "<b>%s</b><br/>
                    %s<br/>
                    %s<br/>
                    <a href=https://xeno-canto.org/%s>%s</a>
                    <div style=\"float:right\">%s</div>", totitle(NA_L2NAME), NOUMENON, AUDIO, gsub("^XC","",XC), XC, RECORDIST
                ), 
                TRUE ~ sprintf("<b>%s</b>", totitle(NA_L2NAME))
            ))

epsg2163 <- leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "EPSG:2163",
    proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
    resolutions = 2^(16:7)
)

er_pal <- colorFactor(mapkey$COLOR, domain=mapkey$NA_L2CODE)

ui <- fluidPage(
    
    tags$head(
        tags$link(rel="stylesheet", type="text/css", href="style.css")
    ), 
    
    div(class="outer", 
        leafletOutput("map", width="100%", height="100%")
    ), 
    
    actionButton("info", label="", icon=icon("info"))
    
)

server <- function(input, output) {

    output$map <- renderLeaflet({
        leaflet(eco, options=leafletOptions(crs=epsg2163)) %>% 
            setView(lng=-101.2996, lat=47.1164, zoom=3) %>% 
            addPolygons(fillColor=~er_pal(NA_L2CODE), fillOpacity=1, color="#272727", weight=1, 
                        popup=~LABEL) %>% 
            addGraticule(interval=10, style=list(color="#a1def7", weight=0.5))
    })
    
    observeEvent( input$info, {
        showModal(modalDialog(
            title="N O U M E N O N", 
            HTML("An audiovisual guide to the avian soul of North America<br/>"), 
            HTML("Spatial data: <a href=https://www.epa.gov/eco-research/ecoregions-north-america>U. S. Environmental Protection Agency</a><br/>"), 
            HTML("Audio data: <a href=https://www.xeno-canto.org>xeno-canto</a>")
        ))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
