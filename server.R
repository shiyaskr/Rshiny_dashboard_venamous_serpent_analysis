library(shiny)

library(dplyr)

library(leaflet)

library(DT)

library(readr)

library(tidyr)  

library(ggplot2)

library(png)
library(shinycssloaders)
library(shinyalert)

shinyServer(function(input, output, session) {
    
    toggleModal(session, "startupModal", toggle = "open")
    
    # toggleModal(session, "startupModal2", toggle = "open")
    # toggleModal(session, "startupModal3", toggle = "open")
    
    # Import Data and clean it
    regionwise_filepath <-  "./SourceData/Regionwise Snake Data.csv"
    hosp_filepath <- "./SourceData/HospitalData.csv"
    
    regionwise_snakes <- read_csv(regionwise_filepath)
    hospitals <- read_csv(hosp_filepath)

regionwise_snakes <- regionwise_snakes %>% 
        group_by(Region, Category) %>% 
        mutate(Number_of_Snake_Species = n())  
    
regionwise_snakes <- regionwise_snakes %>% 
        mutate(Snake_new = trimws(Snake),
               tooltip_snakes = paste0('<strong>Snake: </strong>',Snake,
                                       '<br><strong>Category:</strong> ', Category,
                                       '<br><strong>Region:</strong> ',Region))

color_pal <- colorFactor(pal = c("#34eb3a", "#470F0D"), domain = c( "Non Venomous", "Venomous"))



hospIcons <- iconList(
    hosp = makeIcon("./SourceData/hosp.png", iconWidth = 38, iconHeight = 61,
                    iconAnchorX = 22, iconAnchorY = 80)
)


snakeicons <- icons(
    iconUrl = ifelse(regionwise_snakes$Category == "Venomous",
                     "./SourceData/BSnake.png",
                     "./SourceData/GSnake.png"
    ),
    iconWidth = 38, iconHeight = 61,
    iconAnchorX = 22, iconAnchorY = 80) 
    
    hospitals <- hospitals %>% 
    mutate(tooltip = paste0('<strong>Name: </strong>',NAME,
                            '<br><strong>Address:</strong> ', ADDRESS,
                            '<br><strong>Telephone:</strong> ',TELEPHONE,
                            '<br><strong>Category:</strong>',OWNER))
    
    
    pal <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3"), domain = c("GOVERNMENT - DISTRICT/AUTHORITY", "GOVERNMENT - FEDERAL", "GOVERNMENT - STATE"))
    
    
    # create the leaflet map  
    output$bbmap <- renderLeaflet({
        leaflet(regionwise_snakes) %>% addTiles() %>% 
            setView(-96, 37.8, 7) %>% 
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
            addMarkers(regionwise_snakes$longitude,  regionwise_snakes$latitude,
                       popup = ~as.character(tooltip_snakes), label  = as.factor(regionwise_snakes$Snake), icon = snakeicons) %>% 
            
            addLegend(pal= color_pal, values=regionwise_snakes$Category,opacity=1, na.label = "Not Available") %>%
            addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%
            addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
            addLayersControl(baseGroups=c('OSM','Dark','Light')) %>%   
            
            addEasyButton(easyButton(
                icon = "fa-crosshairs", title = "Locate Me",
                onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
            addEasyButton(easyButton(
                icon = "fa-globe", title = "Zoom to Level 8",
                onClick = JS("function(btn, map){ map.setZoom(8);}")))
    })
    
    
    output$bbmap1 <- renderLeaflet({
        leaflet(data = hospitals) %>% addTiles() %>%
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
            setView(-96, 37.8, 9) %>% 
            addMarkers(~LONGITUDE, ~LATITUDE, popup = ~as.character(tooltip), label = ~as.character(NAME), icon = hospIcons) %>% 
            #addLegend(pal=pal, values=hospitals$OWNER,opacity=1, na.label = "Not Available") %>%
            addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%
            addProviderTiles(providers$CartoDB.Positron, group="Light") %>%
            addLayersControl(baseGroups=c('OSM','Dark','Light')) %>% 
            
            addEasyButton(easyButton(
                icon = "fa-crosshairs", title = "Locate Me",
                onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
            addEasyButton(easyButton(
                icon = "fa-globe", title = "Zoom to Level 8",
                onClick = JS("function(btn, map){ map.setZoom(8);}")))
    })
    
    output$bbmap2 <- renderPlotly({
        plot <- ggplot(regionwise_snakes, aes(Region, Number_of_Snake_Species, fill= Category)) %>% 
            plot + geom_bar(stat = "identity", position = 'dodge', colour="black")  +
            coord_flip() + theme_fivethirtyeight() + scale_fill_fivethirtyeight() +
            guides(fill=guide_legend(title=NULL)) 
        plot + labs(title = "Number of Snake Species Per Region",
                    subtitle = "Plot of Venomous and Non Venomous Serpents")
        plot <- ggplotly(plot)
    })
    
    output$myImage <- renderImage({
        
        filename <- normalizePath(file.path('./SourceData/report-min.png'))
        
        # Return a list containing the filename
        list(src = filename)
        
    },deleteFile = FALSE)
    
    output$myImage2 <- renderImage({
        
        filename <- normalizePath(file.path('./SourceData/report2.png'))
        
        # Return a list containing the filename
        list(src = filename)
        
    },deleteFile = FALSE)
    
    
    output$popup <- renderUI ({
        showModal(modalDialog(title = "",
            
            HTML(paste0(
                "<font size='6' color='#eb6434' ><center><b>Find Near by Snakes</b><center></font>","<br>",
                
                "<p style='line-height:140%, margin-left: auto; margin-right: auto;padding-left: 40px;padding-right: 40px; text-align:justify'>
                                <font size='3' color='#346eeb'><b>
                                The dashboard page helps us understand the various types of snakes that live in the North American city based on the GPS position of the users and 
                                allows us to differentiate between the common bases and those that are venomous
                                or nonvenomous and by clicking on the snake icon on the map, the snake information will be displayed.

                </b></font></p>" )),
            easyClose = TRUE,
            footer = modalButton("Close")
        ))
        
    })
    
output$popup1 <- renderUI ({
        showModal(modalDialog(title = "",
            
            HTML(paste0(
                "<font size='6' color='#eb6434' ><center><b>Find Near by Hospitals</b><center></font>","<br>",
                                
                "<p style='line-height:140%, margin-left: auto; margin-right: auto;padding-left: 40px;padding-right: 40px; text-align:justify'>
                                <font size='3' color='#346eeb'><b>
                                The dashboard page helps us to locate the nearby hospital in the event medical emergency based on the users GPS location , 
                                by clicking on the hospital icon on the map the hospital details will be displayed.
                </b></font></p>" )),
            easyClose = TRUE,
            footer = modalButton("Close")
        ))
        
    })



observeEvent(input$sidebarMenu,{
    req(input$sidebarMenu == "Map")
    # Show a modal when the button is pressed
    shinyalert(HTML(paste0(
        "<font size='6' color='#00cc00' ><center><b>Find Near by Snakes</b><center></font>","<br>",
        "<center><IMG SRC='https://media.giphy.com/media/2lbhL8dSGMh8I/giphy.gif',
                                                  style='width:200px;height:200px;'></center>",
        "<br>",
        "<p style='line-height:140%, margin-left: auto; margin-right: auto;padding-left: 40px;padding-right: 40px; text-align:justify'>
                                <font size='3' color='#66ff33'><b>
                                The dashboard page helps us understand the various types of snakes that live in the North American city based on the GPS position of the users and 
                                allows us to differentiate between the common bases and those that are venomous
                                or nonvenomous and by clicking on the snake icon on the map, the snake information will be displayed.
                </b></font></p>" )))

    })

   
    
})