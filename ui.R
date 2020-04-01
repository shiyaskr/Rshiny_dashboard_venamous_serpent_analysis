library(shiny)
library(leaflet)
library(shinydashboard)
library(shinyBS)
library(DT)
library(readr)
library(tidyr)  
library(ggplot2)
library(png)
library(plotly)
library(ggthemes)
library(shinycssloaders)
library(shinyalert)


dashboardPage(
    dashboardHeader(title ="Venomous Serpent Analysis",titleWidth = 300),
    dashboardSidebar(
        sidebarUserPanel(Sys.info()[["user"]],
                         subtitle = a(href = "", icon("circle", class = "text-success"), "Online")),
        
        sidebarMenu(
            
            id = "tabs",
            menuItem("Analysis", icon = icon("chart-bar","fa-1.5x", lib = "font-awesome"), tabName = "Analysis"),
            menuItem("Reports", icon = icon("dashboard","fa-1.5x", lib = "font-awesome"), tabName = "Report"
                     ),
            menuItem("Map", icon = icon("th"),
                     menuSubItem("Find Near by snakes", tabName = "subitem1"),
                     menuSubItem("Find Near by Hospitals", tabName ="subitem2")
            )
        )),
    dashboardBody(bsModal(id = 'startupModal', "Window",
                          
                          title="",size='Large',
                          HTML(paste0(
                              "<font size='6' color='#eb6434' ><center><b>Venomous Serpent Analysis</b><center></font>","<br>",
                              "<center><IMG SRC='https://media.giphy.com/media/2lbhL8dSGMh8I/giphy.gif',
                                                  style='width:200px;height:200px;'></center>",
                              "<br>",
                              "<p style='line-height:140%, margin-left: auto; margin-right: auto;padding-left: 40px;padding-right: 40px; text-align:justify'>
                                <font size='3' color='#346eeb'><b>
On average, 5.3 million people are bitten by the snake every year around the world. In the United States alone, 1.3 million snake bite deaths are reported annualy. 
This dashboard helps us to analyze the snake species which exist in each United state's region. 
The Geo location map feature will let us know the variety of snakes that resides at current location of the user.

In the event of any medical emergency the user may use the feature of locating nearest hospitals at the site, this tool will save many more lives in the United States
</b></font></p>" ))),
        tabItems(
       
        # First tab content
        tabItem(tabName = "Analysis",
                fluidRow(column(width = 12,
                                box(plotOutput("myImage",width = "70px", height = "300px")
                         ))),
                        
                fluidRow(column(width = 12,
                                box(plotOutput("myImage2",width = "100px",height = "550px")
                                )))),
        
        tabItem(tabName = "Report",
                fluidRow(title = "Number of Snake Species Per Region",
                    withSpinner(plotlyOutput("bbmap2",width = 1000,height = 850), type = getOption("spinner.type", default = 6))
                    )),
##First Tab          
        
        tabItem(tabName = "subitem1",
                fluidRow(uiOutput("popup"),
                    box(withSpinner(leafletOutput("bbmap", width =1100  ,height=700), type = getOption("spinner.type", default = 5))))),
##Second Tab  

        tabItem(tabName = "subitem2", uiOutput("popup1"),
                fluidRow(
                    box(withSpinner(leafletOutput("bbmap1", width =1100  ,height=700), type = getOption("spinner.type", default = 5)))))
        )
    ),skin = "red")
