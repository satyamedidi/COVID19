library(shinydashboard, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(plotly, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(rmarkdown, quietly = TRUE)
library(timevis, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(DT, quietly = TRUE)
library(timevis, quietly = TRUE)
library(rpivotTable, quietly = TRUE)
library(shinyjs, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(networkD3, quietly = TRUE)
library(shinyWidgets, quietly = TRUE)
library(highcharter, quietly = TRUE)
library(viridisLite, quietly = TRUE)
library(stringr, quietly = TRUE)
library(quantmod, quietly = TRUE)
library(xts, quietly = TRUE)
library(shiny, quietly = TRUE)
library(shinydashboardPlus, quietly = TRUE)
library(shinyalert, quietly = TRUE)
library(fst, quietly = TRUE)
library(leaflet, quietly = TRUE)
library(shinybusy, quietly = TRUE)
library(shinycssloaders, quietly = TRUE)
library(shinyjs, quietly = TRUE)
library(googleway, quietly = TRUE)
library(shinymanager, quietly = TRUE)
library(shinyFeedback, quietly = TRUE)
library(shinythemes, quietly = TRUE)
library(fresh, quietly = TRUE)
library(leaflet, quietly = TRUE)

cv_cases <- read.csv("input_data/coronavirus.csv")

cv_min_date  <- as.Date(min(as.Date(cv_cases$date)),"%Y-%m-%d")

current_date <- as.Date(max(as.Date(cv_cases$date)),"%Y-%m-%d")

cv_max_date_clean = format(as.Date(current_date),"%d %B %Y")


fluidPage(
  # use_googlefont("Baloo Thambi 2"),
  # use_theme(create_theme(
  #   theme = "default",
  #   bs_vars_font(family_sans_serif = "'Baloo Thambi 2', cursive")
  # )),
  # 
  
  
  
  navbarPage(
    theme = shinytheme("slate"),
    collapsible = TRUE,
    "COVID-19 UPDATE",
    id = "nav",
    
    tabPanel(
      h5("COVID-19 WORLD COUNT"),
      div(class="outer",
          tags$head(includeCSS("styles.css")),
          # tags$head(
          #  tags$style(HTML(".leaflet-container { background: #272B28; }"))
          #   ),
          
          
          leafletOutput('ALLCOUNTRYVIEW',height = '100%',width = '100%'),
          p(),
          actionButton("recalc", "New points"),
          
          
          
          tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 30pt !important; }")),
          
    
          
          bootstrapPage(
          absolutePanel(id = "table", class = "panel panel-default",
                        top = 70, left = 1100, width = 420, fixed=TRUE,
                        draggable = TRUE, height = "auto",
                        
                        HTML('<button data-toggle="collapse" data-target="#demo"> <h4> Click for Country Details:</h4> </button>'),
                        
                        tags$div(id = 'demo',  class="collapse",
                                 dataTableOutput("Counts",width = '100%'))
                        
                        )
                        
                        
                        
                        
                        # tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                        #           background-color: #272B30 !important;
                        #           } "))),
                        
                        
                        
                        # span(h6(textOutput("reactive_case_count_China"), align = "right"), style="color:#cc4c02"),
                        
          ),     fluidRow(absolutePanel(id = "controls", class = "panel panel-default",
                                top = 80, left = 20, width = 400, fixed=TRUE,
                                draggable = TRUE, height = "auto",
                                
                                column(12,
                                       fluidRow(sliderInput("plot_date",
                                                            label = h3("Timeline: Click on the Play"),
                                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                                            max = as.Date(current_date,"%Y-%m-%d"),
                                                            value = as.Date(current_date,"%Y-%m-%d"),
                                                            timeFormat = "%d %b", 
                                                            animate=animationOptions(interval = 2000, loop = FALSE)))),
                                
                                fluidRow(
                                  span(h3(textOutput("clean_date_reactive"), align = "center"),style="color:#FFFFFF"),
                                  span(h2(textOutput("totalcases"), align = "center"),style="color:#0099CC"),
                                  span(h2(textOutput("Activecases"), align = "center"),style="color:#ffaa1d"),
                                  span(h2(textOutput("recovered"), align = "center"), style="color:#00ab66"),
                                  span(h2(textOutput("deaths"), align = "center"),style="color:#d92121")),
                                
                                fluidRow(highchartOutput("Trend_Overall",width = "auto",height = "300px"))
                                
                                # tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                #           background-color: #272B30 !important;
                                #           } "))),
                                
                                
                                
                                # span(h6(textOutput("reactive_case_count_China"), align = "right"), style="color:#cc4c02"),
                                
          )),
          
          
          
          
          absolutePanel(
            id = "logo",
            class = "card",
            bottom = 20,
            left = 40,
            width = 100,
            fixed = TRUE,
            draggable = FALSE,
            height = "auto",
            
            tags$a(href = 'https://www.linkedin.com/in/satyasekhar/', tags$img(
              src = 'Medidi_4.png',
              height = '140',
              width = '140'
            ))
          )
          
      )),
    
    tabPanel(h5("US: CASES & HOSPITALS"),  div(class="outer",
                                            tags$head(includeCSS("styles.css")),
                                            # tags$head(
                                            #  tags$style(HTML(".leaflet-container { background: #272B28; }"))
                                            #   ),
                                           
                                            leafletOutput('USVIEW',height = '100%',width = '100%')),
             
             
             
             
             absolutePanel(id = "controls", class = "panel panel-default", 
                           fixed = TRUE,
                           draggable = TRUE, top = "10%", left = "auto", right = 20, bottom = "auto",
                           width = 500, height = "500", cursor = "move",
                           br(),
                           h4("Zoom for Details: *Green Dots are the Hospitals"),
                           
                           
                   
                             div(style = "font-size:25px;",
                           selectInput("State", "Select State:",
                                      choices = "",selected = "New York")),
                           
                           
                           
                           fluidRow(
                             span(h2(textOutput("TotalC"), align = "center"),style="color:#ffaa1d"),
                             span(h1(textOutput("TotalD"), align = "center"),style="color:#d92121"),
                             span(h1(textOutput("TotalCareCenters"), align = "center"),style="color:#ffffff"),
                             span(h1(textOutput("TotalBeds"), align = "center"), style="color:#CC8899"),
                             span(h1(textOutput("TotalBeds60"), align = "center"),style="color:#83F52C")),
                           
                           # dataTableOutput("Report",width = '100%')
                           
                          
                       
                            highchartOutput("bar",width = "auto",height = "400px")
                           ),
                                            
                                            
                                            
                                            
                                            absolutePanel(
                                              id = "logo",
                                              class = "card",
                                              bottom = 20,
                                              left = 40,
                                              width = 100,
                                              fixed = TRUE,
                                              draggable = FALSE,
                                              height = "auto",
                                              
                                              tags$a(href = 'https://www.linkedin.com/in/satyasekhar/', tags$img(
                                                src = 'Medidi_4.png',
                                                height = '140',
                                                width = '140'
                                              ))
                                            )
                                            
    ),
    
    tabPanel(h5("US: SIMULATE REQUIRED HOSPITAL RESOURCES"),
             "Coming Soon"),
    
    tabPanel(h5("DATA SOURCES"),
             
             
            h3("https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data"),br(),
             h3("https://hifld-geoplatform.opendata.arcgis.com/search?groupIds=2900322cc0b14948a74dca886b7d7cfc")),
    
    tabPanel(h5("TEAM"),
            
               absolutePanel(
                 id = "Satya",
                 class = "card",
                 top = 100,
                 bottom = 20,
                 left = 90,
                 width = 100,
                 fixed = TRUE,
                 draggable = FALSE,
                 height = "auto",
                 
                 tags$a(href = 'https://www.linkedin.com/in/satyasekhar/', 
                        tags$img( src = 'SM.jpg',
                   height = '400',
                   width = '300'
                 ))),
            
        
            
                br(),
             
             absolutePanel(
               id = "Satya1",
               class = "panel panel-default",
               top = 530,
               bottom = 20,
               left = 40,
               width = 400,
               fixed = TRUE,
               draggable = FALSE,
               height = "300",
               
               
               h2("Satya Medidi",align = "center"),
             br(),
               h3("Data Scientist",align = "center"),
             br(),
               h3("App Developer & Designer",align = "center"),
             h3("email: satyamedidi@outlook.com",align = "center")
             ),
             
             absolutePanel(
               id = "Radha",
               class = "panel panel-default",
               top = 100,
               bottom = 20,
               left = 690,
               width = 100,
               fixed = TRUE,
               draggable = FALSE,
               height = "300",
               
               tags$a(href = 'https://www.linkedin.com/in/kashyap-m-393797153', tags$img(
                 src = 'RK.jpeg',
                 height = '400',
                 width = '300'
               ))),
             
             
             
             br(),
             
             absolutePanel(
               id = "Radha1",
               class = "panel panel-default",
               top = 530,
               bottom = 20,
               left = 650,
               width = 400,
               fixed = TRUE,
               draggable = FALSE,
               height = "300",
               
               
               h2("Radha Krishna",align = "center"),
               br(),
               h3("Technology Architect",align = "center"),
               br(),
               h3("Azure Support",align = "center"),
               h3("email: manda.kashyap@gmail.com",align = "center")
             ),
             
  
  
)))