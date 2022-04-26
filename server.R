#calling libraries
library(shiny,quietly = TRUE)
library(revgeo,quietly = TRUE)
library(ggmap,quietly = TRUE)
library(googleway,quietly = TRUE)
library(htmltools,quietly = TRUE)
library(htmlwidgets,quietly = TRUE)
library(viridis,quietly = TRUE)
library(naniar,quietly = TRUE)
library(leaflet.extras,quietly = TRUE)
library(devtools,quietly = TRUE)
library(dplyr,quietly = TRUE)
library(shinyFeedback,quietly = TRUE)
library(leaflet.extras)
library(leaflet.minicharts,quietly = TRUE)
library(htmltools,quietly = TRUE)
library(lubridate,quietly = TRUE)
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")

library(highcharter,quietly = TRUE)

# update data with automated script

source("jhu_data_full.R")
source("US_Data.R")


shinyServer(function(input,output,session){
  
  

    # import data
    cv_cases <- read.csv("input_data/coronavirus.csv")
    cv_cases <- cv_cases %>% mutate(active_cases = (cv_cases$cases - cv_cases$recovered - cv_cases$deaths))
    
    
    
    us_cases <- read.csv("input_data/us_total.csv")
    
    
    us_cases <-  us_cases %>% mutate(Date_New = mdy(Date))
    
    us_today <- us_cases %>% filter(Date_New == max(Date_New))
    
    
    #Care_Centers <- read.csv("input_data/Hospitals_Final.csv")
    
    Care_Centers  <- readRDS("Care_Centers.rds")
    
    Care_Centers <- Care_Centers %>% select(NAME,ADDRESS,CITY,STATE,ZIP,COUNTY,LATITUDE,LONGITUDE,BEDS,Source_Type)
    
    Care_Centers <- Care_Centers %>% filter(BEDS != '-999' & BEDS != 'NA')
    
    #  Care_Centers <- write_rds(Care_Centers,"Care_Centers.rds")
    
    #worldcountry = geojson_read("input_data/countries.geo.json", what = "sp")
    #country_geoms = read.csv("input_data/country_geoms.csv")
    
    # extract time stamp from cv_cases
    update = tail(cv_cases$last_update,1) 
    
    # extract dates from cv data
    if (any(grepl("/", cv_cases$date))) { 
      cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
    } else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") } 
    
    cv_cases$date <- as.Date(cv_cases$date)
    
    #cv_min_date <- as.Date(min(cv_cases$date),"%Y-%m-%d")
    
    
    
    #as.Date(max(cv_cases$date),"%Y-%m-%d")
    
    #cv_max_date_clean = format(as.Date(current_date),"%d %B %Y")

  
  

  reactive_db = reactive({
    cv_cases %>% filter(date == input$plot_date)
  })
  
  
  
  
  observe({

    output$totalcases <- renderText({
      paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " Total Cases")
    })
    
    
    ac <- reactive_db()$cases - reactive_db()$recovered - reactive_db()$deaths
    
    output$Activecases <- renderText({
      paste0(prettyNum(sum(ac,na.rm = TRUE), big.mark=","), " Active Cases")
    })
    
    
    
    output$recovered <- renderText({
      
      
      
      paste0(prettyNum(sum(reactive_db()$recovered,na.rm=TRUE), big.mark=","), " Recovered")
    })
    
    output$deaths <- renderText({
      paste0(prettyNum(sum(reactive_db()$deaths,na.rm=TRUE), big.mark=","), " Deaths")
    })
    
    
  
    
    
    output$clean_date_reactive <- renderText({
      format(as.POSIXct(input$plot_date),"%d %B %Y")
    })
    
    
   
    
 
    
    observeEvent(input$plot_date, {
      
      
      output$ALLCOUNTRYVIEW <- renderLeaflet({ 
       
        
        dark <- "https://api.mapbox.com/styles/v1/mapbox/dark-v10/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1Ijoic2F0eWFtZWRpZGkiLCJhIjoiY2pkdW1rNzQwMWVtczJ3bGxnd3VqZ2JkbSJ9.oBm8An8Fb3dpCGoWdsrcjQ"
        
        mapattrs <- '© <a href="https://www.mapbox.com/map-feedback/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright" target="_blank">OpenStreetMap</a>'
        
        
      
   basemap <- leaflet(reactive_db()) %>% addTiles(urlTemplate = dark, attribution = mapattrs)  %>% addControlGPS( options = gpsOptions(position = "topleft", activate = TRUE,autoCenter = TRUE, maxZoom = 10, setView = TRUE)) %>% 
     fitBounds(-90, 40, -70, 43) %>% clearBounds()
    
      
      
   
     
   
   # 
      #       %>%
      #         addCircleMarkers(lng= ~test$long, lat= ~test$lat,
      #                          popup= paste("<b>", test$displayName,"</b>", "<br>","<b> Total Confirmed: </b>",  test$totalConfirmed,
      #                                       "<br>","<b> Total Deaths: </b>",test$totalDeaths,"<br>","<b> Total Recovered: </b>",test$totalRecovered)) %>% addControlGPS( options = gpsOptions(position = "topleft", activate = TRUE,
      # 
      # 
      
      
      #                                                                                                                                                                                         
      #                                                                                                                                                                                         autoCenter = TRUE, maxZoom = 10, setView = TRUE)) 
 
   ac <- reactive_db()$cases - reactive_db()$recovered - reactive_db()$deaths
   
      
 
   
      colors <- c("#ffaa1d","#00ab66","#d92121")
      
      basemap %>%
        addMinicharts(
          
          reactive_db()$longitude, 
          reactive_db()$latitude,
          type = "pie",
          chartdata = reactive_db()[, c("active_cases","recovered","deaths")],
          colorPalette = colors,
          legendPosition = "bottomright",
          

          
          popup = popupArgs(
            
            html = paste0(
              "<div>",
              "<h3>",
              reactive_db()$country,
              "</h3>",
              "<h4>",
              "<br>",
              "Total Cases: ",
              prettyNum(reactive_db()$cases,big.mark = ","),
              "</h4>",
              "<h4>",
              "Active Cases: ",
              prettyNum(ac,big.mark = ","),
              "</h4>",
              "<h4>",
              "Recovered Cases: ",
              prettyNum(reactive_db()$recovered,big.mark = ","),
              "</h4>",
              "<h4>",
              "Total Deaths: ",
              prettyNum(reactive_db()$deaths,big.mark = ","),
              "</h4>",
              "</div>"
            )),
          
          width = 80 * sqrt(reactive_db()$cases) / sqrt(max(reactive_db()$cases)), transitionTime = 0
        ) 
      
      
      }) 
      
      
      
      
      
      
      # barda <- points() %>% select(totalConfirmed,totalRecovered,totalDeaths)
      # 
      # 
      # 
      # basemap %>%
      #   addMinicharts(
      #     points()$long, points()$lat,
      #     type = "bar",
      #     chartdata = barda,
      #     colorPalette = colors,
      #     width = 45, height = 50
      #   )
      
      
      
      
    })
    
    
    
    
  })
  
  
  
  
  observe({
    

    
    select_Data <- reactive_db() %>% select(country,cases,recovered,deaths) %>% arrange( desc(cases))
    #     
    #      


    
    #
    
    names(select_Data)[names(select_Data)=="country"] <- "COUNTRIES"
    names(select_Data)[names(select_Data)=="cases"] <- "TOTAL CASES"
    names(select_Data)[names(select_Data)=="recovered"] <- "TOTAL RECOVERED"
    names(select_Data)[names(select_Data)=="deaths"] <- "TOTAL DEATHS"
    
    
    
    
    output$Counts =  DT::renderDataTable(




      DT:::datatable(select_Data,
                     options = list(dom = 'prlti',
                                    scrolly = TRUE,
                                    initComplete =JS(
                                      "function(settings, json) {",
                                      "$(this.api().table().header()).css({'font-size': '10px', 'background-color': '#272B30', 'color': '#fff'});",
                                      "$(this.api().table().body()).css({'font-size': '10px', 'background-color': '#272B30', 'color': '#fff'});",
                                      "$(this.api().table().header()).css({'background-color': '#272B30', 'color': '#fff'});",
                                      "}"),

                                    autoWidth = TRUE,searchable = FALSE,scrollY = "600px",pageLength = 350),
                     rownames= FALSE  ) %>%  formatStyle(columns=colnames(select_Data),color='#CFCFCF',background = '#272B30',target = 'row'))


    
    
    
    
    
    
  })
  
  

  
  output$Trend_Overall <- renderHighchart({
    


    TrendClaims <- cv_cases %>% select(date,cases,active_cases,recovered,deaths) %>%
      group_by(date) %>%
      summarise_each(funs(sum)) %>%
      data.frame()
    

    cols <- c("#ffaa1d","#00ab66","#d92121")

    
    hc <- highchart() %>% 
      hc_xAxis(categories = TrendClaims$date) %>% 
      hc_add_series(name = "Active Cases", data = TrendClaims$active_cases,type = "area") %>% 
      hc_add_series(name = "Recovered", data = TrendClaims$recovered,type = "area") %>% 
      hc_add_series(name = "Deaths",data = TrendClaims$deaths, type = "area") %>%   hc_colors(cols)
    
    
  hc
    
    
    

  })
    
    
 
  #%>%  formatStyle(columns = "displayName",backgroundColor = "#272B30")
  
  
  
  
  #%>% DT::formatStyle(columns = "displayName", backgroundColor = "#272B30")
  
  
  
  
  
  
  # output$ALLCOUNTRYVIEW <- renderLeaflet({
  # 
  # 
  #   leaflet(test) %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron)  %>%
  #     addAwesomeMarkers(lng= ~test$long, lat= ~test$lat,
  #                       popup= paste("<b>", test$displayName,"</b>", "<br>","<b> Total Confirmed: </b>",  test$totalConfirmed,
  #                                    "<br>","<b> Total Deaths: </b>",test$totalDeaths,"<br>","<b> Total Recovered: </b>",test$totalRecovered))
  # 
  # 
  # 
  # 
  # })
  #     
  # 
  #     
  


 
  
  
  #
  #
  #
  # # hospital file
  #
  #
  #
  #   # us_cases <- us_cases %>% filter(Date_New == max(Date_New) )
  #   #
 # HOSPITALS <- Care_Centers %>% filter(BEDS != 'NA' & BEDS != '-999')
  #   #
  #   # names(us_cases)[names(us_cases)=="Province_State"] <- "STATE"
  #   # names(us_cases)[names(us_cases)=="Admin2"] <- "COUNTY"
  #
  #   #
  #   # Tpotal_US <- merge(us_cases,BEDS_County, by ='COUNTY')
  #   #
  #   #
  #   # Total_US$COUNTY <- up
  #   #

  
  # BEDS_State <- HOSPITALS %>% select(STATE,BEDS) %>% group_by(STATE) %>% summarise(count = sum(BEDS))
  #   #
 # BEDS_County <- HOSPITALS %>% select(COUNTY,BEDS) %>% group_by(COUNTY) %>% summarise(count = sum(BEDS))
  #   #
  #   #
  #   #
  #
  #
  #
  
  reactive_us <- reactive({

    us_today

  })
  
  
  # reactive_care <- reactive({
  #   
  #   Care_Centers
  #   
  # })
  
  
  updateSelectInput(session,"State",choices = unique(us_cases$Province_State),selected = "Texas")
  
  
  observeEvent(input$State,{

   

    light <- "https://api.mapbox.com/styles/v1/mapbox/dark-v10/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1Ijoic2F0eWFtZWRpZGkiLCJhIjoiY2pkdW1rNzQwMWVtczJ3bGxnd3VqZ2JkbSJ9.oBm8An8Fb3dpCGoWdsrcjQ"

    mapattrs <- '© <a href="https://www.mapbox.com/map-feedback/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright" target="_blank">OpenStreetMap</a>'


    #

    US_State <- reactive_us() %>% filter(Province_State == input$State)
    
    US_Care <- Care_Centers %>% filter(STATE == input$State)
    
  
   # 

    names(US_State)[names(US_State)=="Long_"] <- "longitude"
    names(US_State)[names(US_State)=="Lat"] <- "latitude"
    
    names(US_State)[names(US_State)=="Province_State"] <- "STATE"

    # leafIcons <- icons(
    #   iconUrl =  "https://imgur.com/lk476Yu.png",
    # 
    #   iconWidth = 54, iconHeight = 54,
    #   iconAnchorX = 22, iconAnchorY = 94,
    #   shadowWidth = 50, shadowHeight = 64,
    #   shadowAnchorX = 4, shadowAnchorY = 62
    # )


    output$USVIEW <- renderLeaflet({
      
      
      
      # 
      # library(rgdal)
      # library(RColorBrewer)
      # library(leaflet)
      # 
      # 
      # 
      # bins <- c(0, 20, 30, 40, 50, 60, 70, 80, 90, Inf)
      # pal <- colorBin("YlOrRd", domain = US_State$Cases, bins = bins)
      # # 
      # labels <- sprintf(
      #   "<strong>%s</strong><br/>%g Cases",
      #   US_State$Admin2, US_State$Cases,
      #   "<strong>%s</strong><br/>%g Cases",
      #   US_State$Admin2, US_State$deaths
      # ) %>% lapply(htmltools::HTML)
      # # 
      # 
      # 
      # 
      # 
       states <- geojsonio::geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")
       
       
      # 
      # countyus <- geojsonio::geojson_read("https://raw.githubusercontent.com/python-visualization/folium/master/tests/us-counties.json",what = "sp")
      # 
      # 
      # basemap <- leaflet(countyus) %>% addTiles(urlTemplate = light, attribution = mapattrs)     
      # 
      # 
      # 
      # basemap %>% 
   

         #%>% setView(mean(US_State$longitude),mean(US_State$latitude), 7)  %>%  addFullscreenControl() 
      

      
      basemap <- leaflet(states[states$name == input$State,]) %>% addTiles(urlTemplate = light, attribution = mapattrs) %>%
        setView(mean(US_State$longitude),mean(US_State$latitude), 7.2) %>% 
        addPolygons(
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "15px",
                         direction = "auto"),smoothFactor = 0.2, fillOpacity = 0,color = 'white'
                       
                       
                       
                       
                       ) %>% addFullscreenControl() 
      
  
      #                                                                                                                                     icon = leafIcons)
      #    
      
      
      # data(unemployment)
      # 
      # hcmap("countries/us/us-all-all", data = unemployment,
      #       name = "Unemployment", value = "value", joinBy = c("hc-key", "code"),
      #       borderColor = "transparent") %>%
      #   hc_colorAxis(dataClasses = color_classes(c(seq(0, 10, by = 2), 50))) %>% 
      #   hc_legend(layout = "vertical", align = "right",
      #             floating = TRUE, valueDecimals = 0, valueSuffix = "%")
      
      
     
      
      
      library(viridisLite)
      
      
      cols <- viridis(81)
    
      
      pal <- colorFactor(
        palette = cols,
        domain = US_Care$Source_Type
      )



      # basemap <- leaflet(states) %>% addTiles(urlTemplate = light, attribution = mapattrs) %>% addpo


      colors <- c("#ffaa1d","#d92121")


      basemap %>%  addMinicharts( US_State$longitude,
                                  US_State$latitude,
                                  type = "pie",
                                  chartdata = US_State[, c("Cases","deaths")],
                                  colorPalette = colors,
                                  legendPosition = "bottomright",
                                  popup = popupArgs(

                                    html = paste0(
                                      "<div>",
                                      "<h3>",
                                      "County: ",
                                      US_State$Admin2,
                                      "</h3>",
                                      "<h5>",
                                      "<br>",
                                      "Total Cases: ",
                                      prettyNum(US_State$Cases,big.mark = ","),
                                      "</h5>",
                                      "<h5>",
                                      "Total Deaths: ",
                                      prettyNum(US_State$deaths,big.mark = ","),
                                      "</h5>",
                                      "</div>"
                                    )),

                                  width = 80 * sqrt(US_State$Cases) / sqrt(max(US_State$Cases)), transitionTime = 0 ) %>%  addCircles(lng = US_Care$LONGITUDE, lat = US_Care$LATITUDE, weight = 5, stroke = FALSE, fillOpacity = 1,radius = 70,color = "#00FF7F",
            popup= paste("<b>",US_Care$NAME ,"</b>", "<br>","<b> Total Beds: </b>",US_Care$BEDS,
                         "<br>","<b> Address: </b>",US_Care$ADDRESS,"<br>","<b> City: </b>",US_Care$CITY,"<br>",
                         "<b> Type of Care: </b>",US_Care$Source_Type)) 
      
      
    })
      

      State_Report <- US_Care %>% select(STATE,BEDS) %>% group_by(STATE) %>% summarise(count = sum(BEDS,na.rm = TRUE))
     
      names(State_Report)[names(State_Report) == 'count'] <- 'Total Beds'
      
      State_Hospitals <- US_Care %>% select(STATE,NAME)  %>%  group_by(STATE) %>% tally()
      
      names(State_Hospitals)[names(State_Hospitals) == 'n'] <- 'Total Care Centers'
      
      STATE_CR <- merge(State_Hospitals,State_Report, by='STATE')
      
      
      names(us_today)[names(us_today) == 'Province_State'] <- 'STATE'
      
      
   STATE_CO  <-  US_State %>% select(STATE,Cases,deaths) %>% group_by(STATE) %>% summarise(sum = sum(Cases,na.rm = TRUE))
   
   STATE_CO  <-  US_State %>% select(STATE,Cases,deaths) %>% group_by(STATE) %>% summarise('Total Cases' = sum(Cases,na.rm = TRUE))
   
   STATE_COD  <-  US_State %>% select(STATE,Cases,deaths) %>% group_by(STATE) %>% summarise('Total Deaths' = sum(deaths,na.rm = TRUE))
      
   STATE_COCD <- merge(STATE_COD,STATE_CO, by='STATE')
   
   STATE_REPORT <- merge(STATE_COCD,STATE_CR, by='STATE')
   
   STATE_REPORT <- STATE_REPORT %>% mutate('BEDS_CAPACITY@60' = round(STATE_REPORT$`Total Beds` * 0.6,0))
   
   
   # output$Report =  DT::renderDataTable(
   #   DT:::datatable(STATE_REPORT,
   #                  options = list(dom = 'prlti',
   #                                 scrolly = TRUE,
   #                                 initComplete =JS(
   #                                   "function(settings, json) {",
   #                                   "$(this.api().table().header()).css({'font-size': '20px', 'background-color': '#272B30', 'color': '#fff'});",
   #                                   "$(this.api().table().body()).css({'font-size': '20px', 'background-color': '#272B30', 'color': '#fff'});",
   #                                   "$(this.api().table().header()).css({'background-color': '#272B30', 'color': '#fff'});",
   #                                   "}"),
   #                                 
   #                                 autoWidth = TRUE,searchable = FALSE,scrollY = "600px",pageLength = 350),
   #                  rownames= FALSE  ) %>%  formatStyle(columns=colnames(STATE_REPORT),color='#CFCFCF',background = '#272B30',target = 'row'))
   # 
   # 
   
   
   
   
   
   
    
    
   
   
   STATE_R <- melt(STATE_REPORT, id="STATE")
   
  
   
   
   colors <- c("#D3D3D3")


#    output$bar <- renderHighchart({
#      
#      
# 
# 
# 
# highchart() %>%
#   hc_title(text = "State Data:",
#            style = list(fontSize = "55px")) %>%
#   hc_chart(type = "bar") %>%
#   hc_xAxis(categories = STATE_R$variable) %>%
#   hc_add_series(STATE_R$value, name = " ", showInLegend = FALSE) %>% hc_colors(colors)
# 
# 
# 
#    })
   
   
   

     
     output$TotalC <- renderText({
       paste0(prettyNum(sum(STATE_REPORT$`Total Cases`,na.rm = TRUE), big.mark=","), " Total Cases")
     })
     
     

     
     output$TotalD <- renderText({
       paste0(prettyNum(sum(STATE_REPORT$`Total Deaths`,na.rm = TRUE), big.mark=","), " Total Deaths")
     })
     
     
     
     output$TotalCareCenters <- renderText({
       
       
       
       paste0(prettyNum(sum(STATE_REPORT$`Total Care Centers`,na.rm = TRUE), big.mark=","), " Total Care Centers")
     })
     
     
     
     output$TotalBeds <- renderText({
       paste0(prettyNum(sum(STATE_REPORT$`Total Beds`,na.rm = TRUE), big.mark=","), " Total Beds")
     })
     
     
     
     
     
     output$TotalBeds60 <- renderText({
       paste0(prettyNum(sum(STATE_REPORT$`BEDS_CAPACITY@60`,na.rm = TRUE), big.mark=","), " Beds Capped at 60%")
     })
     
   
   


      
    })
    






  })

  

  
  
  

  #   renderLeaflet({
  #
  #   #token <- "pk.eyJ1IjoiY3VsdHVyZW9maW5zaWdodCIsImEiOiJjajV4cnJ6NzMwNHI5MnFwZ3E4cDFsMTBuIn0.I2QzkctPro7acqZBVaJ7Nw"
  #
  #
  #
  #   library(sp)
  #   library(leaflet)
  #
  #   df <- data.frame(longitude = runif(10, -97.365268, -97.356546),
  #                    latitude = runif(10, 32.706071, 32.712210))
  #
  #   coordinates(df) <- ~longitude+latitude
  #
  #   leaflet(df) %>% addMarkers() %>%   addTiles(urlTemplate = maptile, attribution = mapattr)
  #
  #
  #
  #   tcu_map <- "https://api.mapbox.com/styles/v1/satyamedidi/ck8m9wubu0r9i1ip7s0fz2fmi.html?fresh=true&title=view&access_token=pk.eyJ1Ijoic2F0eWFtZWRpZGkiLCJhIjoiY2pkdW1rNzQwMWVtczJ3bGxnd3VqZ2JkbSJ9.oBm8An8Fb3dpCGoWdsrcjQ"
  #
  #   mapattr <- '© <a href="https://www.mapbox.com/map-feedback/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright" target="_blank">OpenStreetMap</a>'
  #
  #
  #   leaflet(df) %>%
  #     addMarkers() %>%
  #     addTiles(urlTemplate = tcu_map, attribution = map_attr)
  #
  #   # leaflet(data = reactive_us[[input$region]]) %>%
  #   #   #addProviderTiles("CartoDB.DarkMatter", options = tileOptions(minZoom = 7, maxZoom = 13)) %>%
  #   #   addTiles(urlTemplate = maptile, attribution = mapattr, options = tileOptions(minZoom = 7, maxZoom = 13)) %>%
  #   #   setView(centroids[[input$region]]$x + 0.05, centroids[[input$region]]$y, zoom = zoom()) %>%
  #   #   addFullscreenControl
  #
  #
  #   us_state <- us_today %>% filter(us_today$Province_State == input$State)
  #
  #
  #   usmap <- leaflet(us_state) %>%
  #     addTiles(urlTemplate = maptile,
  #              mapattr,
  #              #attribution = mapattr,
  #              options = tileOptions(minZoom = 7, maxZoom = 13)) %>% addCircles(lng = us_state$Long_,lat = us_state$Lat)
  #
  #
  #
  #
  #     colors <- c("#ffaa1d","#d92121")
  #
  #     usmap %>%
  #       addMinicharts(
  #
  #         us_state$Lat,
  #         us_state$Long_,
  #         type = "pie",
  #         chartdata = us_state[, c("Cases","deaths")],
  #         colorPalette = colors,
  #         legendPosition = "bottomright",
  #
  #
  #
  #         popup = popupArgs(
  #
  #           html = paste0(
  #             "<div>",
  #             "<h3>",
  #             "County: ",
  #             us_state$Admin2,
  #             "</h3>",
  #             "<h5>",
  #             "<br>",
  #             "Total Cases: ",
  #             prettyNum(us_state$Cases,big.mark = ","),
  #             "</h5>",
  #             "<h5>",
  #             "Total Deaths: ",
  #             prettyNum(us_state$deaths,big.mark = ","),
  #             "</h5>",
  #             "</div>"
  #           )),
  #
  #         width = 80 * sqrt(us_state$Cases) / sqrt(max(us_state$Cases)), transitionTime = 0
  #       )
  #
  #
  #   })
  #
  #   })
  #
    

  

    
   # output$USVIEW <- renderLeaflet({ 
   #    
   #    
   #    
   #    
   #    
   #    
   #    
   #    
   #    treemap <- leaflet(reactive_us()) %>% addTiles() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% addControlGPS( options = gpsOptions(position = "topleft", activate = TRUE,autoCenter = TRUE, maxZoom = 10, setView = TRUE))
   #    
   #    # 
   #    #       %>%
   #    #         addCircleMarkers(lng= ~test$long, lat= ~test$lat,
   #    #                          popup= paste("<b>", test$displayName,"</b>", "<br>","<b> Total Confirmed: </b>",  test$totalConfirmed,
   #    #                                       "<br>","<b> Total Deaths: </b>",test$totalDeaths,"<br>","<b> Total Recovered: </b>",test$totalRecovered)) %>% addControlGPS( options = gpsOptions(position = "topleft", activate = TRUE,
   #    # 
   #    # 
   #    
   #    
   #    #                                                                                                                                                                                         
   #    #                                                                                                                                                                                         autoCenter = TRUE, maxZoom = 10, setView = TRUE)) 
   # 
   # 
   #    
   #    
   #    
   #    

   #  
   #  
    
    
    
    
    # barda <- points() %>% select(totalConfirmed,totalRecovered,totalDeaths)
    # 
    # 
    # 
    # basemap %>%
    #   addMinicharts(
    #     points()$long, points()$lat,
    #     type = "bar",
    #     chartdata = barda,
    #     colorPalette = colors,
    #     width = 45, height = 50
    #   )
    
    

  
  
  
  
  














