#######################################################################################
#### FIT 5147 Data Visualisation
#### Visualisation Project - Indego Bike Share System
#### Ting Hsu
#### 28898702
#######################################################################################


#######################################################################################
####----------------------------------------------###
####    load libraries and sources       
####----------------------------------------------###
#######################################################################################

# load helper functions
source('helper.R')

# check if libraries are installed
# list.of.packages <- c('devtools', 'leaflet', 'plotly', 'DT', 'shinythemes', 'htmlwidgets')
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# 
# devtools::install_github("timelyportfolio/parsetR")

# load libraries 
require(shiny)
require(shinythemes)
require(DT)
require(plotly)
require(leaflet)
require(parsetR)


#####################################################################################################################################
## server
#####################################################################################################################################

server <- function(input, output, session) { 
   
    # initialise a random value and variable to trigger modal dialog
    set.seed(122)
    firstpage <- rnorm(500)
    
    
    #################################################################################### 
    ### dataframe
    #################################################################################### 
    
    subscriber_df <- reactive({
      sub_df <- trip_count_helper(input$tripcountY, input$tripcountM, Subscriber) 
      sub_df
    })
    
    non_subscriber_df <- reactive({
      non_df <- trip_count_helper(input$tripcountY, input$tripcountM, nonSubscriber)
      non_df
    })
    
    
    #################################################################################### 
    ### month variable name change
    #################################################################################### 
    
    # to be used in the trip count plot and trip duration plot
    month_name <- reactive({
      switch(input$tripcountM,
             'All' = 'All',
             '1' = 'January',
             '2' = 'Feburary',
             '3' = 'March',
             '4' = 'April',
             '5' = 'May',
             '6' = 'June',
             '7' = 'July',
             '8' = 'August',
             '9' = 'September',
             '10' = 'October',
             '11' = 'November',
             '12' = 'December')
      
    })
    
    
    #################################################################################### 
    ### trip count text in sidePanel - use fa icons
    #################################################################################### 
    
    # create summary table to hold trip counts and trip duration 
    tripCountsummary <- reactive({
      df <- data.frame(
        'subTrip' = sum(aggregated_trip_count(input$tripcountY, input$tripcountM, subscriber_df())$count),
        'subTime' = (subscriber_df() %>% dplyr::group_by(date) %>% dplyr::summarise(mean(duration))  %>% `colnames<-`(c('time', 'avg')) %>% ungroup() %>% dplyr::summarise(m = round(mean(avg))))$m,
        'nonTrip' = sum(aggregated_trip_count(input$tripcountY, input$tripcountM, non_subscriber_df())$count),
        'nonTime' = (non_subscriber_df() %>% dplyr::group_by(date) %>% dplyr::summarise(mean(duration))  %>% `colnames<-`(c('time', 'avg')) %>%ungroup() %>% dplyr::summarise(m = round(mean(avg))))$m)
      df
    })
    
    
    # titles
    output$summaryMainTitle1 <- renderText({'<p align ="left"; style = "font-size:30px"><span class = "fa fa-bicycle"></span> Trip Count</p>'})
    output$summaryMainTitle2 <- renderText({'<p align ="left"; style = "font-size:30px"><span class ="far fa-clock"></span> Duration</p>'})
    
    # values
    output$subTripCount <- renderText({paste('<p align ="left"; style = "font-size:26px">', as.character(tripCountsummary()$subTrip),'</p>')})
    output$nonTripCount <- renderText(
      
    )
    output$subTripTime <- renderText({paste('<p align ="left"; style = "font-size:26px">', as.character(tripCountsummary()$subTime), '  min','</p>')})
    output$nonTripTime <- renderText({paste('<p align ="left"; style = "font-size:26px">', as.character(tripCountsummary()$nonTime),'  min','</p>')})
    
    
    
    #################################################################################### 
    # debugger
    #################################################################################### 
    
    # use random text as placeholder to test function
    output$text1 <- renderText({
      'ROIJfojf;oqejf'
    })
    
    
    #################################################################################### 
    # use modal as a banner 
    #################################################################################### 
    
    # event will be called when the variable firstpage changes, which only happens once at the start when its value is set 
    observeEvent(once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = firstpage, { 
      showModal(modalDialog(
        easyClose = T,
        title=HTML('<br>'),
        size = 'l',
        tags$head(tags$style('body,  { font-family: "Open Sans"; background-color: black;}')),
        HTML('<h1><strong><center>What can we learn from over 1.4 million records of Indego bike trips?</center></strong></h1>')
      ))
    })
    
    
    #####################################################################################################################################
    ## DT table 
    #####################################################################################################################################
    
    # return different table based on choice
    downloadValue <- reactive({
      list(input$dataType, input$dataYear)

    })
  
    
    downloadData <- reactive({
      df <- switch(downloadValue()[[1]],
                   'All' = rbind(Subscriber, nonSubscriber),
                   'Subscriber' = Subscriber,
                   'Non-subscriber' = nonSubscriber)

      if (downloadValue()[[2]] == 'All') {
        df
      } 
      
      else if (downloadValue()[[2]] == '2016') {
        df <- df[df$year == '2016', ]
      }
      
      else if (downloadValue()[[2]] == '2017') {
        df <- df[df$year == '2017', ]
      }
      
      df
      
    })
    
    output$table1 <- DT::renderDataTable({
      downloadData()

    })
    
    
    #####################################################################################################################################
    ## plot: trip count 
    #####################################################################################################################################
    
    output$tripcountPlot <- renderPlotly({
      
      maintTitle <- paste0("Number of Trip Count        ", "Year : ", input$tripcountY, "    Month : ", month_name())
      
      x_title <- 
        if (input$tripcountY != 'All' & (input$tripcountM !='All')) {
          'Date'
        } else if ((input$tripcountY =='All') & (input$tripcountM =='All')) {
          'Date'
        } else if ((input$tripcountY !='All') & (input$tripcountM =='All')) {
          'Month'
        } else if ((input$tripcountY =='All') & (input$tripcountM !='All')) {
          'Year'
        }
      
      subdf <- aggregated_trip_count(input$tripcountY, input$tripcountM, subscriber_df())
      nondf <- aggregated_trip_count(input$tripcountY, input$tripcountM, non_subscriber_df())
      
      if (input$tripcountY == 'All' && input$tripcountM == 'All') {
        tripcountPlot <- plot_ly() %>%
          add_bars(data = subdf,
                   x = subdf$time, 
                   y= subdf$count, 
                   name = 'Subscriber',
                   marker = list(color = '#e09350')) %>%
          add_bars(data = nondf,
                   x = nondf$time, 
                   y= nondf$count, 
                   name = 'Non Subscriber',
                   marker = list(color = '#33699b')) %>%
          add_annotations(x = 300,
                          y = 3500,
                          text = 'SEPTA Strike') %>%
          add_annotations(x = 430,
                          y = 550,
                          text = 'Snow Storm') %>%
          add_annotations(x = 480,
                          y = 2800,
                          text = 'NFL Draft') %>%
          layout(title = maintTitle,
                 titlefont = list(size = 17),
                 margin = list(r = 90,
                               l = 90,
                               pad = 4),
                 xaxis = list(title = x_title,
                              titlefont = c(size=12),
                              tickangle=45,
                              tickfont= list(size=7),
                              gridcolor = 'white'), 
                 yaxis = list(title = "Number of Trip",
                              titlefont = list(size=12)),
                 legend = list(orientation = "h",   # show entries horizontally
                               xanchor = "bottom",  # use center of legend as anchor
                               x = 0.3, 
                               y = -0.25,
                               font = list(size = 10)),
                 paper_bgcolor='rgba(0,0,0,0)',
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 margin = list(l=100, pad = 4))
        tripcountPlot$elementId <- NULL
        tripcountPlot
      }
      
      else {
        tripcountPlot <- plot_ly() %>%
          add_bars(data = subdf,
                   x = subdf$time, 
                   y= subdf$count, 
                   name = 'Subscriber',
                   marker = list(color = '#e09350')) %>%
          add_bars(data = nondf,
                   x = nondf$time, 
                   y= nondf$count, 
                   name = 'Non Subscriber',
                   marker = list(color = '#33699b')) %>%
          layout(title = maintTitle,
                 titlefont = list(size = 17),
                 margin = list(r = 90,
                               l = 90,
                               pad = 4),
                 xaxis = list(title = x_title,
                              titlefont = c(size=12),
                              tickangle=45,
                              tickfont= list(size=7),
                              gridcolor = 'white'), 
                 yaxis = list(title = "Number of Trip",
                              titlefont = list(size=12)),
                 legend = list(orientation = "h",   # show entries horizontally
                               xanchor = "bottom",  # use center of legend as anchor
                               x = 0.3, 
                               y = -0.25,
                               font = list(size = 14)),
                 paper_bgcolor='rgba(0,0,0,0)',
                 plot_bgcolor = 'rgba(0,0,0,0)',
                 margin = list(l=100, pad = 4))
        tripcountPlot$elementId <- NULL
        tripcountPlot
      }
    })
    
    
    #####################################################################################################################################
    ## plot: duration 
    #####################################################################################################################################
    
    # line graph to visualise trip duration
    output$durationPlot <- renderPlotly({
      
      # change title of the plot according to input values
      maintTitle <- paste0("Average Duration of Trip        ", " Year : ", input$tripcountY, "    Month : ", month_name())
      x_title <- 
        if (input$tripcountY != 'All' & (input$tripcountM !='All')) {
          'Date'
        } else if ((input$tripcountY =='All') & (input$tripcountM =='All')) {
          'Date'
        } else if ((input$tripcountY !='All') & (input$tripcountM =='All')) {
          'Month'
        } else if ((input$tripcountY =='All') & (input$tripcountM !='All')) {
          'Year'
        }
      

      # subscriber aggregated duration
      
      subdf <- duration_helper(input$tripcountY, input$tripcountM, subscriber_df())
      nondf <- duration_helper(input$tripcountY, input$tripcountM, non_subscriber_df())
      
      durationPlot <- plot_ly() %>%
        add_lines(data = subdf, 
                  x = subdf$time, 
                  y= subdf$avg,
                  showlegend=TRUE,
                  name = 'Subscriber',
                  line = list(color = '#e09350', 
                              width = 3)) %>%
        add_lines(data = nondf, 
                  x = nondf$time, 
                  y = nondf$avg,
                  name = 'Non Subscriber',
                  line = list(color = '#33699b', 
                              width = 3)) %>%
        layout(title = maintTitle,
               titlefont = list(size = 17, 
                                position = ''),
               margin = list(r = 100,
                             pad = 10),
               paper_bgcolor = 'white',
               plot_bgcolor = 'white',
               xaxis = list(title = x_title,
                            titlefont = list(size=12),
                            tickangle=45,
                            tickfont= list(size=7),
                            gridcolor = 'white'), 
               yaxis = list(title = "Minutes",
                            titlefont = list(size=12)),
               legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "bottom",  # use center of legend as anchor
                             x = 0.3, 
                             y = -0.25,
                             font = list(size = 14)),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)',
               margin = list(l=100, pad = 4))
      
      
      durationPlot$elementId <- NULL
      durationPlot
      
    })
    
    #####################################################################################################################################
    # plot : heatmap
    #####################################################################################################################################
    
    # listen to input in travel behaviour tab 
    heatmap <- reactive({
      non <- trip_count_helper(input$travcountY, input$travcountM, nonSubscriber) %>% dplyr::group_by(weekday, weekday_weekend, hour) %>% dplyr::summarise(count = n()) %>% data.frame()
      sub <- trip_count_helper(input$travcountY, input$travcountM, Subscriber) %>% dplyr::group_by(weekday,  weekday_weekend, hour) %>% dplyr::summarise(count = n()) %>% data.frame()
      list(non, sub) #[[1]] non [[2]] sub
    })
    
    # heatmap to visualise the number of trips over hour and weekday: subscriber
    output$heatmapSub <- renderPlotly({
      
      Plot1 <- plot_ly(
        data = heatmap()[[2]], 
        x = heatmap()[[2]]$weekday,
        y = heatmap()[[2]]$hour,
        z = heatmap()[[2]]$count, 
        type = "heatmap",
        colors = 'Blues',
        name = 'Trip Count',
        reversescale = F ) %>%
        layout(title = 'Number of Trip Over A Week - Subscriber',
               xaxis = list(title = "Weekday",
                            titlefont = c(size=3),
                            tickangle = 0,
                            tickfont = list(size=9.5),
                            position = 0,
                            ticktext = list('Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun')),
               yaxis = list(title = "Hour"),
               margin = list(b = 80, l = 80),
               width = 550,
               height = 450,
               legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.5, 
                             y = 1),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      Plot1$elementId <- NULL
      Plot1
    })
    
    # heatmap to visualise the number of trips over hour and weekday: non -subscriber
    output$heatmapNon <- renderPlotly({
      
      Plot1 <- plot_ly(
        data = heatmap()[[1]],
        x = heatmap()[[1]]$weekday,
        y = heatmap()[[1]]$hour,
        z = heatmap()[[1]]$count, 
        type = "heatmap",
        colors = 'Blues',
        name = 'Trip Count',
        reversescale = F ) %>%
        layout(title = 'Number of Trip Over A Week - Non-Subscriber',
               xaxis = list(title = "Weekday",
                            titlefont = c(size=3),
                            tickangle = 0,
                            tickfont = list(size=9.5),
                            position = 0),
               yaxis = list(title = "Hour"),
               margin = list(b = 80, l = 80),
               width = 550,
               height = 450,
               legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.2, 
                             y = 1),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)')
      Plot1$elementId <- NULL
      Plot1
    })
    
    
    
    #################################################################################### 
    ### travel pattern text in sidePanel - use fa icons
    #################################################################################### 
    
    # create summary table to hold number of trips on weekday/weekend and number of trips during peak/non-peak hour
    # reactive to heatmap() value
    travPatternSummary <- reactive({
      
      
      peak <- c(6, 7, 8, 9, 16,17,18,19) # define peak hour
      
      # [[1]] nonsubscriber [[2]] subscriber
      non_wday <- heatmap()[[1]][heatmap()[[1]][['weekday_weekend']] == 'weekday',]
      non_wend <- heatmap()[[1]][heatmap()[[1]][['weekday_weekend']] == 'weekend',]
      sub_wday <- heatmap()[[2]][heatmap()[[2]][['weekday_weekend']] == 'weekday',]
      sub_wend <- heatmap()[[2]][heatmap()[[2]][['weekday_weekend']] == 'weekend',]
      
      non_peak <- heatmap()[[1]][heatmap()[[1]][['hour']] %in% peak,]
      sub_peak <- heatmap()[[2]][heatmap()[[2]][['hour']] %in% peak,]
      non_total <- sum(heatmap()[[1]]$count)
      sub_total <- sum(heatmap()[[2]]$count)
      
      df <- data.frame(
        'nonWday' = sum(non_wday$count),
        'nonWend' = sum(non_wend$count),
        'subWday' = sum(sub_wday$count),
        'subWend' = sum(sub_wend$count),
        'nonPeak' = sum(non_peak$count),
        'nonNPeak' = non_total - sum(non_peak$count),
        'subPeak' = sum(sub_peak$count),
        'subNPeak' = sub_total - sum(sub_peak$count))
      
      df
    })
    
    # set HTML tags given input$peakCheck status 
    to_show <- reactive({ if (input$peakCheck != FALSE) {
      mainTitle1 <- '<p align ="left"; style = "font-size:30px"><span class ="fas fa-angle-double-right"></span> Peak Hour </p>'
      mainTitle2 <- '<p align ="left"; style = "font-size:30px"><span class ="fas fa-angle-double-right"></span> Non-peak</p>'
      non1 <- paste('<p align ="left"; style = "font-size:26px">', as.character(travPatternSummary()$nonPeak),'</p>')
      non2 <- paste('<p align ="left"; style = "font-size:26px">', as.character(travPatternSummary()$nonNPeak),'</p>')
      sub1 <- paste('<p align ="left"; style = "font-size:26px">', as.character(travPatternSummary()$subPeak),'</p>')
      sub2 <- paste('<p align ="left"; style = "font-size:26px">', as.character(travPatternSummary()$subNPeak),'</p>')
      list(mainTitle1, mainTitle2, non1, non2, sub1, sub2)
    }
      else if (input$peakCheck == FALSE) {
        mainTitle1 <- '<p align ="left"; style = "font-size:30px"><span class ="fas fa-angle-double-right"></span> Weekday </p>'
        mainTitle2 <- '<p align ="left"; style = "font-size:30px"><span class ="fas fa-angle-double-right"></span> Weekend </p>'
        non1 <- paste('<p align ="left"; style = "font-size:26px">', as.character(travPatternSummary()$nonWday),'</p>')
        non2 <- paste('<p align ="left"; style = "font-size:26px">', as.character(travPatternSummary()$nonWend),'</p>')
        sub1 <- paste('<p align ="left"; style = "font-size:26px">', as.character(travPatternSummary()$subWday),'</p>')
        sub2 <- paste('<p align ="left"; style = "font-size:26px">', as.character(travPatternSummary()$subWend),'</p>')
        list(mainTitle1, mainTitle2, non1, non2, sub1, sub2)
      }
      
    })
    
    output$patternMainTitle1 <- renderText({to_show()[[1]]}) #title : peak / weekday
    output$patternMainTitle2 <- renderText({to_show()[[2]]}) #title : non-peak / weekend
    
    output$subWday <- renderText({to_show()[[5]]}) # values : subscriber
    output$nonWday <- renderText({to_show()[[3]]}) # values : non subscriber
    output$subWend <- renderText({to_show()[[6]]}) # values : subscriber
    output$nonWend <- renderText({to_show()[[4]]}) # values : non subscriber
    
    #####################################################################################################################################
    # plot : hourly bar chart
    #####################################################################################################################################
    
    # reactive to heatmap() value
    output$hourly <- renderPlotly({ 
      
      p <- plot_ly() %>%
        add_bars(data = heatmap()[[2]],
                 x = heatmap()[[2]]$hour, 
                 y= heatmap()[[2]]$count, 
                 name = 'Subscriber',
                 marker = list(color = '#e09350')) %>%
        add_bars(data = heatmap()[[1]], 
                 x = heatmap()[[1]]$hour, 
                 y= heatmap()[[1]]$count,
                 name = 'Non Subscriber',
                 marker = list(color = '#33699b')) %>%
        layout(title = "Hourly Trip Count",
               titlefont = list(size = 17),
               xaxis = list(title = "Hour",
                            titlefont = c(size=5),
                            tickangle =0), 
               yaxis = list(title = "Number of Trip",
                            titlefont = c(size=5, color='white')),
               legend = list(orientation = "v",   # show entries horizontally
                             xanchor = "top",  # use center of legend as anchor
                             x = -0.3, 
                             y = -0.4,
                             font = list(size = 8)),
               paper_bgcolor='rgba(0,0,0,0)',
               plot_bgcolor = 'rgba(0,0,0,0)',
               margin = list(l=100, pad = 4))
      
      p$elementId <- NULL
      p
    })
    
    #####################################################################################################################################
    ## map : all stations and locations
    ####################################################################################################################################
    
    output$philly <- renderLeaflet({
      phillyMap
    })
    
    #####################################################################################################################################
    ## map : all popular stations (sub + nonsub)
    ####################################################################################################################################
    
    # react to input$go, store values all at once
    stations <- eventReactive(req(input$go), {
      c(req(input$stationY), 
        req(input$stationM), 
        req(input$riderType))
    })
    
    
    # generate new dataset for leafletProxy
    station_data <- reactive({
      
      radius <- c(0.08, 0.13, 0.35, 0.5) # predefined values to scale circle markers 
      
      # subset data for subscribers and non subscribers given year and month
      subscriber_df_station <- trip_count_helper(stations()[1], stations()[2], Subscriber)
      non_subscriber_df_station <- trip_count_helper(stations()[1], stations()[2], nonSubscriber)
      
      # lookup coordinates for end stations and start stations
      sub_start <- start_getter(subscriber_df_station)
      sub_end <- end_getter(subscriber_df_station)
      non_end <- end_getter(non_subscriber_df_station)
      non_start <- start_getter(non_subscriber_df_station)
      
      # set corresponding radius to ensure that marker sizes are scaled based on count
      if ((stations()[1] =='All') & (stations()[2] !='All')) {
        radius <- radius[3]
      } else if ((stations()[1] =='All') & (stations()[2] =='All')){
        radius <- radius[2]
      } else if ((stations()[1]!='All') & (stations()[2] !='All')){
        radius <- radius[4]
      } else if ((stations()[1]!='All') & (stations()[2] =='All')){
        radius <- radius[2]
      }
      
      # create a list to hold two dataframes (start stations, end stations) for subscriber
      if (stations()[3] == 'Subscriber'){
        total <- list(sub_start, sub_end, radius)
      }
      
      # create a list to hold two dataframes (start stations, end stations) for non subscriber
      if (stations()[3] == 'Non-Subscriber'){
        total <- list(non_start, non_end, radius)
      }
      
      # create a list to hold two dataframes (start stations, end stations) for all riders
      if (stations()[3] == 'All'){
        
        # merge two dataframes of end stations from both riders
        all_end <- merge(non_end, sub_end, by = 'StationName', all=TRUE)[, c(1,2,5)] %>% 
          dplyr::group_by(StationName) %>% 
          replace(is.na(.), 0) %>%
          dplyr::summarise(total = sum(Count.x, Count.y))%>%
          arrange(desc(total)) %>%
          data.frame() %>%
          mutate(lat = unlist(lapply(StationName, lookupStationLat)),
                 lon = unlist(lapply(StationName, lookupStationLon))) %>%
          set_colnames(c('StationName', 'Count', 'lat', 'lon'))%>%
          data.frame() %>%
          "["(.,1:10,)
        
        # merge two dataframes of start stations from both riders
        all_start <- merge(non_start, sub_start, by = 'StationName', all=TRUE)[, c(1,2,5)] %>% 
          dplyr::group_by(StationName) %>% 
          replace(is.na(.), 0) %>%
          dplyr::summarise(total = sum(Count.x, Count.y))%>%
          arrange(desc(total)) %>%
          data.frame() %>%
          mutate(lat = unlist(lapply(StationName, lookupStationLat)),
                 lon = unlist(lapply(StationName, lookupStationLon))) %>%
          set_colnames(c('StationName', 'Count', 'lat', 'lon'))%>%
          data.frame() %>%
          "["(.,1:10,)
        
        total <- list(all_start, all_end, radius)
      }
      
      total
      
    })
    
    # create a base map
    output$riderMap <- renderLeaflet({ 
      leaflet() %>%
        addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
        fitBounds(lng1 = -75.140, lng2 = -75.178, lat1 = 39.934, lat2 = 39.97)
    })
    
    
    # clear and plot new markers based on the values of station_data() 
    # other markers (train, university, attraction, supermarket) remain the same
    observe({
      leafletProxy('riderMap') %>%
        clearMarkerClusters() %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(data= station_data()[[1]],
                         lat = station_data()[[1]]$lat,
                         lng= station_data()[[1]]$lon,
                         radius= sqrt(station_data()[[1]]$Count) * station_data()[[3]],
                         weight=0,
                         stroke = FALSE,
                         fillOpacity = 0.5,
                         color= 'orange',
                         group=c('Start Station'),
                         label=paste(as.character(station_data()[[1]]$StationName), ' : ', station_data()[[1]]$Count),
                         labelOptions = labelOptions(
                           style = list(
                             "font-family" = "open sans",
                             "box-shadow" = "0.5px 0.5px rgba(0,0,0,0.25)",
                             "font-size" = "13px",
                             "border-color" = "rgba(0,0,0,0.5)"))) %>%
        addAwesomeMarkers(data = train_station, 
                          lng = ~ as.numeric(LON), 
                          lat = ~ as.numeric(LAT), 
                          icon = awesomeIcons(
                            icon = "train",
                            library = "fa",
                            markerColor = "blue"), 
                          label = train_station$STOPNAME,
                          labelOptions = labelOptions(noHide = F,
                                                      direction = "center",
                                                      style = list(
                                                        "font-family" = "open sans",
                                                        "box-shadow" = "0.5px 0.5px rgba(0,0,0,0.25)",
                                                        "font-size" = "13px",
                                                        "border-color" = "rgba(0,0,0,0.5)")),
                          group = 'Train Station') %>%
        addCircleMarkers(data= station_data()[[2]],
                         lat = station_data()[[2]]$lat,
                         lng= station_data()[[2]]$lon,
                         radius= sqrt(station_data()[[2]]$Count) * station_data()[[3]],
                         color= '#0ABAB5',
                         group=c('End Station'),
                         weight=0,
                         stroke = FALSE,
                         fillOpacity = 0.5,
                         label=paste(as.character(station_data()[[2]]$StationName), ' : ', station_data()[[2]]$Count),
                         labelOptions = labelOptions(
                           style = list(
                             "font-family" = "open sans",
                             "box-shadow" = "0.5px 0.5px rgba(0,0,0,0.25)",
                             "font-size" = "13px",
                             "border-color" = "rgba(0,0,0,0.5)"))) %>%
        addCircleMarkers(data = att_cor, 
                         lng = ~as.numeric(lng), 
                         lat = ~as.numeric(lat), 
                         radius = 5, 
                         color = 'yellow', 
                         weight=0,
                         stroke = FALSE,
                         fillOpacity = 0.5,
                         label = att_cor$name,
                         labelOptions = labelOptions(noHide = F,
                                                     direction = "center",
                                                     style = list(
                                                       "font-family" = "open sans",
                                                       "box-shadow" = "0.5px 0.5px rgba(0,0,0,0.25)",
                                                       "font-size" = "13px",
                                                       "border-color" = "rgba(0,0,0,0.5)")),
                         group = 'Attractions') %>%
        addAwesomeMarkers(data = supermarket, 
                          lng = ~ lon, 
                          lat = ~ lat, 
                          icon = awesomeIcons(
                            icon = "shopping-cart",
                            library = "fa",
                            markerColor = "orange"), 
                          label = supermarket$name,
                          labelOptions = labelOptions(noHide = F,
                                                      direction = "center",
                                                      style = list(
                                                        "font-family" = "open sans",
                                                        "box-shadow" = "0.5px 0.5px rgba(0,0,0,0.25)",
                                                        "font-size" = "13px",
                                                        "border-color" = "rgba(0,0,0,0.5)")),
                          group = 'Supermarket') %>%
        addAwesomeMarkers(data=uni_cor, 
                          lat= ~lat, 
                          lng = ~lon,
                          icon = awesomeIcons(
                            icon = "university",
                            library = "fa",
                            markerColor = "white"), 
                          label = uni_cor$name,
                          labelOptions = labelOptions(noHide = F,
                                                      direction = "center",
                                                      style = list(
                                                        "font-family" = "open sans",
                                                        "box-shadow" = "0.5px 0.5px rgba(0,0,0,0.25)",
                                                        "font-size" = "13px",
                                                        "border-color" = "rgba(0,0,0,0.5)")),
                          group = 'University') %>%
        addLayersControl(overlayGroups = c('Start Station', 'End Station', 'University', 'Attractions', 'Train Station', 'Supermarket'),
                         options = layersControlOptions(collapsed = F)) %>%
        hideGroup(group = c( 'University', 'Attractions', 'Train Station', 'Supermarket'))
      
    })
    #####################################################################################################################################
    ## plot : route flow chart 
    #####################################################################################################################################
    
    
    # create a list that holds two dataframes of popular routes for subscribers and non-subscribers
    route_df <- eventReactive(req(input$go2), {
      sub <- req(route_getter(trip_count_helper(input$routeY, input$routeM, Subscriber)))
      non <- req(route_getter(trip_count_helper(input$routeY, input$routeM, nonSubscriber)))
      final <- list(sub, non)
    })
    
    
    # create flowchart
    output$route_sub <- renderParset({parsetR::parset(route_df()[[1]], dimensions = c('Start Station Name', 'End Station Name'), 
                                                      value = htmlwidgets::JS("function(d){return d.Count}"))})
    output$route_non <- renderParset({parsetR::parset(route_df()[[2]],dimensions = c('Start Station Name', 'End Station Name'), 
                                                      value = htmlwidgets::JS("function(d){return d.Count}"))})
    
    
    #####################################################################################################################################
    ## plot : weather 
    #####################################################################################################################################
    
    # choose from two pre made weather plots
    output$weatherPlot <- renderPlotly({
      weatherPlot <- switch(input$weatherRadio,
             'Temperature' = cor_temp_trip,
             'Percipitation' = cor_rain_trip)
      weatherPlot$elementId <- NULL
      weatherPlot
    })
    
    
    #####################################################################################################################################
    ## action button : download 
    #####################################################################################################################################
    
    output$downloadbutton <- downloadHandler(
      filename = function() {
        paste(input$filename, ".IndegoTrip", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(downloadData(), file)
      }
    )


}