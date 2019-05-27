
#######################################################################################
####----------------------------------------------###
####    load libraries         
####----------------------------------------------###
#######################################################################################

# check if libraries are installed
# list.of.packages <- c('dplyr', 'broom', 'data.table', 'plyr', 'purrr', 'magrittr', 'leaflet', 'plotly')
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

require(plyr)
require(dplyr)
require(data.table)
require(magrittr)
require(plotly)
require(leaflet)

#######################################################################################
####----------------------------------------------###
####    load data         
####----------------------------------------------###
#######################################################################################

##################################
### load in load in trip data and station location  -----
##################################

folder <- '/Users/tinghsu/Desktop/Monash/FIT5147 VIs/IndegoBikeShare/RawData/'
folder_cleaned <- '/Users/tinghsu/Desktop/Monash/FIT5147 VIs/IndegoBikeShare/cleanedData/'

##################################
### trip: all data  -----
##################################
# df <- data.frame(fread('./data/indego_master.csv', stringsAsFactors=FALSE, drop='V1'))
weekday_order <- c('Monday', 'Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
# set levels to reorder columns in plot
# df$weekday <- factor(df$weekday, levels = weekday_order)
# df$duration <- as.numeric(df$duration)

##################################
### riders subsets  -----
##################################
nonSubscriber <- data.frame(fread('./data/nonSubscriber.csv', stringsAsFactors=FALSE, drop='V1'))
Subscriber <- data.frame(fread('./data/Subscriber.csv', stringsAsFactors=FALSE, drop='V1'))
nonSubscriber$weekday <- factor(nonSubscriber$weekday, levels = weekday_order)
nonSubscriber$duration <- as.numeric(nonSubscriber$duration)
Subscriber$weekday <- factor(Subscriber$weekday, levels = weekday_order)
Subscriber$duration <- as.numeric(Subscriber$duration)

##################################
### locationL: Indego station  -----
##################################
station_loc <- data.frame(fread('./data/station_coord.csv', stringsAsFactors=FALSE))[,2:4]
station_loc <- station_loc %>%
  dplyr::filter(stationName != 'Virtual Station')

##################################
### weather  -----
##################################
weather <- data.frame(fread('./data/merged_weather_trip.csv', stringsAsFactors = FALSE))
# loess smoother 
loess_temp <- loess(data = weather, count~Avg)
loess_temp <- as.data.frame(broom::augment(loess_temp))

##################################
### locationL: amtrak station  -----
##################################
amtrak <- data.frame('station' = c('North Philadelphia', 'Philadelphia'),
                     'lat' = c(39.9969062805176, 39.9546089172363),
                     'lon' = c(-75.1543655395508, -75.1831893920898))
##################################
### location: business centers and station locations  -----
##################################
business_cor <- fread('./data/businessCor.csv', stringsAsFactors = FALSE, drop='V1')

##################################
### location: location attractions  -----
##################################
att_cor <- fread('./data/att_cor.csv', stringsAsFactors = FALSE, drop = 'V1')

##################################
### location: supermarket  -----
##################################
supermarket <- fread('./data/supermarket_cor.csv', stringsAsFactors = FALSE, drop = 'V1')

##################################
### location: university  -----
##################################
uni_cor <- read.csv('./data/unicor.csv', stringsAsFactors=FALSE)

##################################
### location: subway station  -----
##################################

subway_station <- read.csv('./data/subway_cor.csv', stringsAsFactors = FALSE)

##################################
### location: transit station  -----
##################################

# all public transportation stations
septa_station <- read.csv('./data/SEPTAStopsByLineSpring2016.csv', stringsAsFactors = FALSE)

# train stations
train_station <- read.csv('./data/train_station.csv', stringsAsFactors = FALSE)

# only select the stops on particular subway lines and within certain boundary
philly_station <- septa_station %>%
  dplyr::filter(LINEABBR %in% c(10, 11, 13, 15, 34, 36)) %>%
  dplyr::filter(LON < -75.13 & LON > -75.21 & LAT > 39.80 & LAT < 39.99) %>%
  dplyr::select(LINEABBR, STOPNAME, LON, LAT)



philly_station <- rbind(philly_station, 
                        '219' = c('Train Station', 'University City - MFL', -75.190194, 39.947960),
                        '220' = c('Train Station', '30th Street Station - MFL',  -75.183263, 39.954909),
                        '221' = c('Train Station', 'Suburban Station - MFL', -75.166816, 39.954109),
                        '222' = c('Train Station', 'Jefferson Station - MFL', -75.158040, 39.952545),
                        '223' = c('Train Station', '2nd St Station - MFL', -75.143728, 39.949907),
                        '224' = c('Train Station', '12-13th and Locust', -75.162373, 39.948041),
                        '225' = c('Train Station', '8th and Market', -75.15359, 39.95115))

philly_station <- rbind(philly_station, subway_station) 

trolley10 <- philly_station %>%
  filter(LINEABBR == 10)

trolley11 <- philly_station %>%
  filter(LINEABBR == 11)

trolley13 <- philly_station %>%
  filter(LINEABBR == 13)

trolley15 <- philly_station %>%
  filter(LINEABBR == 15)

trolley34 <- philly_station %>%
  filter(LINEABBR == 34)

trolley36 <- philly_station %>%
  filter(LINEABBR == 36)

##################################
## leaflet map control names
##################################
# listOftransit <- c('Trolley 10', 'Trolley 11', 'Trolley 13', 'Trolley 15', 'Trolley 34', 'Trolley 36', 'Train Station', 'Subway Station', 'Amtrak Station')

listOfcontrol <- c('Trolley 10 (Lightrail)', 'Trolley 11 (Lightrail)', 'Train Station', 'Subway Station', 'Amtrak Station',
                   'Indego Station','Attractions','University', 'Subscriber', 'Non Subscriber', 'Business Center', 'Amtrak Station', 'Supermarket')



#######################################################################################
####----------------------------------------------###
####    helper functions
####----------------------------------------------###
#######################################################################################

#####################################################
# coordinats lookup : function to get station name, lat, lon -----
#####################################################
lookupStation <- function(stationName){
  stationCor <- data.frame(name = NA,
                           lat = NA,
                           lon = NA)
  stationCor$lat <- station_loc[station_loc$stationName == stationName,1]
  stationCor$lon <- station_loc[station_loc$stationName == stationName,2]
  stationCor$name <- stationName
  stationCor
}

#####################################################
# coordinats lookup : function to get lat -----
#####################################################
lookupStationLat <- function(stationName){
  lat <- station_loc[station_loc$stationName == stationName,1]
  lat
}

#####################################################
# coordinats lookup : function to get lon -----
#####################################################
lookupStationLon <- function(stationName){
  lon <- station_loc[station_loc$stationName == stationName,2]
  lon
}


#####################################################
# trip count helper -----
#####################################################
trip_count_helper <- function(year, month, df) {
  
  if ((year == 'All') & (month == 'All')){
    df <- df %>% dplyr::select(year, month, date, duration, start_station_name, end_station_name, weekday_weekend, weekday, hour)
  } 
  
  if ((year !='All') & (month !='All')) {
    df <-  df[df$year == year & df$month == month, ] 
  } 
  
  if ((year =='All') & (month !='All')) {
    df <-  df[df$month == month, ] 
  } 
  
  if ((year !='All') & (month == 'All')){
    df <-  df[df$year == year, ] 
  }
  
  data.frame(df)
}

#####################################################
# aggregated trip helper -----
#####################################################

aggregated_trip_count <- function(year, month, df) {
  if ((year == 'All') & (month == 'All')) {
    count <- df %>% dplyr::group_by(date) %>% dplyr::summarise(count=n()) %>% `colnames<-`(c('time', 'count')) %>% data.frame()
  }
  
  if ((year !='All') & (month !='All')) { 
    count <- df %>% dplyr::group_by(date) %>% dplyr::summarise(count = n()) %>% `colnames<-`(c('time', 'count')) %>% data.frame()
  }
  
  if ((year == 'All') & (month !='All')) {
    count <- df %>% dplyr::group_by(year, month) %>% dplyr::summarise(count = n()) %>% `colnames<-`(c('time', 'month', 'count')) %>% data.frame()
  } 
  
  if ((year !='All') & (month == 'All')){
    count <- df %>% dplyr::group_by(month) %>% dplyr::summarise(count = n()) %>% `colnames<-`(c('time', 'count')) %>% data.frame()
  }
  count
}

#####################################################
# duration  helper -----
#####################################################

duration_helper <- function(year, month, df) {
  if ((year == 'All') & (month == 'All')) {
    time <- df %>% dplyr::group_by(date) %>% 
      dplyr::summarise(mean(duration)) %>% 
      `colnames<-`(c('time', 'avg')) %>% data.frame()
  }
  
  if ((year !='All') & (month !='All')) { 
    time <- df %>% dplyr::group_by(date) %>% 
      dplyr::summarise(mean(duration)) %>% 
      `colnames<-`(c('time', 'avg')) %>% data.frame()
  }
  
  if ((year == 'All') & (month !='All')) {
    time <- df %>% dplyr::group_by(year, month) %>% 
      dplyr::summarise(mean(duration)) %>% 
      `colnames<-`(c('time', 'm', 'avg')) %>% data.frame()
  } 
  
  if ((year !='All') & (month == 'All')){
    time <- df %>% dplyr::group_by(month) %>% 
      dplyr::summarise(mean(duration)) %>% 
      `colnames<-`(c('time', 'avg')) %>% data.frame()
  }
  time
}

#####################################################
# start station helper -----
#####################################################

start_getter <- function(df){
  final <- df %>% 
    dplyr::group_by(start_station_name)%>%
    dplyr::summarise(counts = n()) %>%
    dplyr::arrange(desc(counts)) 
  final <- final[final$start_station_name!='Virtual Station',] 
  final <- final[1:10,] %>% 
    data.frame() %>%
    dplyr::mutate(lat = unlist(lapply(start_station_name, lookupStationLat)),
           lon = unlist(lapply(start_station_name, lookupStationLon)))  %>%
    set_colnames(c('StationName', 'Count', 'lat', 'lon'))
  
  final
}

#####################################################
# end station helper -----
#####################################################

end_getter <- function(df){
  final <- df %>% 
    dplyr::group_by(end_station_name)%>%
    dplyr::summarise(counts = n()) %>%
    dplyr::arrange(desc(counts)) 
  final <- final[final$end_station_name!='Virtual Station',] 
  final <- final[1:10,] %>% 
    data.frame() %>%
    dplyr::mutate(lat = unlist(lapply(end_station_name, lookupStationLat)),
           lon = unlist(lapply(end_station_name, lookupStationLon)))  %>%
    set_colnames(c('StationName', 'Count', 'lat', 'lon'))
  
  final
}


#####################################################
# route getter helper -----
#####################################################

route_getter <- function(df) {
  df <- df %>%
    dplyr::group_by(start_station_name, end_station_name) %>%
    dplyr::summarise(count = n()) %>%
    data.frame()
  df <- df[order(df$count, decreasing = TRUE),]
  df <- df[1:10,] %>% `colnames<-`(c('Start Station Name', 'End Station Name', 'Count'))
  df
}


#######################################################################################
####----------------------------------------------###
####    create subsets         
####----------------------------------------------###
#######################################################################################

subStart <- Subscriber %>% 
  dplyr::group_by(start_station_name)%>%
  dplyr::summarise(counts = n()) %>%
  dplyr::arrange(desc(counts)) %>%
  dplyr::slice(1:10) %>%
  dplyr::mutate(lat = unlist(lapply(start_station_name, lookupStationLat)),
         lon = unlist(lapply(start_station_name, lookupStationLon))) %>%
  as.data.frame()

subEnd <- Subscriber %>% 
  dplyr::group_by(end_station_name)%>%
  dplyr::summarise(counts = n()) %>%
  dplyr::arrange(desc(counts)) %>%
  dplyr::slice(1:10) %>%
  dplyr::mutate(lat = unlist(lapply(end_station_name, lookupStationLat)),
         lon = unlist(lapply(end_station_name, lookupStationLon))) %>%
  as.data.frame()

nonsubStart <- nonSubscriber %>% 
  dplyr::group_by(start_station_name)%>%
  dplyr::summarise(counts = n()) %>%
  arrange(desc(counts)) %>%
  dplyr::slice(1:10) %>%
  dplyr::mutate(lat = unlist(lapply(start_station_name, lookupStationLat)),
         lon = unlist(lapply(start_station_name, lookupStationLon))) %>%
  as.data.frame()

nonsubEnd <- nonSubscriber %>% 
  dplyr::group_by(end_station_name)%>%
  dplyr::summarise(counts = n()) %>%
  dplyr::arrange(desc(counts)) %>%
  dplyr::slice(1:10) %>%
  dplyr::mutate(lat = unlist(lapply(end_station_name, lookupStationLat)),
                lon = unlist(lapply(end_station_name, lookupStationLon))) %>%
  as.data.frame()

#####################################################
# all exploration map - "Explore Yourself" tab -----
#####################################################

phillyMap <- leaflet() %>%
  addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
  fitBounds(lng1 = -75.13, lng2 = -75.20, lat1 = 39.92, lat2 = 39.99) %>%
  addCircleMarkers(data = station_loc, 
                   lng = ~as.numeric(lon), 
                   lat = ~as.numeric(lat), 
                   radius = 4,
                   color = 'blue', 
                   opacity = .8, 
                   weight=0,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   label = station_loc$stationName,
                   group = 'Indego Station') %>%
  addCircleMarkers(data = att_cor, 
                   lng = ~as.numeric(lng), 
                   lat = ~as.numeric(lat), 
                   radius = 5, 
                   color = 'yellow', 
                   weight=0,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   label = att_cor$name,
                   group = 'Attractions') %>%
  addCircleMarkers(data=subStart, 
                   lat = ~lat, 
                   lng= ~lon, 
                   radius= sqrt(subStart$counts) * .12, 
                   color= 'Orange', 
                   group='Subscriber', 
                   weight=0,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   label=subStart$start_station_name)  %>%
  addCircleMarkers(data=subEnd, 
                   lat = ~as.numeric(lat), 
                   lng= ~as.numeric(lon), 
                   radius= sqrt(subEnd$counts) * .12, 
                   weight=0,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   color= 'Orange', 
                   group='Subscriber',
                   label=subEnd$end_station_name) %>%
  addCircleMarkers(data=nonsubStart, 
                   lat = ~as.numeric(lat), 
                   lng= ~as.numeric(lon), 
                   radius= sqrt(nonsubStart$counts) * .12, 
                   weight=0,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   color= '#0ABAB5', 
                   group=c('Non Subscriber'),
                   label=nonsubStart$start_station_name) %>%
  addCircleMarkers(data=nonsubEnd, 
                   lat = ~as.numeric(lat), 
                   lng= ~as.numeric(lon), 
                   radius= sqrt(nonsubEnd$counts) * .12, 
                   color= '#0ABAB5', 
                   group='Non Subscriber', 
                   weight=0,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   label=nonsubEnd$end_station_name) %>%
  # addCircleMarkers(data=allStart, 
  #                  lat = ~lat, 
  #                  lng= ~lon, 
  #                  radius= sqrt(allStart$counts) * .12, 
  #                  weight=0,
  #                  stroke = FALSE,
  #                  fillOpacity = 0.5,
  #                  color= 'Pink', 
  #                  group=c('All'),
  #                  label=allStart$start_station_name) %>%
  # addCircleMarkers(data=allEnd, 
  #                  lat = ~lat, 
  #                  lng= ~lon, 
  #                  radius= sqrt(allEnd$counts) * .12, 
  #                  color= 'Pink', 
  #                  group='All', 
  #                  weight=0,
  #                  stroke = FALSE,
  #                  fillOpacity = 0.5,
  #                  label=allEnd$end_station_name) %>%
  addCircleMarkers(data=business_cor, 
                   lat= ~lat, 
                   lng = ~lon, 
                   radius = 7, 
                   color = 'purple', 
                   weight=0,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   group = 'Business Center', 
                   label = business_cor$name) %>%
  addAwesomeMarkers(data=amtrak, 
                    lat= ~lat, 
                    lng = ~lon,
                    icon = awesomeIcons(
                      icon = "train",
                      library = "fa",
                      markerColor = "blue"), 
                    label = amtrak$station,
                    labelOptions = labelOptions(noHide = F,
                                                direction = "center",
                                                style = list(
                                                  "font-family" = "open sans",
                                                  "box-shadow" = "0.5px 0.5px rgba(0,0,0,0.25)",
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)")),
                    group = 'Amtrak Station') %>%
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
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)")),
                    group = 'University') %>%
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
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)")),
                    group = 'Train Station') %>%
  addAwesomeMarkers(data = subway_station, 
                    lng = ~ as.numeric(LON), 
                    lat = ~ as.numeric(LAT), 
                    icon = awesomeIcons(
                      icon = "subway",
                      library = "fa",
                      markerColor = "red"
                    ), 
                    label = subway_station$STOPNAME,
                    labelOptions = labelOptions(noHide = F,
                                                direction = "center",
                                                style = list(
                                                  "font-family" = "open sans",
                                                  "box-shadow" = "0.5px 0.5px rgba(0,0,0,0.25)",
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)")),
                    group = 'Subway Station') %>%
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
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)")),
                    group = 'Supermarket') %>%
  addCircleMarkers(data = trolley10, 
                   lng = ~ as.numeric(LON), 
                   lat = ~ as.numeric(LAT), 
                   radius = 3, 
                   weight=0,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   label = trolley10$STOPNAME,
                   group = 'Trolley 10 (Lightrail)') %>%
  addCircleMarkers(data = trolley11, 
                   lng = ~ as.numeric(LON), 
                   lat = ~ as.numeric(LAT), 
                   radius = 3, 
                   weight=0,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   label = trolley11$STOPNAME,
                   group = 'Trolley 11 (Lightrail)') %>%
  addLayersControl(overlayGroups = listOfcontrol) %>%
  hideGroup(group = listOfcontrol)


phillyMap




##################################
### weather plots  -----
##################################

# yaxis for temperature
temp_yaxis <- list(
  tickfont = list(color = "navy"),
  overlaying = "y",
  side = "right",
  title = "second y axis", 
  range = c(min(weather$Avg)-2, max(weather$Avg)+2))


# -------------
# scatter plot 1: temp and trip count
cor_temp_trip <- plot_ly(data = weather, 
                         x = ~Avg) %>%
  plotly::add_markers(y = ~count, mode = 'markers',
                      marker = list(size = 4,
                                    color = '#045a8d'),
                      text = ~date,
                      showlegend = FALSE) %>%
  add_lines(y = ~fitted(loess(count~Avg)),
            line = list(color = 'rgb(205, 12, 24)',
                        width = 2.5),
            name = 'Loess Smoother') %>%
  add_ribbons(data = loess_temp,
              ymin = ~.fitted - 1.96 * .se.fit,
              ymax = ~.fitted + 1.96 * .se.fit,
              line = list(color = 'rgba(7, 164, 181, 0.05)'),
              fillcolor = 'rgba(1, 164, 181, 0.3)',
              name = "Standard Error")  %>%
  layout(title = "Average Temperature Vs. Number of Trips",
         titlefont = list(size = 17),
         xaxis = list(title = "Average Temperature (C)"),
         yaxis = list (title = "Nunmber of Trips"),
         paper_bgcolor='rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)')
# add_annotations(
#   x= .6,
#   y= 1,
#   xref = "paper",
#   yref = "paper",
#   text = paste0("<b>Correlation:</b>", corWeatherTrip),
#   showarrow = F) %>%


# -------------
# scatter plot 2: rain and trip count
cor_rain_trip <- plot_ly(data = weather, 
                         x = ~Liquid) %>%
  plotly::add_markers(y = ~count, mode = 'markers',
                      marker = list(size = 4,
                                    color = '#045a8d'),
                      text = ~date,
                      showlegend = FALSE) %>%
  layout(title = "Liquid Precipitation (Rain and Snow) Vs. Number of Trips",
         xaxis = list(title = "Liquid Precipitation (cm)"),
         yaxis = list (title = "Nunmber of Trips"),
         paper_bgcolor='rgba(0,0,0,0)',
         plot_bgcolor = 'rgba(0,0,0,0)')



#####################################################
### popular stations  -----
#####################################################
# sStart <- df %>% 
#   dplyr::filter(riderType == 'Subscriber') %>%
#   dplyr::group_by(start_station_name) %>%
#   summarise(counts = n()) 
# 
# sEnd <- df %>% 
#   filter(riderType == 'Subscriber') %>%
#   dplyr::group_by(end_station_name) %>%
#   summarise(counts = n()) 
# 
# nEnd <- df %>% 
#   filter(riderType == 'NonSubscriber') %>%
#   dplyr::group_by(end_station_name) %>%
#   summarise(counts = n()) 
# 
# nStart <- df %>% 
#   filter(riderType == 'NonSubscriber') %>%
#   dplyr::group_by(start_station_name) %>%
#   summarise(counts = n()) 
# 
# sStart <- as.data.frame(sStart)[order(sStart$counts, decreasing = TRUE),][1:6,]
# sEnd <- as.data.frame(sEnd)[order(sEnd$counts, decreasing = TRUE),][1:6,]
# nStart <- as.data.frame(nStart)[order(nStart$counts, decreasing = TRUE),][1:6,]
# nEnd <- as.data.frame(nEnd)[order(nEnd$counts, decreasing = TRUE),][1:6,]
# 
# 
# # all popular start station
# allStart <- df %>% 
#   dplyr::group_by(start_station_name)%>%
#   dplyr::summarise(counts = n()) %>%
#   dplyr::arrange(desc(counts)) %>%
#   dplyr::slice(1:10) %>%
#   dplyr::mutate(lat = unlist(lapply(start_station_name, lookupStationLat)),
#          lon = unlist(lapply(start_station_name, lookupStationLon))) %>%
#   as.data.frame()
# 
# 
# 
# # all popular end station
# allEnd <- df %>% 
#   group_by(end_station_name)%>%
#   summarise(counts = n()) %>%
#   dplyr::arrange(desc(counts)) %>%
#   dplyr::slislice(1:10) %>%
#   dplyr::mutate(lat = unlist(lapply(end_station_name, lookupStationLat)),
#          lon = unlist(lapply(end_station_name, lookupStationLon))) %>%
#   as.data.frame()
# 
# allStart <- df %>% 
#   dplyr::group_by_(start_station_name)%>%
#   summarise(counts = n()) %>%
#   arrange(desc(counts)) %>%
#   slice(1:10) %>%
#   mutate(lat = unlist(lapply(start_station_name, lookupStationLat)),
#          lon = unlist(lapply(start_station_name, lookupStationLon))) %>%
#   as.data.frame()
# 

#####################################################
### bike utilisation  -----
#####################################################
# bike_monthly <- df %>%
#   dplyr::select_(year, month, weekday, weekday_weekend, 
#          weekofyear, date, hour, trip_id, bike_id, 
#          duration) %>%
#   dplyr::group_by(year, month) %>%
#   summarise(bikecount = length(unique(bike_id)), 
#             tripPerbike = sum(length(unique(trip_id))/length(unique(bike_id))) / length(unique(date)),
#             duration = mean(duration)) %>%
#   mutate('time'= paste0(year, ' / ', month)) %>%
#   as.data.frame()
# 
# 
# bike_utilisation <- function(df, stationName, time){
#   if (time == 'yearly'){
#     b <- df %>%
#       filter (start_station_name==stationName | end_station_name == stationName) %>%
#       group_by(year) %>%
#       summarise(count = length(unique(trip_id)),
#                 tripPerbike = sum(length(unique(trip_id))/length(unique(bike_id))) / length(unique(date)),
#                 duration = mean(duration)) %>%
#       set_colnames(c('time', 'count', 'tripPerbike', 'duration'))
#   }
#   
#   else if (time == 'monthly'){
#     b <- df %>%
#       filter (start_station_name==stationName | end_station_name == stationName) %>%
#       group_by(year, month) %>%
#       summarise(count = length(unique(trip_id)),
#                 tripPerbike = sum(length(unique(trip_id))/length(unique(bike_id))) / length(unique(date)),
#                 duration = mean(duration)) %>%
#       mutate('time'= paste0(year, ' / ', month)) %>%
#       as.data.frame()
#   }
#   
#   b
#   
# }


