#######################################################################################
#### FIT 5147 Data Visualisation
#### Visualisation Project - Indego Bike Share System
#### Ting Hsu
#### 28898702
#######################################################################################


#######################################################################################
####----------------------------------------------###
####    load libraries         
####----------------------------------------------###
#######################################################################################

### check if libraries are installed
# list.of.packages <- c('devtools', 'leaflet', 'plotly', 'DT', 'shinythemes', 'shinyjs')
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# 
# devtools::install_github("timelyportfolio/parsetR")

require(shiny)
require(shinythemes)
require(DT)
require(plotly)
require(leaflet)
require(parsetR)
require(shinyjs) 



#####################################################################################################################################
## ui
#####################################################################################################################################


ui <- navbarPage(title = " ",
                 id = 'navtab',
                 theme=shinytheme('cosmo'),
                 ### use below to overwrite current theme
                 # header = tags$head(
                 #   useShinyjs(),
                 #   tags$style(HTML(".one {background: black; color: white; font-family: Open Sans; font-color: white}"))),
                 tabPanel("Home",
                          HTML('<link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.0.13/css/all.css" integrity="sha384-DNOHZ68U8hZfKXOrtjWvjxusGo9WQnrNx2sqG0tfsghAvtVlRW3tvkXWZh58N9jp" crossorigin="anonymous">'),
                          tags$head(tags$style(HTML('
                                                    .modal.in .modal-dialog {
                                                    filter: alpha(opacity=10); 
                                                    opacity: 0.9;
                                                    border-color: 
                                                    height:150%;
                                                    width:100%;
                                                    position:absolute;
                                                    padding:5px;
                                                    top:30%;
                                                    left:0%;
                                                    }
                                                    
                                                    .modal-content {
                                                    filter: alpha(opacity=10); 
                                                    opacity: 0.9;
                                                    background-color: black;
                                                    height:150%;
                                                    width:100%;
                                                    }
                                                    
                                                    .modal-body {
                                                    filter: alpha(opacity=10); 
                                                    opacity: 1;
                                                    float: none;
                                                    margin: 0;
                                                    height:150%;
                                                    width:100%;
                                                    font-size: 42px;
                                                    color: #fff;
                                                    font-weight: 300;
                                                    text-shadow: none;
                                                    font-opacity: 1;
                                                    }
                                                    
                                                    '))),
                          
                          br(),
                          absolutePanel( 
                            fixed = TRUE,
                            draggable = F, 
                            top = 70, 
                            left = 480, 
                            right = "auto", 
                            bottom = "auto",
                            width = 1000, 
                            height = "auto",
                            style = "opacity: .7 ; z-index: 1000",
                                   HTML('<h3><strong><left><span class = "fa fa-bicycle"></span>&emsp;Ready to start the journey?</left></strong></h3>'),
                                   br(),
                                   HTML('<p style="line-height:1.6em; font-size:1.1em; text-align:left"><strong> Historial trip data from 2016 and 2017 are used in the project to reveal trends, travel patterns, and factors that influence ridership and station popularity.</strong> As the leading equitable bike share system in the United States, Indego has proven to be successful in attracting new customers and achieving a wide reach (Sisson, 2017). Launched in 2015, Indego has doubled its capacity in 2 years. The number of bikes grew from 600 to 1,200 while the number of stations increased from 60 to 126 by the end of 2017 (Bouffard, 2017). Data is a main driving force behind Indego’s continuous growth and business decisions. Several government and private organisations such as City of Philadelphia, Bike Coalition of Greater Philadelphia, and Bike Transit Systems conduct both quantitative and qualitative analysis regularly to measure results. 
                                        Quarterly anonymised trip data is also published on Indego’s website for download. Indego encourages the public to use open-source data to conduct data exploration on their own and share any discoveries with the organisation.<strong>What can we uncover after combining trip data with historical weather, geographical information, and local events? </strong></left></p>')
                                  ),
                          absolutePanel( 
                            fixed = TRUE,
                            draggable = F, 
                            top = 500, 
                            left = 480, 
                            right = "auto", 
                            bottom = "auto",
                            width = 1000, 
                            height = "auto",
                            style = "opacity: .7 ; z-index: 1000",
                            HTML('<p style="font-size:1em"><strong><span class = "fas fa-database"></span>&emsp;Data sources:</strong></p>'),
                            br(),
                            HTML('<p style="line-height:1.7em">Indego Trip Data 2016 - 2017 and Bike Station Coordinates by Indego : https://bit.ly/2vxi5ym<br>Weather Data of Philadelphia 2016 - 2017 by Franklin Institute : https://bit.ly/2HBj2KW 
                                 <br> Attractions Locations of Phiadelphi by Visit Philly : https://vstphl.ly/2qM0tu4 <br> Public Transit Station Location by Southeastern Pennsylvania Transportation Authority (SEPTA) : https://bit.ly/2HPfdzl</p>')
                          ),
                          absolutePanel( 
                            fixed = TRUE,
                            draggable = F, 
                            top = "auto", 
                            left = 480, 
                            right = "auto", 
                            bottom = 20,
                            width = 1000, 
                            height = "auto",
                            style = "opacity: .7 ; z-index: 1000",
                            HTML('<p style="font-size:1em; text-align:left"><right><strong><span class = "fas fa-book"></span>&emsp;References:</strong></p>'),
                            HTML('<p style="text-align:left">Bouffard, M. (2017, April 25). Indego 2 Years In: Lessons from Bike Share. Retrieved from http://www.greenphillyblog.com/philly-biking/indego-2-years-lessons-bike-share/</p>'),
                            HTML('<p style="text-align:left">Sisson, P. (2017, April 18). Cycling success: 10 U.S. cities pushing biking forward. Retrieved from https://www.curbed.com/2017/4/18/15333796/best-cities-bike-commute-us-cycling</p>')
                          ),
                          absolutePanel( 
                            fixed = TRUE,
                            draggable = F, 
                            top = 70, 
                            left = 50, 
                            right = "auto", 
                            bottom = "auto",
                            width = 400, 
                            height = "auto",
                            style = "opacity: .8 ; z-index: 1000",
                            HTML('<p style="font-size:5em; line-height: 1.2em"><strong>Indego<br>Bike<br>Share</strong></p>'),
                            HTML('<div style="width:235px;height:10px;background-color:black;opacity:0.8"></div>'),
                            br(),
                            HTML('<p style="font-size:1.2em"><strong>of Philadelphia</strong></p>'),
                            HTML('<p style="font-size:3.2em"><strong>2016 - 2017<br> Trip Analysis</strong></p>')
                          ),
                          absolutePanel( 
                            fixed = TRUE,
                            draggable = F, 
                            top = "auto", 
                            left = 50, 
                            right = "auto", 
                            bottom = 20,
                            width = 400, 
                            height = "auto",
                            style = "opacity: .8 ; z-index: 1000",
                            HTML('<p style="font-size:.9em; font-color:gray">Indego Bike Share 2016-2017 by Ting Hsu <br> tinghsuanhsu@gmail.com</p>')
                          )
                          
                          
                          ),
                 tabPanel("Trip Count",
                          HTML('<link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.0.13/css/all.css" integrity="sha384-DNOHZ68U8hZfKXOrtjWvjxusGo9WQnrNx2sqG0tfsghAvtVlRW3tvkXWZh58N9jp" crossorigin="anonymous">'),
                          sidebarLayout(
                            sidebarPanel(
                              width = 2,
                              selectInput('tripcountY',
                                          'Year',
                                          choices = c('All', '2016','2017')
                              ),
                              selectInput('tripcountM',
                                          'Month',
                                          choices = c('All', '1','2', '3','4', '5','6', '7','8', '9','10', '11','12')
                              ),
                              br(),
                              br(),
                              htmlOutput('summaryMainTitle1'),
                              HTML('<p align ="left"; style = "font-size:12px">Subscriber</p>'),
                              htmlOutput('subTripCount'),
                              HTML('<p align ="left"; style = "font-size:12px">Non-Subscriber</p>'),
                              htmlOutput('nonTripCount'),
                              br(),
                              br(),
                              htmlOutput('summaryMainTitle2'),
                              HTML('<p align ="left"; style = "font-size:12px">Subscriber</p>'),
                              htmlOutput('subTripTime'),
                              HTML('<p align ="left"; style = "font-size:12px">Non-Subscriber</p>'),
                              htmlOutput('nonTripTime')
                              # ('<font-family:open sans><right><h2>49281</h2></right></font>')
                            ),
                            mainPanel(
                              h2("How many trips happen at a certain time? How long are the trips?"),
                              absolutePanel( 
                                fixed = TRUE,
                                draggable = F, 
                                top =  140, 
                                left = "auto", 
                                right = 185, 
                                bottom = "auto",
                                width = 1080, 
                                height = "auto",
                                style = "opacity: .8 ; z-index: 1000",
                                p('Indego experiences a 20% increase in trips from 2016 to 2017 while 85% of the trips come from subscribers. The number of trips fluctuates over time as ridership is affected by season and local events. Generally, the number of trips starts increasing in the spring time and continues to grow until winter hits. Three key events contribute to the apparent spikes and dips - SEPTA strike, yearly NFL draft, and heavy snow storm. During the SEPTA public transport strike in November 2016, Indego becomes the main transportation method to get around the city. 2017 NFL draft is hosted at Philadelphia Museum of Art which is in proximity to 7 Indego stations. 
                                  In terms of trip duration, subscribers usually stay on the bikes for less than 20 minutes while non-subscribers ride for about 45 minutes. This difference indicates that riders use Indego for different purposes.')
                              ),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              fluidRow(
                                column(
                                  width = 7, 
                                  offset = 0,
                                  plotlyOutput('tripcountPlot', width = '700px')
                                ),
                                column(
                                  width = 4,
                                  plotlyOutput('durationPlot', width = '600px')
                                )
                              )
                              )
                          )
                 ),
                 tabPanel("Travel Pattern",
                          HTML('<link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.0.13/css/all.css" integrity="sha384-DNOHZ68U8hZfKXOrtjWvjxusGo9WQnrNx2sqG0tfsghAvtVlRW3tvkXWZh58N9jp" crossorigin="anonymous">'),
                          sidebarLayout(
                            sidebarPanel(
                              width = 2,
                              selectInput('travcountY',
                                          'Year',
                                          choices = c('All', '2016','2017')
                              ),
                              selectInput('travcountM',
                                          'Month',
                                          choices = c('All', '1','2', '3','4', '5','6', '7','8', '9','10', '11','12')
                              ),
                              checkboxInput('hourlyCheck',
                                            'Show hourly breakdown',
                                            FALSE),
                              conditionalPanel(
                                'input.hourlyCheck == true',
                                tags$head(tags$style(HTML('.panel{background:white;}'))),
                                absolutePanel( 
                                  id = 'controls',
                                  class = "panel panel-default", 
                                  fixed = TRUE,
                                  draggable = TRUE, 
                                  top = 60, 
                                  left = "auto", 
                                  right = 20, 
                                  bottom = "auto",
                                  width = 400, 
                                  height = "auto",
                                  style = "opacity: .7 ; z-index: 1000",
                                  plotlyOutput('hourly', width = '350px', height = '250px'))),
                              checkboxInput('peakCheck',
                                            'Show peak and non-peak hour',
                                            FALSE),
                              htmlOutput('patternMainTitle1'),
                              HTML('<p align ="left"; style = "font-size:12px">Subscriber</p>'),
                              htmlOutput('subWday'),
                              HTML('<p align ="left"; style = "font-size:12px">Non-Subscriber</p>'),
                              htmlOutput('nonWday'),
                              br(),
                              br(),
                              htmlOutput('patternMainTitle2'),
                              HTML('<p align ="left"; style = "font-size:12px">Subscriber</p>'),
                              htmlOutput('subWend'),
                              HTML('<p align ="left"; style = "font-size:12px">Non-Subscriber</p>'),
                              htmlOutput('nonWend')
                            ), 
                            mainPanel(
                              h2("How do subscribers and non-subscribers behave differently?"),
                              absolutePanel( 
                                fixed = TRUE,
                                draggable = F, 
                                top =  140, 
                                left = "auto", 
                                right = 185, 
                                bottom = "auto",
                                width = 1080, 
                                height = "auto",
                                style = "opacity: .8 ; z-index: 1000",
                                p('Subscribers use Indego mainly on weekdays during peak hour. Subscribers most likely use the bikes as part of the commute as there are two spikes at peak times and their average travel time is less than 20 minutes. On the other hand, non-sbuscribers travel primarily from 10 am to 5pm while the most amount of trips happen at around 3 pm. As they also spend on average 40 minutes on the bikes, we can safely assume that non-subscribers use the bike for touring purposes.')
                              ),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              fluidRow(
                                column(
                                  width = 6, 
                                  offset = 0,
                                  plotlyOutput('heatmapSub', width = '500px')
                                ),
                                column(
                                  width = 4,
                                  offset = 0,
                                  plotlyOutput('heatmapNon', width = '500px')
                                )
                              )
                            )
                          )
                 ),
                 tabPanel("Weather",
                          sidebarLayout(
                            sidebarPanel(
                              width = 2,
                              radioButtons('weatherRadio',
                                           h5('Choose type:'),
                                           choices = list('Temperature',
                                                          'Percipitation')
                              )
                            ),
                            mainPanel(
                              h2("Does weather affect ridership?"),
                              absolutePanel( 
                                fixed = TRUE,
                                draggable = F, 
                                top =  140, 
                                left = "auto", 
                                right = 185, 
                                bottom = "auto",
                                width = 1080, 
                                height = "auto",
                                style = "opacity: .8 ; z-index: 1000",
                                p('As cycling is an outdoor activity, it could be affected by weather. Comparing average daily temperature and daily trip count, the scatter plot demonstrates strong correlation. The correlation coefficient of these two variables is 0.73. The Loess Smoother shows a more complete story. The maximum number of trips happen roughly at 18-25 C. Once the temperature rises above 28, the number of trips decreases. At the same time, the number of trips are low when temperature drops below 0. This finding explains how weather affects ridership.')
                              ),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              br(),
                              plotlyOutput('weatherPlot', width = '100%', height = "500px")
                            )
                          )
                 ),
                 tabPanel('Station and Route',
                          tabsetPanel(type = "tabs",
                                      tabPanel("Station", 
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   width = 2, 
                                                   selectInput('stationY',
                                                               'Year',
                                                               choices = c('', 'All', '2016','2017'),
                                                               selected = ''
                                                   ),
                                                   selectInput('stationM',
                                                               'Month',
                                                               choices = c('','All', '1','2', '3','4', '5','6', '7','8', '9','10', '11','12'),
                                                               selected = ''
                                                   ),
                                                   selectInput('riderType',
                                                               'Rider Type',
                                                               choices = c('', 'All', 'Subscriber', 'Non-Subscriber'),
                                                               selected = ''
                                                   ),
                                                   actionButton("go", "Go!", icon("fa fa-play"), width = '180px')
                                                 ),
                                                 mainPanel(
                                                   h2('What are the top 10 popular stations?'),
                                                   absolutePanel( 
                                                     fixed = TRUE,
                                                     draggable = F, 
                                                     top =  190, 
                                                     left = "auto", 
                                                     right = 185, 
                                                     bottom = "auto",
                                                     width = 1080, 
                                                     height = "auto",
                                                     style = "opacity: .8 ; z-index: 1000",
                                                     p('Where do our riders ride their bikes? Subscribers mostly travel in the West side of Philadelphia. Upon adding surrounding locations to the map, we can see that popular stations are close to public transport stations, business buildings, and universities. Local supermarkets are also in proximity with the stations in the South and the North. The alignment of stations and these locations points out that they typically use the bikes to extend their commute and for short trips to run errands.
                                                     Contrary to subscribers, non-subscribers mainly travel in the Centre and the East side. Numerous local tourist attractions are close to the bike stations, especially the ones in the East. Combining this observation with the long travel time, we can say taht non-subcribers mostly ride the bikes to visit attractions in Philadelphia.')
                                                   ),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   leafletOutput('riderMap', width = '120%', height = "480px"))
                                                 )
                                      ),  
                                      tabPanel("Route", 
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   width = 2, 
                                                   selectInput('routeY',
                                                               'Year',
                                                               choices = c('', 'All', '2016','2017')
                                                   ),
                                                   selectInput('routeM',
                                                               'Month',
                                                               choices = c('','All', '1','2', '3','4', '5','6', '7','8', '9','10', '11','12')
                                                   ),
                                                   actionButton('go2', 'Go', icon("fa fa-play"), width = '180px')
                                                 ),
                                                 mainPanel(
                                                   h2('What are the top 10 popular routes?'),
                                                   absolutePanel( 
                                                     fixed = TRUE,
                                                     draggable = F, 
                                                     top =  190, 
                                                     left = "auto", 
                                                     right = 185, 
                                                     bottom = "auto",
                                                     width = 1080, 
                                                     height = "auto",
                                                     style = "opacity: .8 ; z-index: 1000",
                                                     p('There are 13796 different routes from 2016 to 2017. Most of the popular routes are in the Northwest part of Philadelphia. Do subscribers and non-subscribers have different route choices?
                                                     The all-time top 10 popular routes for non-subscribers are all singe-station routes. On the other hand, subscribers have a wide variety of route choices. 
                                                       This obersation explains why riders use Indego bike. Subscribers use the bikes go from one place to another while non-subscribers tend to linger around one location for a longer period of time. Considering their travel time and travel patterns, we can say that non-subscribers are visitors and mainly use the bike for touring purposes')
                                                   ),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   br(),
                                                   fluidRow(
                                                     column(
                                                       width = 6, 
                                                       offset = 0,
                                                       HTML('<h4>Subscriber</h4>'),
                                                       parsetOutput('route_sub', width = '500px')
                                                     ),
                                                     column(
                                                       width = 5, 
                                                       offset = 0,
                                                       HTML('<h4>Non-subscriber</h4>'),
                                                       parsetOutput('route_non', width = '500px')
                                                     )
                                                   )
                                                   )
                                      )
                                      )
                                      )
                          ),
                 tabPanel('Explore Yourself',
                          h2('Dig in! Play around with the map.'),
                          p('Go over to the upper right corner of the map, you will see a list of available choices. Tick on boxes to see how surrounding locations affect the top 10 all-time popular for both subscribers and non-subscribers. What can you find after putting markers on the map? What other geographical factors might influence the popuarity of a station?'),
                          leafletOutput('philly', width = '100%', height = "600px")
                 ),
                 tabPanel('Data',
                          sidebarLayout(
                            sidebarPanel(
                              width = 2,
                              selectInput('dataType',
                                          'Choose rider type',
                                          choices = list('All', 'Subscriber', 'Non-subscriber'),
                                          selected = 'All'),
                              selectInput('dataYear',
                                          'Choose year',
                                          choices = list('All', '2016', '2017'),
                                          selected = 'All'),
                              checkboxInput('changeFilename',
                                            'Change File Name',
                                            FALSE),
                              conditionalPanel(
                                'input.changeFilename == true',
                                textInput("filename", "Filename")
                              ),
                              downloadButton('downloadbutton', 'Download Data', icon = icon('download'))
                            ),
                            mainPanel(
                              h1("Want to do your own analysis? Download the dataset."),
                              br(),
                              DT::dataTableOutput('table1')
                            )
                          )
                 )
                 )
