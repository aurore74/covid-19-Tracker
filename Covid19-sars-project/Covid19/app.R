library(shiny)
library(shinydashboard)
library(shinyjs)
library(cli)
library(data.table)
library(tidyr)
library(shinythemes)
library(DT)

library(plotly)

library(leaflet)
library(leaflet.extras)
library(leaflet.providers)

library(rgdal)


#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="DATA/world_shape_file.zip")
#system("unzip DATA/world_shape_file.zip")
print(getwd())
world_spdf <- readOGR( 
  dsn= "./DATA", 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)
#print(world_spdf)

# Clean the data object
library(dplyr)
world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)


ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "COVID-19 Tracker"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Global Statistics", tabName="GS", icon=icon("chart-bar")),
                        menuItem("Trends", tabName="Trends", icon=icon("chart-line")),
                        menuItem("World Wide", tabName="WW", icon=icon("globe")),
                        menuItem("Database", tabName="DB", icon=icon("database")),
                        menuItem("About this app", tabName = "ATA", icon = icon("info-circle"))
                      )
                    ),
                    
                    dashboardBody(
                      fluidRow(
                        tags$head(
                          tags$link(rel = "stylesheet", type = "text/css", href = "style5.css"),
                        ),
                        
                        tabItems(
                          tabItem(tabName="GS",
                                  div(
                                    div(
                                      div(
                                        selectizeInput('choice1', "Country", choices = NULL, options = list(placeholder = 'Country'))
                                      ),
                                      div(
                                        selectizeInput('choice2', "Month", choices = NULL, options = list(placeholder = 'Month'))
                                      ),
                                      div(
                                        selectizeInput('choice3', "Day", choices = NULL, options = list(placeholder = 'Day'))
                                      ),
                                      class="choice_div"
                                    ),
                                    
                                    div(
                                      div(
                                        valueBoxOutput("vbox1"),
                                        class="valuebx"
                                      ),
                                      div(
                                        valueBoxOutput("vbox2"),
                                        class="valuebx"
                                      ),  
                                      div(
                                        valueBoxOutput("vbox3"),
                                        class="valuebx"
                                      ),
                                      div(
                                        valueBoxOutput("vbox4"),
                                        class="valuebx"
                                      ),
                                      class="inner_div"
                                    ),
                                    
                                    div(
                                      div(
                                        div(
                                          div(
                                            valueBoxOutput("vbox5"),
                                            class="valuebx"
                                          ),
                                          div(
                                            valueBoxOutput("vbox6"),
                                            class="valuebx"
                                          ),  
                                          div(
                                            valueBoxOutput("vbox7"),
                                            class="valuebx"
                                          ), 
                                          class="inner_div",
                                        ),
                                        
                                        div(
                                          div(
                                            div(
                                              div(
                                                selectizeInput('choice4', NULL, choices = c("Total Cases by Continent", "Highest Total Cases by Country"), options = list(placeholder = 'Type')),
                                                div(
                                                  plotlyOutput("plot_g1", width="97%", height="90%"),
                                                  class="cont_div2"
                                                ),
                                                class="sub_div"
                                              ),
                                              div(
                                                selectizeInput('choice5', NULL, choices = c("Total Active Cases by Continent", "Highest Total Active Cases by Country"), options = list(placeholder = 'Type')),
                                                div(
                                                  plotlyOutput("plot_g2", width="97%", height="90%"),
                                                  class="cont_div2"
                                                ),
                                                class="sub_div"
                                              ),
                                              class="inner_d2 inner_d3_d1"
                                            ),
                                            
                                            div(
                                              div(
                                                selectizeInput('choice6', NULL, choices = c("Total Deaths by Continent", "Total Cases/Total Deaths by Continent", "Highest Total Deaths by Country"), options = list(placeholder = 'Type')),
                                                div(
                                                  plotlyOutput("plot_g3", width="97%", height="95%"),
                                                  class="cont_div3"
                                                ),
                                                class="sub_div inner_d3_d2"
                                              ),
                                              div(
                                                selectizeInput('choice7', NULL, choices = c("Total Recovered by Continent", "Total Cases/Total Recovered by Continent", "Highest Total Recovered by Country"), options = list(placeholder = 'Type')),
                                                div(
                                                  plotlyOutput("plot_g4", width="100%", height="95%"),
                                                  class="cont_div3"
                                                ),
                                                class="sub_div inner_d3_d2"
                                              ),
                                              style="width: 50%;",
                                              class="control"
                                            ),
                                            class="inner_d2 inner_d3",
                                            style="width: 100%; display: flex; justify-content: center;"
                                          ),
                                          class="choice_result_div"
                                        ),
                                        class="interractive" 
                                      ),
                                    ),
                                  style="width: 100%; height: 93% !important; overflow-y: scroll; background-color: white; display: flex; flex-direction: column;" 
                                ),
                          ),
                          
                          tabItem(tabName="Trends",
                                  div(
                                    div(
                                      plotlyOutput("plot_g5", width="97%", height="300px"),
                                      style="width: 95%; margin: auto; margin-top: 3vh; height: 50vh;"
                                    ),
                                    div(
                                      plotlyOutput("plot_g6", width="97%", height="300px"),
                                      style="width: 95%; margin: auto; margin-top: 3vh; height: 50vh;"
                                    ),
                                    div(
                                      plotlyOutput("plot_g7", width="97%", height="300px"),
                                      style="width: 95%; margin: auto; margin-top: 3vh; height: 50vh;"
                                    ),
                                    div(
                                      plotlyOutput("plot_g8", width="97%", height="300px"),
                                      style="width: 95%; margin: auto; margin-top: 3vh; height: 50vh;"
                                    ),
                                    style="width: 100%; height: 93% !important; padding-top: 5vh; overflow-y: scroll; background-color: white; display: flex; flex-direction: column;" 
                                  )
                          ),
                          
                          tabItem(tabName="WW",
                                  div(
                                    leafletOutput("mymap", height="93vh"),
                                    style="width: 100%; height: 93% !important; overflow-y: scroll; background-color: white; display: flex; flex-direction: column;" 
                                  )
                          ),
                          
                          tabItem(tabName="DB",
                                  div(
                                    tabsetPanel(
                                      tabPanel("Worldometer Data", 
                                                div(
                                                  dataTableOutput("datatable1"),
                                                  style="width: 100%; padding-top: 3vh; overflow-x: scroll;"
                                                )
                                      ),
                                      
                                      tabPanel("Full_grouped Data", 
                                               div(
                                                 dataTableOutput("datatable2"),
                                                 style="width: 100%; padding-top: 3vh;  overflow-x: scroll;"
                                               )
                                      ),
                                      
                                      tabPanel("Download Data", 
                                               div(
                                                 downloadLink("download_data1", "Wordometer Data", style="text-decoration: underline; margin-bottom: 33px;"),
                                                 tags$br(),
                                                 tags$br(),
                                                 downloadLink("download_data2", "Full Grouped Data", style="text-decoration: underline;"),
                                                 style="width: 100%; padding: 1% 2.5%; display: block;"
                                               )
                                      )
                                    ),
                                    style="width: 100%; height: 93% !important; padding: 1% 3%; overflow-y: scroll; background-color: white; display: flex; flex-direction: column;" 
                                  )
                          ),
                          
                          tabItem(tabName="ATA",
                                  div(
                                    div(
                                      p(tags$b(h3("ABOUT US"))),
                                      style="margin-bottom: 8px;"
                                    ),
                                    div(
                                      p("The COVID 19 pandemic has had a major impact on the lives of people worldwide.  The pandemic has resulted in millions of deaths worldwide and has affected the health of many people. For these reasons, we decided to develop an application that would use the 2020 Covid datasets to visualize the number of confirmed, deceased, and recurred cases on a daily basis worldwide. The key points of visualization would be: 
                                          The number of deaths, confirmed cases, active cases, and cured cases. 
                                          Through this data, we can understand the impact of the pandemic and develop strategies and make decisions that can help save lives and reduce the impact of the disease. 
                                          We can prevent a Corona-like pandemic in the near future and 
                                          take swift action against it."),
                                      p(tags$b(h3("Sources"))),
                                      tags$ul(
                                        tags$li(
                                          tags$a(href="https://www.kaggle.com/datasets/imdevskp/corona-virus-report?resource=download&select=worldometer_data.csv", "https://www.kaggle.com/datasets/imdevskp/corona-virus-report?resource=download&select=worldometer_data.csv")
                                        ),
                                        tags$li(
                                          tags$a(href="https://mastering-shiny.org/", "https://mastering-shiny.org/")
                                        ),
                                        tags$li(
                                          tags$a(href="https://shiny.rstudio.com/gallery/covid19-tracker.html", "https://shiny.rstudio.com/gallery/covid19-tracker.html")
                                        ),
                                        tags$li(
                                          tags$a(href="https://shiny.rstudio.com/gallery/nz-trade-dash.html", "https://shiny.rstudio.com/gallery/nz-trade-dash.html")
                                        )
                                      ),
                                      p(tags$b(h3("Code"))),
                                      p("The code is on Github"),
                                      p(tags$b(h3("Data"))),
                                      p("
                                        The data used here comes from COVID-19 Dataset 'Number of 
                                        Confirmed, Death and Recovered cases every day across the globe and were collected from Kaggle 
                                        .The author Devakumar K. P. has retrieved essential data from Covid -19 Pandemic (Year 
                                        2020), which will help us to analyze the trend of Corona pandemic in this year worldwide, so that we can respond quickly to similar pandemic cases")
                                    ),
                                    style="width: 100%; height: 93% !important; padding: 0.5% 3%; overflow-y: scroll; background-color: white; display: flex; flex-direction: column;" 
                                  )
                          )
                        )
                      )
                    )
)


server <- function(input, output, session) {

  #covid-19 data load
  mydata <- fread("worldometer_data.csv")
  
  
  ###------------World Wide--------------------
  #Creation of Id to use for keeping same order after merge
  world_spdf@data$id  <- 1:nrow(world_spdf@data)
  
  #merge of spatial data and our covid-19 data
  finaldata <- merge(world_spdf@data, mydata, by.x="NAME", by.y="Country/Region", all.x=TRUE, sort=FALSE)
  
  #reordering
  finaldata <- finaldata[order(finaldata$id), ]
  
  #removing na cells from specific columns
  finaldata["LON"][is.na(finaldata["LON"])] <- 0
  finaldata["LAT"][is.na(finaldata["LAT"])] <- 0
  finaldata["TotalCases"][is.na(finaldata["TotalCases"])] <- 0
  finaldata["TotalDeaths"][is.na(finaldata["TotalDeaths"])] <- 0
  
  #change data type of column
  finaldata$TotalCases <- as.integer(finaldata$TotalCases)
  print(typeof(finaldata$TotalCases))
  
  #text to show on popup
  mytext <- paste(
    "Country: ", finaldata$NAME, "<br/>", 
    "Area: ", finaldata$AREA, "<br/>",   
    "Population: ", round(finaldata$Population, 2), "<br/>",  
    "Total Cases: ", finaldata$TotalCases, "<br/>",   
    "Total Active Cases: ", finaldata$ActiveCases, "<br/>",
    "Total Tests: ", finaldata$TotalTests, "<br/>", 
    "%(Cases/Tests): ", (finaldata$TotalCases/finaldata$TotalTests)*100, "<br/>", 
    "Total Recovered: ", finaldata$TotalRecovered, "<br/>",  
    sep="") %>%
    lapply(htmltools::HTML)
  
  mytext2 <- paste(
    "Country: ", finaldata$NAME, "<br/>", 
    "Area: ", finaldata$AREA, "<br/>",  
    "Population: ", round(finaldata$Population, 2), "<br/>",  
    "Total Deaths: ", finaldata$TotalDeaths, "<br/>",    
    "Critical: ", finaldata$Critical, "<br/>",   
    sep="") %>%
    lapply(htmltools::HTML)
  
  #bins for map legend and color palettes
  mybins <- c(0, 1000, 10000, 50000, 100000, 500000, 1000000, 2000000, Inf)
  mypalette <- colorBin(palette="Reds", domain=world_spdf@data$POP2005, na.color="transparent", bins=mybins) #YlOrBr
  mypalette2 <- colorBin(palette="Blues", domain=world_spdf@data$POP2005, na.color="transparent", bins=mybins) #YlOrBr
  
  #map output
  output$mymap <- renderLeaflet({
    leaflet(finaldata) %>% 
      addTiles()  %>% 
      setView(lat=25, lng=0, zoom=2.25) %>%
      addPolygons(
        data=world_spdf,
        group="Global",
        fillColor = ~mypalette(finaldata$TotalCases),
        stroke = FALSE, 
        fillOpacity = 0.5, 
        smoothFactor = 0.5, 
        color = "white",
        weight=0.3
      ) %>%
      addPolygons(
        data=world_spdf,
        group="Global with Overlay",
        fillColor = ~mypalette(finaldata$TotalCases),
        stroke = FALSE, 
        fillOpacity = 0.5, 
        smoothFactor = 0.5, 
        color = "white",
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight"="normal", padding="3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addCircleMarkers(
        ~finaldata$LON, 
        ~finaldata$LAT,
        group="Deaths > 1",
        radius = ~ifelse(finaldata$TotalDeaths>0, 7, 0),
        color = ~mypalette(finaldata$TotalDeaths),
        fillColor = "#ffe56f",
        stroke = TRUE, 
        fillOpacity = 0.5,
        label = mytext2,
        labelOptions = labelOptions( 
          style = list("font-weight"="normal", padding="3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLayersControl(
        overlayGroups = c("Global", "Global with Overlay", "Deaths > 1"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(
        pal=mypalette, 
        values=~POP2005, 
        opacity=0.9, 
        title="Total Deaths", 
        position = "bottomright"
      ) %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Locate Me",
        onClick = JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  ###------------END World Wide----------------------
  
  
  ###------------Global Statistics-------------------
  #data load
  mydata2 <- fread("full_grouped.csv")
  
  ## Data Conversion
  #Extract years from dataset
  liste_y <- unique(mydata2$Date)
  liste_y <- format(liste_y, "%Y")
  
  #Extract months from dataset
  liste_m2 <- unique(mydata2$Date)
  liste_m3 <- as.Date(liste_m2)
  print("liste_m3")
  print(liste_m3)
  print("liste_m2")
  print(liste_m2)
  liste_m <- months(liste_m3)
  print("liste_m")
  print(liste_m)
  liste_m2 <- format(liste_m2, "%m")
  print(liste_m2)
  
  #Extract days from dataset
  liste_j <- unique(mydata2$Date)
  liste_j <- format(liste_j, "%d")
  
  #add these new columns to our dataset
  mydataF <- data.table(mydata2, liste_y, liste_m, liste_m2, liste_j)
  
  #countries
  countries <- reactive({
    input$choice1
  })
  #months
  month <- reactive({
    input$choice2
  })
  #days
  day <- reactive({
    input$choice3
  })
  
  #initialize selectors
  observe({
    grouped_by_country <- mydataF %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `Country/Region`) %>% group_by(`Country/Region`) %>% summarise_each(list(sum))
    #select countries
    countries <- unique(unlist(grouped_by_country$`Country/Region`, use.names = FALSE))
    
    updateSelectizeInput(session, 'choice1', choices = c("All Countries", countries), selected = "All Countries", server=TRUE)
    
    #select months
   
    updateSelectizeInput(session, 'choice2', choices = c("All Months", unique(liste_m)), selected = "All Months", server=TRUE)
    
    #select days
    updateSelectizeInput(session, 'choice3', choices = c("All Days", sort(unique(liste_j), decreasing=FALSE)), selected = "All Days", server=TRUE)
  })
  
  #on month change, update day selector
  observeEvent(month(), {
    liste_d <- mydataF %>% filter(liste_m == month()) %>% select(liste_j)
    print(unique(liste_j))
    
    updateSelectizeInput(session, 'choice3', choices = c("All Days", sort(unique(liste_j), decreasing=FALSE)), selected = "All Days", server=TRUE)
  })
  
  
  #KPI's
  res <- reactive({
    if(countries() == "All Countries" & month() == "All Months" & day() == "All Days"){
      data <- mydataF %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `Country/Region`) %>% group_by(`Country/Region`) %>% summarise_each(list(sum))
    }
    else if(countries() == "All Countries" & month() == "All Months" & day() != "All Days"){
      data <- mydataF %>% filter(liste_j==day()) %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `Country/Region`)%>% group_by(`Country/Region`) %>% summarise_each(list(sum))
    }
    else if(countries() == "All Countries" & month() != "All Months" & day() == "All Days"){
      data <- mydataF %>% filter(liste_m==month()) %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `Country/Region`)%>% group_by(`Country/Region`) %>% summarise_each(list(sum))
    }
    else if(countries() == "All Countries" & month() != "All Months" & day() != "All Days"){
      data <- mydataF %>% filter(liste_m==month() & liste_j==day()) %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `Country/Region`)%>% group_by(`Country/Region`) %>% summarise_each(list(sum))
    }
    else if(countries() != "All Countries" & month() == "All Months" & day() == "All Days"){
      data <- mydataF %>% filter(`Country/Region`==countries()) %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `Country/Region`)%>% group_by(`Country/Region`) %>% summarise_each(list(sum))
    }
    else if(countries() != "All Countries" & month() == "All Months" & day() != "All Days"){
      data <- mydataF %>% filter(`Country/Region`==countries() & liste_j==day()) %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `Country/Region`)%>% group_by(`Country/Region`) %>% summarise_each(list(sum))
    }
    else if(countries() != "All Countries" & month() != "All Months" & day() == "All Days"){
      data <- mydataF %>% filter(`Country/Region`==countries() & liste_m==month()) %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `Country/Region`)%>% group_by(`Country/Region`) %>% summarise_each(list(sum))
    }
    else if(countries() != "All Countries" & month() != "All Months" & day() != "All Days"){
      data <- mydataF %>% filter(`Country/Region`==countries() & liste_m==month() & liste_j==day()) %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `Country/Region`) %>% group_by(`Country/Region`) %>% summarise_each(list(sum))
    }
    print(data)
    data
  })
  
  #kpi's display
  output$vbox1 <- renderValueBox({
    valueBox(sum(res()$Confirmed), subtitle = "Total Cases", icon=icon("user"), color="orange")
  })
  
  output$vbox2 <- renderValueBox({
    valueBox(sum(res()$Deaths), subtitle = "Total Deaths", icon=icon("user"), color="red")
  })
  
  output$vbox3 <- renderValueBox({
    valueBox(sum(res()$Recovered), subtitle = "Total Recovered", icon=icon("user"), color="lime")
  })
  
  output$vbox4 <- renderValueBox({
    valueBox(sum(res()$Active), subtitle = "Active Cases", icon=icon("user"), color="purple")
  })
  
  output$vbox5 <- renderValueBox({
    valueBox(sum(res()$`New cases`), subtitle = "New Cases", icon=icon("user"), color="fuchsia")
  })
  
  output$vbox6 <- renderValueBox({
    valueBox(sum(res()$`New deaths`), subtitle = "New Deaths", icon=icon("user"), color="red")
  })
  
  output$vbox7 <- renderValueBox({
    valueBox(sum(res()$`New recovered`), subtitle = "New Recovered", icon=icon("user"), color="lime")
  })
  
  
  #COVID-19 Evolution by Country
  #choice4
  choice4 <- reactive({
    input$choice4
  })
  choice5 <- reactive({
    input$choice5
  })
  choice6 <- reactive({
    input$choice6
  })
  choice7 <- reactive({
    input$choice7
  })
  
  res4 <- reactive({
    data <- mydataF %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `WHO Region`) %>% group_by(`WHO Region`) %>% summarise_each(list(sum))
    data
  })
  
  res5 <- mydataF %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `Country/Region`) %>% group_by(`Country/Region`) %>% summarise_each(list(sum)) %>% arrange(desc(Confirmed)) %>% slice(1:5)
  res6 <- mydataF %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `Country/Region`) %>% group_by(`Country/Region`) %>% summarise_each(list(sum)) %>% arrange(desc(Deaths)) %>% slice(1:5)
  res7 <- mydataF %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `Country/Region`) %>% group_by(`Country/Region`) %>% summarise_each(list(sum)) %>% arrange(desc(Active)) %>% slice(1:5)
  res8 <- mydataF %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `Country/Region`) %>% group_by(`Country/Region`) %>% summarise_each(list(sum)) %>% arrange(desc(Recovered)) %>% slice(1:5)
  res9 <- mydataF %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `WHO Region`) %>% group_by(`WHO Region`) %>% summarise(ration=sum(Confirmed)/sum(Deaths))
  res10 <- mydataF %>% select(Confirmed, Deaths, Recovered, Active, `New cases`, `New deaths`, `New recovered`, `WHO Region`) %>% group_by(`WHO Region`) %>% summarise(ration=sum(Confirmed)/sum(Recovered))
  
  #PLOT 1
  output$plot_g1 <- renderPlotly({
    if(choice4() == "Total Cases by Continent"){
      res4() %>% 
        plot_ly(
          x = ~`WHO Region`,
          y = ~Confirmed,
          name = "Total Cases by Continent",
          type = "bar",
          text = ~paste("\n", Confirmed),
          textposition = "outside",
          marker = list(color = "red",
                        line = list(color = "rgb(255, 55, 55)",
                                    width = 1.5))
        ) %>% layout(
          title = list(text = " ", font = 22), 
          xaxis = list(
            categoryorder = "total descending",
            title = "Total Cases"
          ),
          yaxis = list(
            title = "Continent",
            range = c(0, max(res4()$Confirmed)+38000000)
          )
        )
    }
    else if(choice4() == "Highest Total Cases by Country"){
      res5 %>% 
        plot_ly(
          x = ~`Country/Region`,
          y = ~Confirmed,
          name = "Highest Total Cases by Country",
          type = "bar", 
          text = ~paste("\n", Confirmed),
          textposition = "outside",
          marker = list(color = "red",
                        line = list(color = "rgb(255, 55, 55)",
                                    width = 1.5))
        ) %>% layout(
          title = list(text = " ", font = 22), 
          xaxis = list(
            categoryorder = "total descending",
            title = "Total Confirmed"
          ),
          yaxis = list(
            title = "Country",
            range = c(0, max(res5$Confirmed)+38000000)
          )
        )
    }
  })
  
  #PLOT 2
  output$plot_g2 <- renderPlotly({
    if(choice5() == "Total Active Cases by Continent"){
      res4() %>% 
        plot_ly(
          x = ~`WHO Region`,
          y = ~Active,
          name = "Total Active Cases by Continent",
          type = "bar",
          text = ~paste("\n", Active),
          textposition = "outside",
          marker = list(color = "red",
                        line = list(color = "rgb(255, 55, 55)",
                                    width = 1.5))
        ) %>% layout(
          title = list(text = " ", font = 22), 
          xaxis = list(
            categoryorder = "total descending",
            title = "Total Active Cases"
          ),
          yaxis = list(
            title = "Continent",
            range = c(0, max(res4()$Active)+24000000)
          )
        )
    }
    else if(choice5() == "Highest Total Active Cases by Country"){
      res7 %>% 
        plot_ly(
          x = ~`Country/Region`,
          y = ~Active,
          name = "Highest Total Active Cases by Country",
          type = "bar",
          text = ~paste("\n", Active),
          textposition = "outside",
          marker = list(color = "red",
                        line = list(color = "rgb(255, 55, 55)",
                                    width = 1.5))
        ) %>% layout(
          title = list(text = " ", font = 22), 
          xaxis = list(
            categoryorder = "total descending",
            title = "Total Active Cases"
          ),
          yaxis = list(
            title = "Country",
            range = c(0, max(res7$Active)+38000000)
          )
        )
    }
  })
  
  #PLOT 3
  output$plot_g3 <- renderPlotly({
    if(choice6() == "Total Deaths by Continent"){
      res4() %>% 
        plot_ly(
          x = ~`WHO Region`,
          y = ~Deaths,
          name = "Total Deaths by Continent",
          type = "bar",
          text = ~paste("\n", Deaths),
          textposition = "outside",
          marker = list(color = "red",
                        line = list(color = "rgb(255, 55, 55)",
                                    width = 1.5))
        ) %>% layout(
          title = list(text = " ", font = 22), 
          xaxis = list(
            categoryorder = "total descending",
            title = "Total Deaths"
          ),
          yaxis = list(
            title = "Continent",
            range = c(0, max(res4()$Deaths)+2000000)
          )
        )
    }
    else if(choice6() == "Highest Total Deaths by Country"){
      res6 %>% 
        plot_ly(
          x = ~`Country/Region`,
          y = ~Deaths,
          name = "Highest Total Deaths by Country",
          type = "bar",
          text = ~paste("\n", Deaths),
          textposition = "outside",
          marker = list(color = "red",
                        line = list(color = "rgb(255, 55, 55)",
                                    width = 1.5))
        ) %>% layout(
          title = list(text = " ", font = 22), 
          xaxis = list(
            categoryorder = "total descending",
            title = "Total Deaths"
          ),
          yaxis = list(
            title = "Country",
            range = c(0, max(res6$Deaths)+2000000)
          )
        )
    }
    else if(choice6() == "Total Cases/Total Deaths by Continent"){
      res9 %>% 
        plot_ly(
          x = ~`WHO Region`,
          y = ~ration,
          name = "Total Cases/Total Deaths by Continent",
          type = "bar",
          text = ~paste("\n", round(ration, 2)),
          textposition = "outside",
          marker = list(color = "red",
                        line = list(color = "rgb(255, 55, 55)",
                                    width = 1.5))
        ) %>% layout(
          title = list(text = " ", font = 22), 
          xaxis = list(
            categoryorder = "total descending",
            title = "Total Cases/Total Deaths"
          ),
          yaxis = list(
            title = "Continent",
            range = c(0, max(res9$ration)+7)
          )
        )
    }
  })
  
  #PLOT 4
  output$plot_g4 <- renderPlotly({
    if(choice7() == "Total Recovered by Continent"){
      res4() %>% 
        plot_ly(
          x = ~`WHO Region`,
          y = ~Recovered,
          name = "Total Recovered by Continent",
          type = "bar",
          text = ~paste("\n", Recovered),
          textposition = "outside",
          marker = list(color = "rgb(4, 236, 35)",
                        line = list(color = "lightgreen",
                                    width = 1.5))
        ) %>% layout(
          title = list(text = " ", font = 22), 
          xaxis = list(
            categoryorder = "total descending",
            title = "Total Recovered"
          ),
          yaxis = list(
            title = "Continent",
            range = c(0, max(res4()$Recovered)+15000000)
          )
        )
    }
    else if(choice7() == "Highest Total Recovered by Country"){
      res8 %>% 
        plot_ly(
          x = ~`Country/Region`,
          y = ~Recovered,
          name = "Highest Total Recovered by Country",
          type = "bar",
          text = ~paste("\n", Recovered),
          textposition = "outside",
          marker = list(color = "rgb(4, 236, 35)",
                        line = list(color = "lightgreen",
                                    width = 1.5))
        ) %>% layout(
          title = list(text = " ", font = 22), 
          xaxis = list(
            categoryorder = "total descending",
            title = "Total Recovered"
          ),
          yaxis = list(
            title = "Country",
            range = c(0, max(res8$Recovered)+10000000)
          )
        )
    }
    else if(choice7() == "Total Cases/Total Recovered by Continent"){
      res10 %>% 
        plot_ly(
          x = ~`WHO Region`,
          y = ~ration,
          name = "Total Cases/Total Recovered by Continent",
          type = "bar",
          text = ~paste("\n", round(ration, 2)),
          textposition = "outside",
          marker = list(color = "rgb(4, 236, 35)",
                        line = list(color = "lightgreen",
                                    width = 1.5))
        ) %>% layout(
          title = list(text = " ", font = 22), 
          xaxis = list(
            categoryorder = "total descending",
            title = "Total Cases/Total Recovered"
          ),
          yaxis = list(
            title = "Continent",
            range = c(0, max(res10$ration)+0.6)
          )
        )
    }
  })
  
  ###------------END Global Statistics---------------
  
  
  ###------------Trends------------------------------
  trends <- mydataF %>% select(liste_m, liste_m2, Confirmed, Deaths, Recovered, Active, `WHO Region`) %>% group_by(`WHO Region`, liste_m2) %>% summarise(Confirmed=sum(Confirmed), Recovered=sum(Recovered), Deaths=sum(Deaths), Active=sum(Active))
  

  trends1 <-trends %>% filter(`WHO Region` == "Americas") 
  trends2 <-trends %>% filter(`WHO Region` == "Europe")
  trends2 <- trends2 %>% arrange(liste_m2)
  trends3 <-trends %>% filter(`WHO Region` == "Eastern Mediterranean")
  trends4 <-trends %>% filter(`WHO Region` == "South-East Asia")
  trends5 <-trends %>% filter(`WHO Region` == "Western Pacific")
  trends6 <-trends %>% filter(`WHO Region` == "Africa")
  
  #plot5
  output$plot_g5 <- renderPlotly({
    trends1 %>% 
      plot_ly(
        x = ~liste_m2,
        y = ~Confirmed,
        text = ~Confirmed,
        name="Americas",
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        showlegend = T,
        line = list(color = "red", width = 2),
        marker = list(color = "red", size = 7)
      ) %>% add_trace(
              y = ~trends2$Confirmed, 
              name = 'Europe', 
              mode = 'marker', 
              line = list(color="blue"),
              marker = list(color="blue")
            ) %>% add_trace(
                y = ~trends3$Confirmed, 
                name = 'Eastern Mediterranean', 
                mode = 'marker', 
                line = list(color="puprle"),
                marker = list(color="puprle")
              ) %>% add_trace(
                  y = ~trends4$Confirmed, 
                  name = 'South-East Asia', 
                  mode = 'marker', 
                  line = list(color="green"),
                  marker = list(color="green")
                ) %>% add_trace(
                    y = ~trends5$Confirmed, 
                    name = 'Western Pacific', 
                    mode = 'marker', 
                    line = list(color="brown"),
                    marker = list(color="brown")
                  ) %>% add_trace(
                      y = ~trends6$Confirmed, 
                      name = 'Africa', 
                      mode = 'marker', 
                      line = list(color="yellow"),
                      marker = list(color="yellow")
                    ) %>% layout(
                        title = list(text = "Total Cases Evolution by Continent", font = 22), 
                        xaxis = list(
                          title = "Month"
                        ),
                        yaxis = list(
                          title = "Total Cases"
                        )
                      )
  })
  
  #plot6
  output$plot_g6 <- renderPlotly({
    trends1 %>% 
      plot_ly(
        x = ~liste_m2,
        y = ~Deaths,
        text = ~Deaths,
        name="Americas",
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers',
        showlegend = T,
        line = list(color = "red", width = 2),
        marker = list(color = "red", size = 7)
      ) %>% add_trace(
        y = ~trends2$Deaths, 
        name = 'Europe', 
        mode = 'marker', 
        line = list(color="blue"),
        marker = list(color="blue")
      ) %>% add_trace(
        y = ~trends3$Deaths, 
        name = 'Eastern Mediterranean', 
        mode = 'marker', 
        line = list(color="puprle"),
        marker = list(color="puprle")
      ) %>% add_trace(
        y = ~trends4$Deaths, 
        name = 'South-East Asia', 
        mode = 'marker', 
        line = list(color="green"),
        marker = list(color="green")
      ) %>% add_trace(
        y = ~trends5$Deaths, 
        name = 'Western Pacific', 
        mode = 'marker', 
        line = list(color="brown"),
        marker = list(color="brown")
      ) %>% add_trace(
        y = ~trends6$Deaths, 
        name = 'Africa', 
        mode = 'marker', 
        line = list(color="yellow"),
        marker = list(color="yellow")
      ) %>% layout(
        title = list(text = "Total Deaths Evolution by Continent", font = 22), 
        xaxis = list(
          title = "Month"
        ),
        yaxis = list(
          title = "Total Deaths"
        )
      )
  })
    
  #plot7
  output$plot_g7 <- renderPlotly({
      trends1 %>% 
        plot_ly(
          x = ~liste_m2,
          y = ~Recovered,
          text = ~Recovered,
          name="Americas",
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers',
          showlegend = T,
          line = list(color = "red", width = 2),
          marker = list(color = "red", size = 7)
        ) %>% add_trace(
          y = ~trends2$Recovered, 
          name = 'Europe', 
          mode = 'marker', 
          line = list(color="blue"),
          marker = list(color="blue")
        ) %>% add_trace(
          y = ~trends3$Recovered, 
          name = 'Eastern Mediterranean', 
          mode = 'marker', 
          line = list(color="puprle"),
          marker = list(color="puprle")
        ) %>% add_trace(
          y = ~trends4$Recovered, 
          name = 'South-East Asia', 
          mode = 'marker', 
          line = list(color="green"),
          marker = list(color="green")
        ) %>% add_trace(
          y = ~trends5$Recovered, 
          name = 'Western Pacific', 
          mode = 'marker', 
          line = list(color="brown"),
          marker = list(color="brown")
        ) %>% add_trace(
          y = ~trends6$Recovered, 
          name = 'Africa', 
          mode = 'marker', 
          line = list(color="yellow"),
          marker = list(color="yellow")
        ) %>% layout(
          title = list(text = "Total Recovered Evolution by Continent", font = 22), 
          xaxis = list(
            title = "Month"
          ),
          yaxis = list(
            title = "Total Recovered"
          )
        )
    })
  
  #plot8
  output$plot_g8 <- renderPlotly({
      trends1 %>% 
        plot_ly(
          x = ~liste_m2,
          y = ~Active,
          text = ~Active,
          name="Americas",
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers',
          showlegend = T,
          line = list(color = "red", width = 2),
          marker = list(color = "red", size = 7)
        ) %>% add_trace(
          y = ~trends2$Active, 
          name = 'Europe', 
          mode = 'marker', 
          line = list(color="blue"),
          marker = list(color="blue")
        ) %>% add_trace(
          y = ~trends3$Active, 
          name = 'Eastern Mediterranean', 
          mode = 'marker', 
          line = list(color="puprle"),
          marker = list(color="puprle")
        ) %>% add_trace(
          y = ~trends4$Active, 
          name = 'South-East Asia', 
          mode = 'marker', 
          line = list(color="green"),
          marker = list(color="green")
        ) %>% add_trace(
          y = ~trends5$Active, 
          name = 'Western Pacific', 
          mode = 'marker', 
          line = list(color="brown"),
          marker = list(color="brown")
        ) %>% add_trace(
          y = ~trends6$Active, 
          name = 'Africa', 
          mode = 'marker', 
          line = list(color="yellow"),
          marker = list(color="yellow")
        ) %>% layout(
          title = list(text = "Total Active Evolution by Continent", font = 22), 
          xaxis = list(
            title = "Month"
          ),
          yaxis = list(
            title = "Total Active"
          )
        )
    })
  
  ###------------END Trends--------------------------
  
  
  
  ###------------Database----------------------------
  #display worldometer data
  output$datatable1 <- renderDataTable({
      mydata
    },
    escape = F,
    rownames = F,
    options = list(
      processing = FALSE,
      columnDefs = list(list(className = 'dt-center', targets = "_all"))
    )
  )
  
  #display full_grouped data
  output$datatable2 <- renderDataTable({
      mydataF
    },
    escape = F,
    rownames = F,
    options = list(
      processing = FALSE,
      columnDefs = list(list(className = 'dt-center', targets = "_all"))
    )
  )
  
  
  #download data
  output$download_data1 <- downloadHandler(
    filename = function() {
      paste("Worldometer_data", ".csv", sep = "")
    },
    
    content = function(file) {
      x <- fread("./worldometer_data.csv")
      write.csv(x, file, row.names=FALSE)
    }
  )
  
  output$download_data2 <- downloadHandler(
    filename = function() {
      paste("full_grouped", ".csv", sep = "")
    },
    
    content = function(file) {
      x <- fread("./full_grouped.csv")
      write.csv(x, file, row.names=FALSE)
    }
  )
  
  ###------------End Database------------------------
}

# Run the application 
shinyApp(ui = ui, server = server)
