library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(sf)
library(viridis)
library(colorRamps)
library(colorspace)
library(DT)

#source("aux fcns.R")
load("appdata.RData")


#source("owner-occupancy leaflet.R")
#owner.occupancy <-
#  readRDS(file = "owner-occupancy-leaflet.RDS")
#colnames(dat)
?diverge_hcl
col.pal <- 
  diverge_hcl(3,
                       h = c(190, 320),
                       l = c(40, 80),
                       power = 2.5)
'diverge_hcl(5
                       , h = c(200, 10)
                       , c = 90
                       , l = c(40, 80)
                       )'
#diverge_hcl(2,
#h = c(190, 320)),#"Accent",#c('#004B69','#1a7160','#ffdab9','#ce7226','#770B3B'),

############ set option lists ############

#geo.vars <- c("Borough",
#              "Sub-borough")

group.vars <- c("All city",
                "Owners" = 1,
                "Renters" = 9)
#               , "Coop/Condo"

vars <- c("Mean Household Income" = "Mean.Income",
          "Total Households" = "Total.Households",
          "Condo/Coop Count" = "Coop.condo.count",
          "Median Rent : Income Ratio" = "Rent.Income.Ratio",
          "Years in Current Residence" = "Mean.years.in.home")

oo.grps <- c("All city",
             "1 - 3 Unit Homes",
             "Coops & Condos",
             "Other Class 2")

############################################################

# Define UI
ui <- fluidPage(
  
  theme = "paper",
  
  titlePanel("Mapping NYC Housing"),
  
  tabsetPanel(type = "tabs",
              
              tabPanel("Housing Overview",
                       
                       sidebarLayout(
                         sidebarPanel(
                           
                           # select groups to map
                           selectInput(inputId = "grp",
                                       label = strong("Select group to map"),
                                       choices = group.vars,
                                       selected = "All city"),
                           
                           #  select information to map
                           selectInput(inputId = "var",
                                       label = strong("Select information to map"),
                                       choices = vars,
                                       selected = "Median Income"),
                           width = 3
                         ),
                         
                         # Output
                         mainPanel(
                          leafletOutput(outputId = "map",
                                         height = "740px"),
                          width = 9))
                       
                       , tags$div(
                         HTML("<font size ='-3'>
                              <p align = right><em>
                              All values estimated from the 2017 NYC Housing Vacancy Survey, downloadable at: <a href='https://www.census.gov/programs-surveys/nychvs.html'> https://www.census.gov/programs-surveys/nychvs.html</a>.
                              <br>Because values reflect survey data, each has a standard error. Clicking on any geography will show the exact estimate and its standard error.
                              </em></font>"),
                         #Maps and underlying data prepared by Kira McDonald, Economist at NYC Council Finance.<br>
                         style = "line-height:11px")
              )
              
              , tabPanel("Owner Occupancy Map",
                         
                       #  sidebarLayout(
                       #     sidebarPanel(
                             # select groups to map
                       #       selectInput(inputId = "oo.grp",
                      #                   label = strong("Select Building Type"),
                       #                  choices = oo.grps,
                      #                   selected = "All city"),
                       #      width = 3
                        #     ),
                      #     mainPanel(
                             leafletOutput(outputId = "oo",
                                           height = "740px")
                      #       , width = 9
                      #       )
                      #   )
              )
              
              , tabPanel("Owner Occupancy by Tax Class Table",
                         dataTableOutput(outputId = "oot")
                         
                         , tags$div(
                           HTML("<br><em>Units in building approximate Tax Classes in the NYC property tax system.<br>
                                    In general, 1-3 unit buildings are class 1; 4-10 unit buildings are small class 2; and 10+ unit ones are large class 2.</em>"))
                         )
  )
)



server <- function(input, output) {
  
  xmin <- as.numeric(st_bbox(dat)$xmin)
  xmax <- as.numeric(st_bbox(dat)$xmax)
  ymin <- as.numeric(st_bbox(dat)$ymin)
  ymax <- as.numeric(st_bbox(dat)$ymax)
  
  grp.by.input <- reactive ({
    
    if (input$grp == "All city") {
      ac.dat
    } else {
      dat[which(dat$Tenure.1 == input$grp),] 
    }
    
  })
  
  # render map
  output$map <- renderLeaflet({
    
    to.map <- grp.by.input()
    
    to.map <-
      rename(to.map,#[, input$var],
             "plot.var" = input$var)
    
    # I want the color domain to be defined over full range of possible values of all groups
    # so I combine all city dat with owner-renter disaggregrated dat. Otherwise comparison btwn
    # grouping would get confusing because meanings of colors would change as you looked at the different groups.
    # note that this command both runs manipulates the dataframes and defines the domain category
     ### (does this manipulation make map bug out if you swtich back and forth too much?)
    domain <-
      append(eval(parse(text = paste0("ac.dat$",
                                      input$var))),
             eval(parse(text = paste0("dat$",
                                      input$var))))

    'domain <- 
      append(rename(ac.dat[, input$var],
                    "plot.var" = input$var)$plot.var,
             rename(dat[, input$var],
                    "plot.var" = input$var)$plot.var)'
    
    #?colorQuantile
    #?viridis_pal
    #?colorBin
    pal <- colorBin(palette = col.pal,
                    
                    #c('#004B69','#1a7160','#ffdab9','#ce7226','#800000'), #c('#004E62','#1a7146','#ffdab9','#ce7226','#800000'),
                    #c('#008080','#51969e','#83aebe','#ffdab9','#f88858','#c73f1a','#800000'),##      "BrBG",#"RdGy", #viridis(3), #
                    #"PRGn",#primary.colors(3),#
                    domain = domain,
                    bins = 6,
                    reverse = FALSE
                    )
    
    
    # creates what pops up when you click on an area
    popup <- paste0("<strong>",
                    to.map$Sub.borough.name,
                    "<br>",
                    #### Variable name (special formatting for mean income)
                    case_when(input$var == "Mean.Income" ~ "Mean Income, thousands",#) </i><strong>",
                              TRUE ~ gsub("\\.", " ",
                                          input$var))
                    , ": </strong>"
                    
                    #### variable value (special formating for rent:income ratio)
                    , case_when(input$var == "Rent.Income.Ratio" ~ paste0(format(to.map$plot.var,
                                                                                  digits = 0,
                                                                                  big.mark = ",",
                                                                                  scientific = FALSE),
                                                                           "%"),
                                 TRUE ~ format(to.map$plot.var,
                                               digits = 0,
                                               big.mark = ",",
                                               scientific = FALSE))
                    
                    #### standard errors             
                    , "<br><strong>Standard Error: </strong>"
                    ,  case_when(input$var == "Rent.Income.Ratio" ~ "n/a",
                                                                    #paste0(format(eval(parse(text = paste0("to.map$",
                                                                    #                                       input$var,
                                                                    #                                       "_se"))),
                                                                    #              digits = 2,
                                                                    #              big.mark = ",",
                                                                    #              scientific = FALSE),
                                                                    #       "%"),
                                 
                                 TRUE ~ format(eval(parse(text = paste0("to.map$",
                                                                        input$var,
                                                                        "_se"))),
                                               digits = 0,
                                               big.mark = ",",
                                               scientific = FALSE))
                    )
    
    leaflet(options = leafletOptions(minZoom = 9)) %>%
    
      addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% #"CartoDB.PositronNoLabels" "Esri.WorldGrayCanvas"
      
      addPolygons(data = BB,
                  fillColor = "transparent",
                  weight = 1.9,
                  opacity = .6,
                  color = "#00345A",
                  stroke = TRUE) %>%
      
      addPolygons(data = to.map,
                  fillColor = ~pal(to.map$plot.var),
                  fillOpacity = .85,
                  popup = popup,
                  color = "#002A33",
                  weight = 1.2,
                  opacity = .4,
                  stroke = TRUE
                  ) %>%
      
      addPolygons(data = parks,
                  color = "#3F783B",
                  fillOpacity = 1,
                  stroke = FALSE) %>%
      
      setMaxBounds(lng1 = xmax+1,
                   lng2 = xmin-1,
                   lat1 = ymax+1,
                   lat2 = ymin-1) %>%
      
      addLegend(data = dat,
                pal = pal,
                values = ~domain,
                opacity = .6,
                title = case_when(input$var == "Mean.Income" ~ "Mean Household Income<br>($, thousands)",
                                  TRUE ~ gsub("\\.", " ",
                                              input$var)),
                labFormat = labelFormat(suffix = if_else(input$var == "Rent.Income.Ratio",
                                                         "%",
                                                         "")),
                            
                position = "bottomleft") %>%
      setView(lng = (xmin + xmax) / 2,
              lat = (ymin + ymax) / 2,
              zoom = 10)

  
  })
  

  #######################################
  #                                   ###
  #     ~~~ Owner Occupancy ~~~       ###
  #                                   ###
  #######################################
  
  oo.grp.by.input <- reactive ({
    
    if (input$grp == "All city") {
      ac.dat
    } else {
      dat[which(dat$Tenure.1 == input$grp),] 
    }
    
  })
  
  
  output$oo <- renderLeaflet({
    
    pal <- colorBin(palette = col.pal,
                    domain = oo.to.map$owner.occupancy.rate,
                    bins = 4,
                    reverse = TRUE)
    
    
    #to.map
    # creates what pops up when you click on an area
    popup <- paste0("<strong>",
                    oo.to.map$Sub.borough.name,
                    "<br><strong>Estimated Owner Occupancy Rate: </strong>"
                    , paste0(format(100 * oo.to.map$owner.occupancy.rate,
                                    digits = 2),
                             "%")
                    )
    
    leaflet(options = leafletOptions(minZoom = 9)) %>%
      addProviderTiles(provider = "Esri.WorldGrayCanvas") %>% #"CartoDB.PositronNoLabels" "Esri.WorldGrayCanvas"
      
      addPolygons(data = BB,
                  fillColor = "transparent",
                  weight = 1.9,
                  opacity = .6,
                  color = "#00345A",
                  stroke = TRUE) %>%
      
      addPolygons(data = oo.to.map,
                  fillColor = ~pal(owner.occupancy.rate),
                  fillOpacity = .85,
                  popup = popup,
                  color = "#002A33",
                  weight = 1.2,
                  opacity = .4,
                  stroke = TRUE)  %>%
      
      addPolygons(data = parks,
                  color = "#3F783B",
                  fillOpacity = 1,
                  stroke = FALSE) %>%

      setMaxBounds(lng1 = xmax+1,
                   lng2 = xmin-1,
                   lat1 = ymax+1,
                   lat2 = ymin-1) %>%
      addLegend(pal = pal,
                values = oo.to.map$owner.occupancy.rate,
                opacity = .6,
                labFormat = labelFormat(suffix = "%",
                                        transform = function(x) 100 * x),
                title = "Estimated Owner Occupancy<br>Rate",
                position = "bottomleft") %>%
      setView(lng = (xmin + xmax) / 2,
              lat = (ymin + ymax) / 2,
              zoom = 10)
  })
  
  
  
  
  output$oot <- renderDataTable({
    
    colnames(oot) <- c("Units in Building",
                       "Coop/Condo Status",
                       "Estimated Total Households",
                       "Estimated Owner-occupied Households",
                       "Estimated Owner Occupancy",
                       "Owner Occupancy 95% Confidence Interval")

    DT::datatable(oot,
                  options = list(lengthMenu = c(4, 8, 12),
                                 pageLength = 12))  %>%
      formatRound("Estimated Total Households",
                  digits = 0) %>%
      formatRound("Estimated Owner-occupied Households",
                  digits = 0) %>%
      formatPercentage("Estimated Owner Occupancy",
                       digits = 1)
    
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)

