library(shiny)
library(plotly)
library(tidyr)
library(dplyr)
library(maptools)
library(ggmap)
library(broom)
library(sp)
library(htmlwidgets)
library(htmltools)

##Data
corn_harvest <- read.csv("US_corn_harvest.csv")
corn_harvest$Value <- as.character(corn_harvest$Value)
corn_harvest$Value <- as.numeric(gsub(",","",corn_harvest$Value))
corn_harvest$State <- as.character(corn_harvest$State)

soybeans_harvest <-read.csv("US_soybeans_harvest.csv")
soybeans_harvest$Value <- as.character(soybeans_harvest$Value)
soybeans_harvest$Value <- as.numeric(gsub(",","", soybeans_harvest$Value))
soybeans_harvest$State <- as.character(soybeans_harvest$State)

All_harvest <- left_join(corn_harvest, soybeans_harvest, by = c("Year", "State", "Domain.Category"))
All_harvest_f <- All_harvest %>% select(Year, State, Code, Domain.Category, Value.x, Value.y, )
All_harvest_f <- All_harvest_f %>% dplyr::rename(Corn_harvest = Value.x, Soybeans_harvest = Value.y)
All_harvest_f <- All_harvest_f %>% gather(Corn_harvest, Soybeans_harvest, key = "Crop", value = "Value")
All_harvest_f$Value[is.na(All_harvest_f$Value)] <- 0


##################################################
codes <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
codes2 <- codes %>% select(code, state)
names(codes2)[names(codes2) == "state"] <- "State"
GMOdata <- read.csv("GMO1.csv", stringsAsFactors = TRUE)
GMOdata$Value <- as.numeric(GMOdata$Value)

#Whitespaces in Variety
GMOdata$Variety <- as.character(GMOdata$Variety)
GMOdata$Variety <- trimws(GMOdata$Variety, which = c("both"))
GMOdata$Variety <- as.factor(GMOdata$Variety)

GMOdata <- left_join(GMOdata, codes2, by = "State" )
GMOdata$hover <- with(GMOdata, paste(State,'<br>', Value, "% of", Crop, " planted are GMO"))

#############################################
ui <- fluidPage(
   
   titlePanel("How has the US embraced Genetically Modified (GM) Crops? Find out which states have undergone this transition, and how fast."),
   
   
   sidebarLayout(
      sidebarPanel(
         verbatimTextOutput("Selected_Year"),
         sliderInput(inputId = "Year",
                     label = "Select the Year:",
                     min = 2000,
                     max = 2018,
                     value = 2000,
                     sep = "", animate = animationOptions(interval = 1300, loop = FALSE)),
         br(),
         h3("Map 1 inputs:"),
         selectInput("Crop", "Crop type:", c("Corn", "Cotton", "Soybeans"), selected = "Corn"),
         selectInput("Variety", "Variety:", c("All GE varieties", "Herbicide-tolerant only", "Insect-resistant (Bt) only", "Stacked gene varieties"), selected = "All GE varieties"),
         actionButton("ConfirmButton", "Confirm"),
         br(),
         br(),
         verbatimTextOutput("GMO_inputsf"),
         br(),
         br(),
         
         h3("Map 2 inputs:"),
         selectInput("Crop2", "Crop type:", c("Corn", "Soybeans"), selected = "Corn"),
         actionButton("ConfirmButton2", "Confirm"),
         verbatimTextOutput("Total_crops"),
         
         h3("Background"),
         helpText("Genetically Modified Crops (GMO) and Genetic Engineering (GE) technology are terms which cause anxiety
                  in some countries, especially in regards to Food safety. Whilst Europe has been reluctant to embrace GMO foods in recent times, 
                  the US has been very quick to adopt it. Corn, Soybeans and Cotton are the main GE products here. This app will help to visualise
                  the adoption of these GE practices from 2000-2018. Note, the lack of data for Total cotton produced which is reflected in Map 2."),
         helpText( a("Read more here.", href= "https://theconversation.com/gm-crop-ruling-shows-why-the-eus-laws-are-wholly-inadequate-100675"))
      ),
      
      mainPanel(
         h3(textOutput("Map1")),
         plotlyOutput("USAGMO", width = "100%", height = 400),
         h3(textOutput("Map2")),
         plotlyOutput("USACorn", width = "100%", height = 400)
      )
   )
)

server <- function(input, output, session) {
   
   output$Map1 <- renderText({
      "Genetically Modified Crops: percentage of GM crops from All crops"
   })
   
   df_codes <- eventReactive(input$ConfirmButton, {
      FilteredYr <- GMOdata %>% filter(Year== input$Year)
      FilteredCp = switch(input$Crop, 
                       "Corn" = FilteredYr %>% filter(Crop == "Corn"),
                       "Soybeans" = FilteredYr %>% filter(Crop == "Soybeans"),
                       "Cotton" = FilteredYr %>% filter(Crop == "Upland cotton"))
      FilteredV = switch(input$Variety, 
                        "All GE varieties" = FilteredCp %>% filter(Variety == "All GE varieties"),
                        "Herbicide-tolerant only" = FilteredCp %>% filter(Variety == "Herbicide-tolerant only"),
                        "Insect-resistant (Bt) only" = FilteredCp %>% filter(Variety == "Insect-resistant (Bt) only"),
                        "Stacked gene varieties" = FilteredCp %>% filter(Variety == "Stacked gene varieties"))
      return (FilteredV)
   })
   
   col.list <- list("YlOrRd", "Greens", "Blues")
   
   cols <- eventReactive(input$ConfirmButton, {
      switch(input$Crop,
             "Corn" = col.list[[1]],
             "Soybeans" = col.list[[2]],
             "Cotton" = col.list[[3]])
     
   })
   
   cropname1 <- eventReactive(input$ConfirmButton,  {
      ignoreNULL = TRUE
      switch(input$Crop,
             "Corn" = "Corn",
             "Cotton" = "Cotton",
             "Soybeans" = "Soybeans")
   })
   
   Yr <- eventReactive(input$ConfirmButton, {
         return(input$Year)
      
   })
   
   
   GMO_input <- eventReactive(input$ConfirmButton, {
      sprintf("Crop type selected: %s , \nVariety chosen: %s", input$Crop, input$Variety)
   })
   output$GMO_inputs  <- renderText({GMO_input()})
   
   Total_crops <- eventReactive(input$ConfirmButton2, {
      sprintf("Crop type selected: %s", input$Crop2)
   })
   output$Total_crops  <- renderText({Total_crops()})
 
   
   output$Selected_Year <- renderText(sprintf("Year:  %s \n Please press confirm for both Map1 and Map2", input$Year))
   
   output$Map2 <- renderText({
      "All crops"
   })
   
   output$USAGMO <- renderPlotly({
      
      
      l <- list(color = toRGB("white"), width = 1)
      
      # specify some map projection/options
      g <- list(
         scope = 'usa',
         projection = list(type = 'albers usa'),
         showlakes = TRUE,
         lakecolor = toRGB('white')
      )
      
      plot_geo(df_codes(), locationmode = 'USA-states') %>%
         add_trace(
            z = ~Value, text = ~hover, locations = ~code,
            color = ~ Value, colors = cols(),
            zmin = 0, zmax = 100
         ) %>%
         colorbar(title = sprintf("Percentage of all %s planted.", cropname1())) %>%
         layout(
            title = sprintf('Percentage of %s planted in %.0f' , cropname(), Yr()),
            geo = g
         )
      
   })
   
   crop_total <- eventReactive(input$ConfirmButton2, {
      FilteredTotal = switch(input$Crop2, 
                          "Corn" = All_harvest_f %>% filter(Crop == "Corn_harvest"),
                          "Soybeans" = All_harvest_f %>% filter(Crop == "Soybeans_harvest"))
      if(input$Year %in% c(2000, 2001, 2002, 2003,2004, 2005, 2006)){
         FilteredTotal <- FilteredTotal %>% filter(Year == 2002)
      } else if (input$Year %in% c(2007, 2008, 2009, 2010, 2011)) {
         FilteredTotal <- FilteredTotal %>% filter(Year == 2007)
      } else if (input$Year %in% c(2012, 2013, 2014, 2015, 2016)) {
         FilteredTotal <- FilteredTotal %>% filter(Year == 2012)
      } else {
         FilteredTotal <- FilteredTotal %>% filter(Year == 2017)
      }
      return (FilteredTotal)
   })
   
   
   cols2 <- eventReactive(input$ConfirmButton2, {
      switch(input$Crop2,
             "Corn" = col.list[[1]],
             "Soybeans" = col.list[[2]])
      
   })
   
   YrPeriod <- eventReactive(input$ConfirmButton2, {
      if(input$Year %in% c(2000, 2001, 2002, 2003, 2004, 2005, 2006)){
         Yr <- "2000 -2006"
      } else if (input$Year %in% c(2007, 2008, 2009, 2010, 2011)) {
         Yr <- "2007-2011"
      } else if (input$Year %in% c(2012, 2013, 2014, 2015, 2016)) {
         Yr <- "2012-2016"
      } else {
         Yr <- "2017-2018"
      }
      return(Yr)
   })
   
   cropname <- eventReactive(input$ConfirmButton2,  {
      ignoreNULL = TRUE
      switch(input$Crop2,
             "Corn" = "Corn",
             "Soybeans" = "Soybeans")
   })
   
   br()
   br()
   br()
   
   output$USACorn <- renderPlotly({
      Map2 <- crop_total()
      Map2 <- Map2 %>% group_by(State, Code) %>% summarise("Acres" = sum(Value, na.rm = TRUE))
      
      l <- list(color = toRGB("white"), width = 1)
      
      
      
      # specify some map projection/options
      g <- list(
         scope = 'usa',
         projection = list(type = 'albers usa'),
         showlakes = TRUE,
         lakecolor = toRGB('white')
      )
      
      plot_geo(Map2, locationmode = 'USA-states') %>%
         add_trace(
            z = ~ Acres, locations = ~Code,
            color = ~ Acres, colors = cols2(),
            text = ~ State,
            zmin = 0, zmax = 5000000
         ) %>%
         colorbar(title = paste("Total acres harvested of: ", cropname())) %>%
         layout(
            title = sprintf('Total %s harvested. Data available between %s', cropname(), YrPeriod()),
            geo = g
         )
      
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server, options=list(height = "100%"))