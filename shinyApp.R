# Brett W.
# 26 July 2019
# Lesson 9: Shiny Web Apps
# Build Your Own Shiny App

# Necessary libraries.
library(shiny)
library(ggmap)
library(ggplot2)

ui <- fluidPage(
  selectInput("variable", "Variable: ",
  c("July Population" = "july11pop",
    "Region of Country" = "region",
    "Change in Population" = "popChange",
    "Percent Change in Population" = "percentChange")),

plotOutput("plot")
)

server <- function(input, output) {
  dfStates <- readCensus()
  dfStates <- dfStates[dfStates$stateName != "District of Columbia",]

dfStates$region <- state.region
dfStates$stateName <- tolower(dfStates$stateName)

dfStates$popChange <- dfStates$july11pop - dfStates$july10pop

dfStates$percentChange <- dfStates$popChange / dfStates$july10pop * 100

us <- map_data("state")

output$plot <- renderPlot(
  ggplot(dfStates, aes(map_id = state)) + 
    geom_map(map = us,
             aes(fill=dfStates[,input$variable])) +
    expand_limits(x = us$long, y = us$lat) +
    coord_map() + ggtitle("State Population") + 
    guides(fill=guide_legend(title=input$variable))
  )
}

# Read in the census data set. This function
# is the same as the one previously defined.

readCensus <- function() {
  urlToRead <- "http://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  
  # Do the basic cleanup
  testFrame <- read.csv(url(urlToRead))
  testFrame <- testFrame[-1:-8,]
  testFrame <- testFrame[,1:5]
  testFrame$stateName <- testFrame[,1]
  testFrame <- testFrame[,-1]
  testFrame <- testFrame[-52:-58,]
  
  #remove the 'dot' form the state name
  testFrame$stateName <- gsub("\\.","", testFrame$stateName)
  
  # Numberize function. 
  Numberize <- function(inputVector)
  {
    # Get rid of commas
    inputVector<-str_replace_all(inputVector,",","")
    # Get rid of spaces
    inputVector<-str_replace_all(inputVector," ","")
    
    return(as.numeric(inputVector))
  }
  
  # Convert colummns to numbers and rename columns
  testFrame$april10census <- Numberize(testFrame$X)
  testFrame$april10base <- Numberize(testFrame$X.1)
  testFrame$july10pop <- Numberize(testFrame$X.2)
  testFrame$july11pop <- Numberize(testFrame$X.3)
  testFrame <- testFrame[,-1:-4]
  
  # Remove the old rownames, which are now confusing
  rownames(testFrame) <- NULL
  
  return(testFrame)
}

shinyApp(ui = ui, server = server)