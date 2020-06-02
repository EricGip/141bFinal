#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(jsonlite)
library(lubridate)
library(httr)
library(ggplot2)
library(dplyr)
library(shiny)
library(usethis)


# loading in api keys 
# usethis::edit_r_environ("project")
readRenviron(".Renviron")



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    shinyUI(navbarPage("League Of Legends Statistics",
                       tabPanel("Top players"),
                       tabPanel("Search for players"),
                       tabPanel("Test42")
    )),
    
    # Application title
    titlePanel("League of Legends rankings"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("", "Highest Rank", c("test", "test2")),
            sliderInput("obs",
                        "Number of oberservations:",
                        min = 1,
                        max = 200,
                        value = 1
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("summaryTitle"),
            verbatimTextOutput("summary"),
           #tableOutput("lpOut"),
           #plotOutput("plotOut"),
           tableOutput("tableOut")
        )
    ),
    
    fluidRow(
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ## Getting top ranks
    leaderBoard <- str_glue(
        "https://na1.api.riotgames.com/lol/league/v4/challengerleagues/by-queue/RANKED_SOLO_5x5/?api_key={key}",
        key = Sys.getenv("riotKey")
    )
    
    leaderBoardCall <- GET(
        leaderBoard
    )
    stop_for_status(leaderBoardCall)
    json2 <- content(leaderBoardCall, as = "text")
    topRanks <- fromJSON(json2, flatten = TRUE)
    
    test42 <- topRanks %>%
        as.data.frame %>%
        select(entries.summonerName, entries.leaguePoints) %>%
        arrange(desc(topRanks$entries$leaguePoints))
    
    output$summaryTitle <- renderPrint({
        "Summary Statistics of LP in Challenger"
    })
    
    output$summary <- renderPrint({
        summary(topRanks$entries$leaguePoints)
    })
    
    output$lpOut <- renderTable({
        test52
    })
    
    output$plotOut <- renderPlot({
        scatter.smooth(topRanks$entries$wins, topRanks$entries$leaguePoints)
    })
    
    
    output$tableOut <- renderTable({
        test42
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
