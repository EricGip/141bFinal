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

# Meal API 
categories <- fromJSON("https://www.themealdb.com/api/json/v1/1/categories.php")

catChoices <- categories$categories$strCategory

areas <- fromJSON("https://www.themealdb.com/api/json/v1/1/list.php?a=list")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    shinyUI(navbarPage("141B Final", id = "tabs",
                       #start of about panel
                       tabPanel("About", 
                                titlePanel("About"),
                       ),
                       
                       #end of about panel
                       # start of meal panel
                       tabPanel("Meal", 
                                # Application title
                                titlePanel("Meal Generator!"),
                                
                                # Sidebar with a slider input for number of bins 
                                sidebarLayout(
                                    sidebarPanel(
                                        selectInput(inputId = "catChoices", label = "Pick a category", choices = categories$categories$strCategory),
                                        selectInput(inputId = "areas", label = "Pick a category", choices = areas$meals$strArea),
                                        actionButton(inputId = "searchCat", label = "Search!")
                                    ),
                                    
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                        
                                    )
                                )
                       ),
                       
                       
                       tabPanel("Random", 
                                titlePanel("Pick a random meal"),
                       ),
                       # start of cocktail panel
                       tabPanel("Cocktail",
                                titlePanel("Mix the perfect cocktail"),
                       ),
                       # end of cocktail panel
                       
                       # start of riot panel
                       tabPanel("Riot",
                                titlePanel("League of Legends API"),
                                sidebarLayout(
                                    sidebarPanel(
                                        actionButton(inputId = "searchChal", label = "Search top players")
                                    ),
                                    mainPanel(
                                        
                                    )
                                )
                       )
                       # end of riot panel
                       ## end of shinyUI
    )),
    
    # end of fluid page
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    # riot games api 
    getLeaderBoard <- str_glue(
        "https://na1.api.riotgames.com/lol/league/v4/challengerleagues/by-queue/RANKED_SOLO_5x5/?api_key={key}",
        key = Sys.getenv("riotKey")
    )
    
    leaderBoardCall <- GET(
        getLeaderBoard
    )
    stop_for_status(leaderBoardCall)
    json2 <- content(leaderBoardCall, as = "text")
    topRanks <- fromJSON(json2, flatten = TRUE)
    
    leaderBoard <- topRanks %>%
        as.data.frame %>%
        select(entries.summonerName, entries.leaguePoints) %>%
        arrange(desc(topRanks$entries$leaguePoints))
    
    
    observeEvent(input$searchChal, {
        
    })
    output$lpOut <- renderTable({
        leaderBoard
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
