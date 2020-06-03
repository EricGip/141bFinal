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
library(DT)

# loading in api keys 
# usethis::edit_r_environ("project")
readRenviron(".Renviron")

# Meal API choices for sidebar. 
categories <- fromJSON("https://www.themealdb.com/api/json/v1/1/categories.php")

catChoices <- categories$categories$strCategory

areas <- fromJSON("https://www.themealdb.com/api/json/v1/1/list.php?a=list")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    shinyUI(navbarPage("141B Final", id = "tabs",
                       #start of about panel
                       tabPanel("About", 
                                titlePanel("About"),
                                tags$h3("About this project"),
                                tags$p("This Shiny app takes advantage of the TheMealDB api, TheCocktailDB api, and the Riot Games API."),
                                tags$p("This was challenging because I had to learn a completely new framework and work in a statistical language, R, that isn't tradionally used for web development. I'm amazed at how far you can push DOM manipulation and create a site without having to learn 3 different languages (HTML, CSS, JS)."),
                                tags$p("Since this was an individual project instead of a group project, I embraced the freedom given and added all the APIs that interested me. ")
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
                                        selectInput(inputId = "areaChoices", label = "Pick an area or ethnicity", choices = areas$meals$strArea),
                                        actionButton(inputId = "searchCat", label = "Search by category"),
                                        actionButton(inputId = "searchArea", label = "Search by area"),
                                        
                                        tags$br(),
                                        tags$br("After searching by category or area, enter the Meal ID here."),
                                        tags$br(),
                                        
                                        textInput(inputId = "searchMealId", label = "Enter Meal ID", value = "52772", placeholder = "Enter Meal ID here"),
                                        actionButton(inputId = "searchById", label = "Search by Id"),
                                        tags$br("Alternatively, search for a random meal here."),
                                        tags$br(),
                                        
                                    actionButton(inputId = "searchRandomMeal", label = "Search for a random meal"),
                                    ),
                                
                                    
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                        tableOutput("catSearchOut"),
                                        tableOutput("areaSearchOut"),
                                        tableOutput("randomMealOut"),
                                        tableOutput("mealIdSearchOut")
                                        
                                    )
                                )
                       ),
                     
                       # start of cocktail panel
                       tabPanel("Cocktail",
                                titlePanel("Mix the perfect cocktail")
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
                                        tableOutput("lpOut")
                                    )
                                )
                       )
                       # end of riot panel
                       ## end of shinyUI
    ))
    
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
    
    # End of riot games api 
    
    # observing button for current challengers
    observeEvent(input$searchChal, {
        output$lpOut <- renderTable({
            leaderBoard
        })
    })
    
    # Button to search by category 
    
    ## have to set our search to the category?, just use nested observeEvent i guess.
    observeEvent(input$catChoices, {
        getCategory <- str_glue("https://www.themealdb.com/api/json/v1/1/filter.php?c={categorySearch}",
                                categorySearch = input$catChoices)
        categoryCall <- GET(
            getCategory
        )
        stop_for_status(categoryCall)
        catJson <- content(categoryCall, as = "text")
        catMeal <- fromJSON(catJson, flatten = TRUE)
        
        observeEvent(input$searchCat, {
            output$catSearchOut <- renderTable({
                catMeal %>%
                    as.data.frame %>%
                    select(meals.strMeal, meals.idMeal)
            })
        })
    })
    
    # end of category search 
    
    # button to search by area
    
    ## TO DO -- MAYBE SHOW PICTURES, FIX COLUMNS. 
    observeEvent(input$areaChoices, {
        getArea <- str_glue("https://www.themealdb.com/api/json/v1/1/filter.php?a={area}",
                            area = input$areaChoices)
        
        areaCall <- GET(
            getArea
        )
        stop_for_status(areaCall)
        areaJson <- content(areaCall, as = "text")
        areaMeals <- fromJSON(areaJson, flatten = TRUE)
        
        observeEvent(input$searchArea, {
            output$areaSearchOut <- renderTable({
                areaMeals
            })
        })
    })
    
    # end of button to search by area
    
    # button to search by meal ID
    
    # To do
    # breaks when invalid mealId is entered... need to make it pause and only "onCLick?"
    # need to make the columns rows or find a better way to display this like the random meal. 
    observeEvent(input$searchMealId, {
        observeEvent(input$searchById, {
            getMealById <- str_glue("https://www.themealdb.com/api/json/v1/1/lookup.php?i={mealId}",
                                    mealId = input$searchMealId)
            
            mealIdCall <- GET(
                getMealById
            )
            stop_for_status(mealIdCall)
            mealIdJson <- content(mealIdCall, as = "text")
            MealIdMeals <- fromJSON(mealIdJson, flatten = TRUE)
            
            output$mealIdSearchOut <- renderTable({
                MealIdMeals
            })
        })
    })
    
    
    # button to generate a random meal
    
    # TO DO, ADJUST THE MATRIX? MAKE ALL OF THE COLMNS ROWS??? 
    observeEvent(input$searchRandomMeal, {
        randomMealCall <- GET(
            "https://www.themealdb.com/api/json/v1/1/random.php"
        )
        stop_for_status(randomMealCall)
        randomMealJson <- content(randomMealCall, as = "text")
        randomMeal <- fromJSON(randomMealJson, flatten = TRUE)
        
        output$randomMealOut <- renderTable({
            randomMeal
        })
    })
    
    
    # end of random Meal 
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
