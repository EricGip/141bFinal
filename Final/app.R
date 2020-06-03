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

drinkCategories <- fromJSON("https://www.thecocktaildb.com/api/json/v1/1/list.php?c=list") 

drinkIngredients <- fromJSON("https://www.thecocktaildb.com/api/json/v1/1/list.php?i=list")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    shinyUI(navbarPage("141B Final", id = "tabs",
                       #start of about panel
                       tabPanel("About", 
                                titlePanel("About"),
                                tags$h3("About this project"),
                                tags$p("This Shiny app takes advantage of the TheMealDB API, TheCocktailDB API, and the Riot Games API."),
                                tags$h3("Challenging and enjoyable parts of the project"),
                                tags$p("This was challenging because I had to learn a completely new framework and work in a statistical language, R, that isn't tradionally used for web development. I'm amazed at how far you can push DOM manipulation and create a site without having to learn 3 different languages (HTML, CSS, JS). However, after learning the Shiny framework I have a new appreciation for front-end development and will never take Bootstrap/React for granted ever again..."),
                                tags$h3("Why I chose these APIs"),
                                tags$p("Since this was an individual project instead of a group project, I embraced the freedom given and added all the APIs that interested me. I enjoy cooking, but am often as a loss at what to cook; this API allows me to easily pick a dinner with a drink on the side. I am also among the top 500 players in League Of Legends, but unable to play as much as I did before college. I am easily able to keep track of the Most Efficient Tactics Available (META) by seeing familiar names and knowing their playstyles to determine what the optimal playstyle is on the current patch just by few simple API calls compared to manually looking up leaderboards.")
                       ),
                       
                       #end of about panel
                       
                       # start of meal panel
                       tabPanel("Meal Generator", 
                                # Application title
                                titlePanel("Pick a Meal, any meal"),
                                
                                # Sidebar with a slider input for number of bins 
                                sidebarLayout(
                                    sidebarPanel(
                                        selectInput(inputId = "catChoices", label = "Pick a category", choices = categories$categories$strCategory),
                                        actionButton(inputId = "searchCat", label = "Search by category"),
                                        tags$br(),
                                        tags$br(),
                                        selectInput(inputId = "areaChoices", label = "Pick an area or ethnicity", choices = areas$meals$strArea),
                                        
                                        actionButton(inputId = "searchArea", label = "Search by area"),
                                        
                                        tags$br(),
                                        tags$br("After searching by category or area, enter the Meal ID here."),
                                        tags$br(),
                                        
                                        textInput(inputId = "searchMealId", label = "Enter Meal ID", value = "52772", placeholder = "Enter Meal ID here"),
                                        actionButton(inputId = "searchById", label = "Search by Id"),
                                        tags$br(),
                                        tags$br("Alternatively, search for a random meal here."),
                                        tags$br(),
                                        
                                    actionButton(inputId = "searchRandomMeal", label = "Search for a random meal")
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
                       tabPanel("Drink Generator",
                                titlePanel("Mix the perfect cocktail"),
                                sidebarLayout(
                                    sidebarPanel(
                                        selectInput(inputId = "drinkCatSelect", label = "Pick a drink category", choices = drinkCategories$drinks$strCategory),
                                        actionButton(inputId = "drinkCatSearch", label = "Search drink by category"),
                                        
                                        tags$br(),
                                        tags$br(),
                                        
                                        selectInput(inputId = "drinkIngredSelect", label = "Alternatively, Pick a drink ingredient", choices = drinkIngredients$drinks$strIngredient1),
                                        actionButton(inputId = "drinkIngredSearch", label = "Search drink by ingredient"),
                                        tags$br(),
                                        tags$h5("Let fate decide: "),
                                        actionButton(inputId = "randomDrinkSearch", label = "Search for a random drink")
                                        
                                        
                                    ),
                                    
                                    mainPanel(
                                        tableOutput("drinkCatSearchOut"),
                                        tableOutput("randomDrinkOut")
                                    )
                                )
                       ),
                       # end of cocktail panel
                       
                       # start of riot panel
                       tabPanel("League of Legends API",
                                titlePanel("Search for a player or the top 200 players."),
                                sidebarLayout(
                                    sidebarPanel(
                                        actionButton(inputId = "searchChal", label = "Show current leaderboards"),
                                        tags$br(),
                                        tags$br(),
                                        textInput(inputId = "searchPlayer", label = "Search player match history", value = "doublelift", placeholder = "Enter summoner name here"),
                                    ),
                                    mainPanel(
                                        tableOutput("lpOut"),

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
    
    # START OF MEAL API
    
    # Button to search meal by category 
    
    ## have to set our search to the category?, just use nested observeEvent i guess.
    
    observeEvent(input$catChoices, {
        getCategory <- str_glue("https://www.themealdb.com/api/json/v1/1/filter.php?c={categorySearch}",
                                categorySearch = input$catChoices)
        categoryCall <- GET(
            getCategory
        )
        #stop_for_status(categoryCall)
        catJson <- content(categoryCall, as = "text", encoding = "utf-8")
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
        areaJson <- content(areaCall, as = "text", encoding = "utf-8")
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
            mealIdJson <- content(mealIdCall, as = "text", encoding = "utf-8")
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
        randomMealJson <- content(randomMealCall, as = "text", encoding = "utf-8")
        randomMeal <- fromJSON(randomMealJson, flatten = TRUE)
        
        output$randomMealOut <- renderTable({
            randomMeal
        })
    })
    
    
    # end of random Meal 
    
    # END OF MEAL API 
    
    
    # Start of drinks API 
    
    # again, probably need to convert columns to rows or something.. 
    observeEvent(input$randomDrinkSearch, {
        getRandomDrink <- GET(
            "https://www.thecocktaildb.com/api/json/v1/1/random.php"
        )
        stop_for_status(getRandomDrink)
        randomDrinkJson <- content(getRandomDrink, as = "text", encoding = "utf-8")
        randomDrink <- fromJSON(randomDrinkJson, flatten = TRUE)
        
        output$randomDrinkOut <- renderTable({
            randomDrink
        })
    })
    
    # NOT READING SPACES PROPERLY... 
    #observeEvent(input$drinkCatSelect, {
        #getDrinksCat <- str_glue(
        #    "https://www.thecocktaildb.com/api/json/v1/1/filter.php?c={drinkcategory}",
        #    drinkcategory = input$drinkCatSelect
        #)
        
        #drinksCatCall <- GET(
        #    getDrinksCat
        #)
        #stop_for_status(drinksCatCall)
        #drinksCatJson <- content(drinksCatCall, as = "text", encoding = "utf-8")
        #catDrinks <- fromJSON(drinksCatJson, flatten = TRUE)
        
        #observeEvent(input$drinkCatSearch, {
        #    output$drinkCatSearchOut <- renderTable({
        #        catDrinks
        #    })
        #})
        
    #})
    # end of drinks API 
    
    # START OF RIOT GAMES API 
    getLeaderBoard <- str_glue(
        "https://na1.api.riotgames.com/lol/league/v4/challengerleagues/by-queue/RANKED_SOLO_5x5/?api_key={key}",
        key = Sys.getenv("riotKey")
    )
    
    leaderBoardCall <- GET(
        getLeaderBoard
    )
    stop_for_status(leaderBoardCall)
    json2 <- content(leaderBoardCall, as = "text", encoding = "utf-8")
    topRanks <- fromJSON(json2, flatten = TRUE)
    
    leaderBoard <- topRanks %>%
        as.data.frame %>%
        select(entries.summonerName, entries.leaguePoints) %>%
        arrange(desc(topRanks$entries$leaguePoints))
    
    output$summary <- renderPrint({
        summary(topRanks$entries$leaguePoints)
    })
    
    # observing button for current challengers
    observeEvent(input$searchChal, {
        output$lpOut <- renderTable({
            leaderBoard
        })
    })
    
        # end of current challengers
    
    # match history 
    
    # spacing problem, after fixing cocktail db should be able to fix this. 
    
    #getSummonerName <- str_glue(
    #    "https://na1.api.riotgames.com/lol/summoner/v4/summoners/by-name/bite/?api_key={key}",
    #    key = Sys.getenv("riotKey")
    #)
    
    #IGNCall <- GET(
    #    getSummonerName
    #)
    #stop_for_status(IGNCall)
    #IGNJson <- content(IGNCall, as = "text", encoding = "utf-8")
    # IGNReturn <- fromJSON(IGNJson, flatten = TRUE)
    # 
    # IGNReturn
    # 
    # getMatchHistory <- str_glue(
    #     "https://na1.api.riotgames.com/lol/match/v4/matchlists/by-account/{accId}/?api_key={key}",
    #     accId = IGNReturn$accountId,
    #     key = Sys.getenv("riotKey")
    # )
    # 
    # matchHistoryCall <- GET(
    #     getMatchHistory
    # )
    # stop_for_status(matchHistoryCall)
    # MHJson <- content(matchHistoryCall, as = "text", encoding = "utf-8")
    # MHReturn <- fromJSON(MHJson, flatten = TRUE)
    # 
    # MHReturn
    
    # end of match history 

    # END OF RIOT GAMES API
    
}

# Run the application 
shinyApp(ui = ui, server = server)
