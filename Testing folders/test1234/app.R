#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)


categories <- fromJSON("https://www.themealdb.com/api/json/v1/1/categories.php")

catChoices <- categories$categories$strCategory

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
  navbarPage(
    tabPanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    ),
    tabPanel("Meal generator",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "catChoices", label = "pick a category", choices = categories$categories$strCategory),
                 actionButton(inputId = "searchCat", label = "Search by category")
               ),
               mainPanel(
                 shinyUI(
                   DT::dataTableOutput("catSearchOut")
                 )
               )
             )
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    ## can have it listed out like how we did, just need to make the {catgoryValue} api reactive. 
  
      categorySearch <- reactive({
        
        
        
      getCategory <- str_glue("https://www.themealdb.com/api/json/v1/1/filter.php?c={categoryValue}",
                                categoryValue = input$catChoices)
        
              
      categoryCall <- GET(
        getCategory
      )

      stop_for_status(categoryCall)
      catJson <- content(categoryCall, as = "text", encoding = "utf-8")
      catMeal <- fromJSON(catJson, flatten = TRUE)
    })
    
    observeEvent(input$searchCat, { 
      output$catSearchOut <- DT::renderDataTable({
      # dependency on button search
      input$searchCat
      
      isolate(categorySearch()) %>%
        as.data.frame()
      })
    })
    
    # observeEvent(input$catChoices, {
    #   getCategory <- str_glue("https://www.themealdb.com/api/json/v1/1/filter.php?c={categorySearch}",
    #                           categorySearch = input$catChoices)
    #   categoryCall <- GET(
    #     getCategory
    #   )
    #   stop_for_status(categoryCall)
    #   catJson <- content(categoryCall, as = "text", encoding = "utf-8")
    #   catMeal <- fromJSON(catJson, flatten = TRUE)
    #   
    #   observeEvent(input$searchCat, {
    #     output$catSearchOut <- renderTable({
    #       catMeal %>%
    #         as.data.frame %>%
    #         select(meals.strMeal, meals.idMeal)
    #     })
    #   })
    # })
    
    searchMealId <- reactive({
      
      getMealById <- str_glue("https://www.themealdb.com/api/json/v1/1/lookup.php?i={mealId}",
                              mealId = input$searchMealId)
      
      mealIdCall <- GET(
        getMealById
      )
      stop_for_status(mealIdCall)
      mealIdJson <- content(mealIdCall, as = "text", encoding = "utf-8")
      MealIdMeals <- fromJSON(mealIdJson, flatten = TRUE)
      
    })
    #watching this button
    observeEvent(input$searchById, {
      output$mealIdSearchOut <- DT::renderDataTable({
        
        #dependency
        input$searchById
        
        isolate(searchMealId()) %>%
          as.data.frame
      })
    })
    
    # START OF RIOT GAMES API 
    # getLeaderBoard <- str_glue(
    #     "https://na1.api.riotgames.com/lol/league/v4/challengerleagues/by-queue/RANKED_SOLO_5x5/?api_key={key}",
    #     key = Sys.getenv("riotKey")
    # )
    # 
    # leaderBoardCall <- GET(
    #     getLeaderBoard
    # )
    # stop_for_status(leaderBoardCall)
    # json2 <- content(leaderBoardCall, as = "text", encoding = "utf-8")
    # topRanks <- fromJSON(json2, flatten = TRUE)
    # 
    # leaderBoard <- topRanks %>%
    #     as.data.frame %>%
    #     select(entries.summonerName, entries.leaguePoints) %>%
    #     arrange(desc(topRanks$entries$leaguePoints))
    # 
    # output$summary <- renderPrint({
    #     summary(topRanks$entries$leaguePoints)
    # })
    # 
    # # observing button for current challengers
    # observeEvent(input$searchChal, {
    #     output$lpOut <- renderTable({
    #         leaderBoard
    #     })
    # })
    
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
    
    output$randomMealOut <- DT::renderDataTable({
      DT::datatable(randomMeal %>%
                      as.data.frame %>%
                      mealThumb <- tags$img(meals.strMealThumb) %>%
                      select(meals.idMeal, meals.strMeal, meals.strCategory, meals.strArea, meals.strMealThumb, meals.strTags, meals.strYoutube, meals.strSource), escape = FALSE)
    })
}




# Run the application 
shinyApp(ui = ui, server = server)

