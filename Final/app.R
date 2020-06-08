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
library(dplyr)
library(shiny)
library(usethis)
library(DT)
library('rsconnect')


# loading in api keys 
# usethis::edit_r_environ("project")
# readRenviron(".Renviron")

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
                                tags$h3("Eric Gip 141B Final"),
                                tags$h3("About this project"),
                                tags$p("This Shiny app takes advantage of the TheMealDB API and TheCocktailDB API."),
                                tags$p("You're able to search for a meal Id by category or area then search for the meal ID to get a YouTube tutorial or recipe source to make it at home. You can also just search for a random meal. If the API does not contain a tutorial, it is still a great tool for referencing and choosing a meal."),
                                tags$p("For a drink / cocktail, you're able to search for an ID by a category or ingredient, then enter the ID to find the drink name, serving glass, and instructions. Disclaimer: I didn't make the database and not responsible for cocktail names (see Other/Unknown category for a laugh)"),
                                tags$a("https://www.themealdb.com/api.php", "https://www.thecocktaildb.com/api.php"),
                                tags$h3("Challenging and enjoyable parts of the project"),
                                tags$p("This was challenging because I had to learn a completely new framework and work in a statistical language, R, that isn't tradionally used for web development. I'm amazed at how far you can push DOM manipulation and create a site without having to learn 3 different languages (HTML, CSS, JS). However, after learning the Shiny framework I have a new appreciation for front-end development and will never take Bootstrap/React for granted ever again... I'm really amazed that these two panels took almost 400 lines of code."),
                                tags$p("I had another panel for the Riot Games API, but unforunately it had no place here; you can see the finished, but unused source code in the other shiny app folders."),
                                tags$h3("Why I chose these APIs"),
                                tags$p("Since this was an individual project instead of a group project, I embraced the freedom given and added all the APIs that interested me. I enjoy cooking, but am often as a loss at what to cook; this API allows me to easily pick a dinner with a drink on the side."),
                                tags$h3("Future improvements"),
                                tags$p("I would like to implement the meal or drink ids to always be present on the right after a search, or maybe even appear as a drop down selection into the API's output of meal ids. Also, there's an issue with white spaces on the Id searches.")
                       ),
                       
                       #end of about panel
                       
                       # start of meal panel
                       tabPanel("Meal Generator", 
                                # Application title
                                titlePanel("Pick a Meal"),
                                
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
                                        
                                        textInput(inputId = "searchMealId", label = "Enter Meal ID", placeholder = "Please make sure there's no whitespace"),
                                        actionButton(inputId = "searchById", label = "Search by Id"),
                                        tags$br(),
                                        tags$br("Alternatively, search for a random meal here."),
                                        tags$br(),
                                        
                                    actionButton(inputId = "searchRandomMeal", label = "Search for a random meal")
                                    ),
                                
                                    
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                        # All of our buttons update`catSearchOut`, instead of their own respective tables.
                                        # if they have their own respective tables, they stack on each other. 
                                      
                                        # going to make mealId appear on top, and push the search tables down to keep the IDs open. 
                                        DT::dataTableOutput("mealIdSearchOut"),
                                        DT::dataTableOutput("catSearchOut")
                                        #DT::dataTableOutput("areaSearchOut"),
                                        #DT::dataTableOutput("randomMealOut")
                                        
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
                                        tags$br(),
                                        
                                        textInput(inputId = "drinkIdInput", label = "Enter Drink ID here!", placeholder = "Please mind the whitespace!"),
                                        
                                        actionButton(inputId = "drinkIdSearch", label = "Search drink by ID"),
                                        
                                        tags$h5("Let fate decide: "),
                                        actionButton(inputId = "randomDrinkSearch", label = "Search for a random drink")
                                        
                                    ),
                                    
                                    mainPanel(
                                        # drink ID on top, 
                                        # all drink outputs plug into drinkCatSearchOut to have all id's showing.
                                        DT::dataTableOutput("drinkIdOut"),
                                        DT::dataTableOutput("drinkCatSearchOut")
                                        #DT::dataTableOutput("IngredDrinkOut"),
                                        #DT::dataTableOutput("randomDrinkOut")
                                    )
                                )
                       )
                       # end of cocktail panel
                       
                       ## end of shinyUI
    ))
    
    # end of fluid page
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # START OF MEAL API
    
    # Button to search meal by category 
    
    ## have to set our search to the category?, just use nested observeEvent i guess.
    # guess we shouldnt do that

  
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
      
      DT::datatable(isolate(categorySearch()) %>%
        as.data.frame() %>%
          select(meals.strMeal, meals.idMeal),
        colnames = c("Meal Names", "Meal Ids")
      )
    })
  })
    
    
  
    # end of category search 
    
    # button to search by area
    
    ## TO DO -- MAYBE SHOW PICTURES
    areaSearch <- reactive({
        getArea <- str_glue("https://www.themealdb.com/api/json/v1/1/filter.php?a={area}",
                            area = input$areaChoices)
        
        areaCall <- GET(
            getArea
        )
        stop_for_status(areaCall)
        areaJson <- content(areaCall, as = "text", encoding = "utf-8")
        areaMeals <- fromJSON(areaJson, flatten = TRUE)
    })
      
        #setting to catSeach so the table updates, instead of stacking tables.
        observeEvent(input$searchArea, {
            output$catSearchOut <- DT::renderDataTable({
                input$searchArea
              
              DT::datatable(isolate(areaSearch()) %>%
                  as.data.frame() %>% 
                  select(meals.strMeal, meals.idMeal), 
                  colnames = c("Meal Name", "Meal ID")
              )
            })
        })
    
    # end of button to search by area
    
    # button to search by meal ID
    
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
      # set to catSearchOut to update table, set to IdSearchOut if you want tables stacking. 
      output$mealIdSearchOut <- DT::renderDataTable({
        
        #dependency
        input$searchById
        
        DT::datatable(isolate(searchMealId()) %>%
          as.data.frame %>%
            select(meals.idMeal, meals.strMeal, meals.strCategory, meals.strArea, meals.strTags, meals.strYoutube, meals.strSource), 
          colnames = c("Meal ID", "Meal Name", "Category", "Area", "Tags", "Youtube Tutorial", "Recipe Source")
      )
    })
  })    
    
    # button to generate a random meal
    
    observeEvent(input$searchRandomMeal, {
        randomMealCall <- GET(
            "https://www.themealdb.com/api/json/v1/1/random.php"
        )
        stop_for_status(randomMealCall)
        randomMealJson <- content(randomMealCall, as = "text", encoding = "utf-8")
        randomMeal <- fromJSON(randomMealJson, flatten = TRUE)
        
        # setting to catSearchOut to replace table instead of stack on top of it. 
        output$catSearchOut <- DT::renderDataTable({
          DT::datatable(randomMeal %>%
                          as.data.frame %>%
                          select(meals.idMeal, meals.strMeal, meals.strCategory, meals.strArea, meals.strTags, meals.strYoutube, meals.strSource), 
                        colnames = c("Meal ID", "Meal Name", "Category", "Area", "Tags", "Youtube Tutorial", "Recipe Source"))
        })
    })
    
    
    # end of random Meal 
    
    # END OF MEAL API 
    
    
    # Start of drinks API 
    
    
    # start of drinkCategory 
    # FIXED --- NOT READING SPACES PROPERLY... -- Just URLEencode() AFTER str_glue()
    drinkCatSelect <- reactive({
        getDrinksCatRAW <- str_glue(
            "https://www.thecocktaildb.com/api/json/v1/1/filter.php?c={drinkcategory}",
            drinkcategory = input$drinkCatSelect
        )
        # encoding the space hopefully
        getDrinksCat <- URLencode(getDrinksCatRAW)
        
        drinksCatCall <- GET(
            getDrinksCat
        )
        stop_for_status(drinksCatCall)
        drinksCatJson <- content(drinksCatCall, as = "text", encoding = "utf-8")
        catDrinks <- fromJSON(drinksCatJson, flatten = TRUE)
        })
    
    observeEvent(input$drinkCatSearch, {
      output$drinkCatSearchOut <- DT::renderDataTable({
        
        input$drinkCatSearch
        
        DT::datatable(isolate(drinkCatSelect()) %>%
          as.data.frame %>% 
            select(drinks.strDrink, drinks.idDrink), 
            colnames = c("Drink Name", "Drink Id")
        )
      })
    })
    # end of drinksCategory       
  
    # start of drinksIngredient
    
    drinksIngredSelect <- reactive({
      getDrinkByIngredRAW <- str_glue("https://www.thecocktaildb.com/api/json/v1/1/filter.php?i={ingred}",
                                      ingred = input$drinkIngredSelect)
      getDrinkByIngred <- URLencode(getDrinkByIngredRAW)
      
      drinksIngredCall <- GET(
        getDrinkByIngred
      )
      stop_for_status(drinksIngredCall)
      drinksIngredJson <- content(drinksIngredCall, as = "text", encoding = "utf-8")
      drinksIngredDrinks <- fromJSON(drinksIngredJson, flatten = TRUE)
    })
    
    observeEvent(input$drinkIngredSearch, {
      # again, outputting into a single table to update as each button is clicked instead of stacking tables on top of each other.
      output$drinkCatSearchOut <- DT::renderDataTable({
        #dependency
        input$drinkIngredSearch
        
        DT::datatable(isolate(drinksIngredSelect()) %>%
          as.data.frame() %>%
            select(drinks.strDrink, drinks.idDrink),
          colnames = c("Drink Name", "Drink ID")
        )
      })
    })
    
    # end of drinksIngredient
    
    # start of drinkIdSearch
    
    drinkIdSearch <- reactive({
      getDrinkByIdRAW <- str_glue("https://www.thecocktaildb.com/api/json/v1/1/lookup.php?i={drinkId}",
                                  drinkId = input$drinkIdInput)
      
      getDrinkById <- URLdecode(getDrinkByIdRAW)
      
      drinkIdCall <- GET(
        getDrinkById
      )
      stop_for_status(drinkIdCall)
      drinkIdJson <- content(drinkIdCall, as = "text", encoding = "utf-8")
      drinkIdDrinks <- fromJSON(drinkIdJson, flatten = TRUE) 
    })
    
    observeEvent(input$drinkIdSearch, {
      # not putting this into a single table because I want all meal Ids showing. 
      output$drinkIdOut <- DT::renderDataTable({
        #dependency 
        input$drinkIdSearch
        
        DT::datatable(isolate(drinkIdSearch()) %>%
                        as.data.frame %>%
                        select(drinks.idDrink, drinks.strDrink, drinks.strCategory, drinks.strAlcoholic, drinks.strGlass, drinks.strInstructions),
                      colnames = c("Drink ID", "Drink Name", "Drink category", "Is this alcoholic?", "Serving Glass", "Drink Instructions")
        
        )
      })
    })
    
    # end of drinkIdSearch
    
    # start of random drink
    # again, probably need to convert columns to rows or something.. 
    observeEvent(input$randomDrinkSearch, {
        getRandomDrink <- GET(
            "https://www.thecocktaildb.com/api/json/v1/1/random.php"
        )
        stop_for_status(getRandomDrink)
        randomDrinkJson <- content(getRandomDrink, as = "text", encoding = "utf-8")
        randomDrink <- fromJSON(randomDrinkJson, flatten = TRUE)
        
        ## again, outputting into single table.
        output$drinkCatSearchOut <- DT::renderDataTable({
            DT::datatable(randomDrink %>%
            as.data.frame %>%
            select(drinks.idDrink, drinks.strDrink, drinks.strTags, drinks.strCategory, drinks.strAlcoholic, drinks.strGlass, drinks.strInstructions), 
          colnames = c("Drink ID", "Drink Name", "Tags", "Category", "Is this alcoholic?", "Serving Glass", "Drink Instructions")
            )
        })
    })
    
    # end of randomDrinks
    
    # end of drinks API 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
