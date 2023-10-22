library(shiny)
library(bslib)
source("./helpers.R")

# ui <- fluidPage(
ui <- navbarPage("Kobe Vis",
  theme = bs_theme(version = 4, bootswatch = "minty"),
  tabPanel("Population",
     sidebarLayout(
      sidebarPanel(
       selectInput(inputId = "target_areas",
                   label = "表示する地域を選択",
                   choices = list("全市",
                                  "東灘区",
                                  "灘区",
                                  "中央区",
                                  "兵庫区",
                                  "長田区",
                                  "垂水区",
                                  "北区",
                                  "須磨区",
                                  "西区"),
                   selected = "全市",
                   multiple = FALSE
      ),
      sliderInput("year", "Year:",
                  min = 2014, max = 2023,
                  value = 2023, step = 1,
                  sep="",
                  animate =
                    animationOptions(interval = 1, loop = TRUE)
      ),
      sliderInput("month", "Month:",
                  min = 1, max = 9,
                  value = 1, step = 1,
                  sep="",
                  animate =
                    animationOptions(interval = 3000, loop = TRUE)
      ),
      checkboxGroupInput("checkbox_people", "Attribute of people:",
                         c("Japanese" = "japanese",
                           "Foreiners" = "foreigners"),
                         selected=c("japanese", "foreigners")
      ),
      checkboxGroupInput("checkbox_gender", "Gender:",
                         c("Male" = "male", "Female" = "female"),
                          selected=c("male", "female")
      ),
     ),
    
     mainPanel(
       # plotOutput(outputId = "barPopulation"),
       # plotOutput(outputId = "map_all_wards"),
       # plotOutput(outputId = "map_population"),
       # plotOutput(outputId = "map_population_density"),
       # plotOutput(outputId = "map_location_city_ward_offices")
       plotOutput(outputId = "map_population_by_year_month"),
       plotOutput(outputId =  "plot_bar_population_year_month")
     )
   )
  ),
  tabPanel("Nursing"),
  tabPanel("Education"),
  navbarMenu("City Administration",
   tabPanel("panel 4a", "four-a"),
   tabPanel("panel 4b", "four-b"),
   tabPanel("panel 4c", "four-c")
  )
)

server <- function(input, output) {
  # bs_themer()
  
   output$barPopulation <- renderPlot({
    plot_bar_population(input$target_areas)
  })
  
  output$map_all_wards <- renderPlot({
   map_all_wards(input$target_areas)
  })

  output$map_population <- renderPlot({
    map_population(input$target_areas)
  })

  output$map_population_density <- renderPlot({
    map_population_density(input$target_areas)
  })

  output$map_location_city_ward_offices <- renderPlot({
    map_location_city_ward_offices(input$target_areas)
  })
  
  output$plot_bar_population_year_month <- renderPlot({
    plot_bar_population_year_month(input$target_areas, input$year, input$month, input$checkbox_people, input$checkbox_gender)
  })
  
  
  output$map_population_by_year_month <- renderPlot({
    map_population_by_year_month(input$target_areas, input$year, input$month, input$checkbox_people, input$checkbox_gender)
  })
}

shinyApp(ui = ui, server = server)