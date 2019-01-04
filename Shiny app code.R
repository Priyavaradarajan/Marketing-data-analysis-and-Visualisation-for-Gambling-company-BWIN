library(shiny)
library(ggplot2)
library(dplyr)

########Buiulding Shiny App for the Sports Gambling activity########
#Reading the data

load("bastetable.Rdata")

# Define UI for application that plots the analytics of Gambling data
ui <- fluidPage(
  
  # Application title
  titlePanel("Analysis of Internet Gambling Activity across all Sports"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      #Explanatory text for the sidebar panel for sports
      
      HTML(paste0("X and Y axis selection is only applicable for the Sports(FO and LA),
                  You can chose your variables for plotting the graph for insights.The check boxes for Gender and
                  Continent can be used across the tabs for all graphs.")),
      
      # Break for visual separation
      br(), br(),
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("FOAvgStake", "LAAvgStake", "FOAvgTotalWinnings", "LAAvgTotalWinnings",
                              "FOTotalActiveDays", "LATotalActiveDays", "Recency_bet_sportsFO", "Recency_bet_sportsLA",
                              "Gender","Continent"), 
                  selected = "FOTotalActiveDays"),
      
      
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("FOAvgStake", "LAAvgStake", "FOAvgTotalWinnings", "LAAvgTotalWinnings",
                              "FOTotalActiveDays", "LATotalActiveDays", "Recency_bet_sportsFO", "Recency_bet_sportsLA",
                              "Gender","Continent"), 
                  selected = "FOAvgStake"),
      
      # Select variable for color
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("Gender", "Continent"),
                  selected = "Gender"),
      
      # Set alpha level
      sliderInput(inputId = "alpha", 
                  label = "Alpha:", 
                  min = 0, max = 1, 
                  value = 0.5),
      
      #selecting the check box for male and female
      checkboxGroupInput(inputId = "gender_type",
                         label = "Select gender:",
                         choices = c("Male", "Female"),
                         selected = "Male"),
      
      #selecting the check box for continents
      checkboxGroupInput(inputId = "continent_type",
                         label = "Select Continent:",
                         choices = c("Europe","Asia","Oceania","Others","Americas","Africa"),
                         selected = "Europe")

   ),
  
    
    # Outputs
    mainPanel(
      # Tab 1:Sports data
      
      tabsetPanel(type = "tabs",
                  tabPanel("Sports",  
                           h3("Analysis of the variables in Sports(FO and LA)"), 
                           plotOutput(outputId = "scatterplot"),
                           plotOutput(outputId = "hist4"),
                           plotOutput(outputId = "hist5"),
                           br()),
      # Tab 2: Sports(Stakes VS Age)
      
           tabPanel("Sports(StakesVSAge)", 
                 h3("Sports(FO,LA) Average Stakes & Winnings VS Age"), 
                 plotOutput(outputId = "plot1"),
                 plotOutput(outputId = "plot3"),
                 textOutput(outputId = "description"),
                 br()),
                  
       # Tab 3:Poker
      
                tabPanel("Poker",
                  h3("Poker Analysis"), 
                 plotOutput(outputId = "plot"),
                 plotOutput(outputId = "hist3"),
                 br()),
                              
      
      # Tab 4: Casino
          tabPanel("Casino",
                   h3("Casino Analysis"), 
                           plotOutput(outputId = "scatterplot1"),
                           plotOutput(outputId = "hist"),
                           br()),
      # Tab 5: Games
         tabPanel("Games",
                  h3("Games Analysis"), 
         plotOutput(outputId = "scatterplot2"),plotOutput(outputId = "hist1"),br()),

    # Tab 6: Supertoto
          tabPanel("Supertoto",
                   h3("Supertoto Analysis"), 
         plotOutput(outputId = "scatterplot3"),plotOutput(outputId = "hist2"),br()),
    
    # Tab 7: Demographics
    tabPanel("Demographics",
             plotOutput(outputId = "demoplot"),plotOutput(outputId = "demoplot1"),
             textOutput(outputId = "description1"),
             br())
    )
  )
)
)

####Server function details
# Define server function required to create the scatterplot
server <- function(input, output) {
  
  
  #Creating a subset for filtering the gender and continent checkboxes across all the sports tabs using Reactive
  
  Casinosubset <- reactive ({
    req(input$gender_type)
    req(input$continent_type)
    Casino %>% filter(Gender %in% input$gender_type & Continent %in% input$continent_type)
  })
  
  Gamessubset <- reactive ({
    req(input$gender_type)
    req(input$continent_type)
    Games %>% filter(Gender %in% input$gender_type & Continent %in% input$continent_type)
  })
  
  Supertotosubset <- reactive ({
    req(input$gender_type)
    req(input$continent_type)
    Supertoto %>% filter(Gender %in% input$gender_type & Continent %in% input$continent_type)
  })
  
  Sportssubset <- reactive ({
    req(input$gender_type)
    req(input$continent_type)
    Sports %>% filter(Gender %in% input$gender_type & Continent %in% input$continent_type)
  })
  
  Pokersubset <- reactive ({
    req(input$gender_type)
    req(input$continent_type)
    Poker %>% filter(Gender %in% input$gender_type & Continent %in% input$continent_type)
  })
  
  ####Creating output plots across all the tabs
  
  #create Plot for casino
  output$scatterplot1 <- renderPlot({
    ggplot(Casinosubset(), aes(x=Casino_mean_bets, y=Casino_mean_Winnings)) +  geom_point() +
      ggtitle("Casino Bets VS Winnings") +
      theme(plot.title = element_text(size=14, face="bold",hjust = 0.5))
  })
  
  output$hist <- renderPlot({
    hist(Casino$Casino_LOS,col = "blue",main="Duration of Casino Users Since Last Bet")
  })
  
  #create the  plot for Games
  output$scatterplot2 <- renderPlot({
    ggplot(Gamessubset(), aes(x=Games_mean_Stakes, y=Games_mean_Winnings)) + 
      geom_point() + ggtitle("Games Stakes VS Winnings") +
      theme(plot.title = element_text(size=14, face="bold",hjust = 0.5))
  })
  
  output$hist1 <- renderPlot({
    hist(Games$Games_LOS,col="blue",main="Duration of Games Users Since Last Bet")
  })
  
  #create the  plot for Supertoto
  output$scatterplot3 <- renderPlot({
    ggplot(Supertotosubset(), aes(x=Supertoto_max_stakes, y=Supertoto_mean_Winnings)) + 
      ggtitle("Supertoto Stakes VS Winnings")+ geom_point() + theme(plot.title = element_text(size=14, face="bold",hjust = 0.5))
  })
  
  output$hist2 <- renderPlot({
    hist(Supertoto$Supertoto_LOS,col="blue",main="Duration of Supertoto Users Since Last Bet")
  })
  
  
  #Create a line plot for poker
  output$plot <- renderPlot({
    ggplot(data = Pokersubset(), aes(x = Profit_Poker)) + geom_line(aes(y = Poker_Sell,colour = "Poker_Sell")) +
      geom_line(aes(y = Poker_Buy,colour = "Poker_Buy")) + ggtitle("Poker Analysis of Profit VS Buy/Sell") +
      theme(plot.title = element_text(size=14, face="bold",hjust = 0.5))
    
  })
  
  
  output$hist3 <- renderPlot({
    hist(Poker$Recency_Poker,col="blue",main="Recency of Users Playing Poker")
  })
  
  
  #Create plot for Sports
  output$scatterplot <- renderPlot({
    ggplot(data = Sportssubset(), aes_string(x = input$x, y = input$y,
                                             color = input$z)) +
      geom_point(alpha=input$alpha) + facet_grid(cols = vars(Continent)) + theme_bw()
  })
  
  output$hist4 <- renderPlot({
    hist(Sports$Recency_bet_sportsFO,col="blue",main="Recency of Users in Sports FO")
  })
  
  output$hist5 <- renderPlot({
    hist(Sports$Recency_bet_sportsLA,col="blue",main="Recency of Users in Sports LA")
  })
  
  #Rendering text output for Sports
  output$description<- renderText({
    paste0("The plot above visualizes the relationship between FO and LA 
           Average stakes and Winnings across all the Age groups.The inference is that 
           people of age groups between 25 to 55 are the ones who have played the most.")
  })
  
  #Create plot for Sports(StakeVSAge)
  output$plot1 <- renderPlot({
    ggplot(data = Sportssubset(), aes(x = AGE)) + geom_line(aes(y = FOAvgStake,colour = "FOAvgStake" )) +
      geom_line(aes(y = FOAvgTotalWinnings,colour = "FOAvgTotalWinnings" )) +
      ggtitle("FO Average Stakes & Winnings VS Age") + 
      theme(plot.title = element_text(size=14, face="bold",hjust = 0.5))
    
  })
  output$plot3 <- renderPlot({
    ggplot(data = Sportssubset(), aes(x = AGE)) + geom_line(aes(y = LAAvgStake,colour = "LAAvgStake" )) +
      geom_line(aes(y = LAAvgTotalWinnings,colour = "LAAvgTotalWinnings" )) +
      ggtitle("LA Average Stakes & Winnings VS Age") + 
      theme(plot.title = element_text(size=14, face="bold",hjust = 0.5))
  })
  
  #Create a demographic plot
  output$demoplot <- renderPlot({
    barplot(table(Demographic$Continent),col="blue",main="No of Players across Continents")
  })
  
  output$demoplot1 <- renderPlot({
    barplot(table(Demographic$Gender),col="blue",main= "Male VS Female Players")
  })
  
  #Rendering text output for Demographics
  output$description1<- renderText({
    paste0("The above plots for overall demographics of the Internet Gambling activity shows us that
             there were more Male players than Female and most number of people who played were from
             Europe.")
  })
  
  }


# Create a Shiny app object
shinyApp(ui = ui, server = server)