#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load in my libraries

library(shiny)
library(plotly)
library(dplyr)
library(shinythemes)
library(shinyWidgets)
library(ggrepel)
library(tidyverse)
library(shinydashboard)
library(forcats)

# Load in the cleaned data into Shiny

teams <- read_rds("teams.rds")
players <- read_rds("players.rds")

# Define UI for application
# Use a black theme because that is the NHL's color in their logo

ui <- dashboardPage(skin = 'black',
                    
      # Add the title of my application
      
      dashboardHeader(title = "NHL Statistics"),
      
      # Create the sidebar navigation
      
      dashboardSidebar(
        sidebarMenu(
          menuItem("Home", tabName = "home"),
          menuItem("Team Stats", tabName = "teams"),
          menuItem("Player Stats", tabName = "players")
        )
      ),
      dashboardBody(
        tabItems(
          
          # Creating the About page in my dashboard
          
          tabItem(tabName = "home",
                  h1("About the project"),
                  p("The National Hockey League (NHL) recently published the complete
                    digitization of their statistical records, dating back 100 years to
                    the first season in 1917-1918. This data includes team statistics and
                    player statistics as well as visualization and filtering tools at",
                    tags$a("www.nhl.com", href = "http://www.nhl.com/stats/"),". The page includes more than
                    15 million player and game data points, with those from 1917 to 1987
                    previously unavailable. The data points range from more typical ice
                    hockey statistics such as goals, assists, and points to penalty minutes,
                    shot percentage, and time on ice."),
                    br(),
                    p("The data is not available for download
                    on their website, so I had to extract the data from the website using
                    webscraping software from", tags$a("webscraper.io", href = "https://webscraper.io/"),". The
                    website includes tutorials for how to navigate different website layouts,
                    so I essentially followed those instructions and solved issues with trial
                    and error. This software is an extension for Google Chrome and allows
                    you to select the data on the website you’re interested in, and then
                    it navigates multiple pages of the site until it has extracted all of
                    the data. Once the data has been extracted, I am able to download it as
                    a csv file which R is able to understand. One complication I faced was
                    the sheer amount of data; my computer was not able to extract it all at
                    once. I had to section the data by year, into 25 year increments so that
                    my computer could extract the data in a reasonable amount of time."),
                    br(),
                    p("I love
                    the idea of using analytics in sports because it ramps up the competition
                    factor; each team is trying to get an advantage over the others in any
                    way that they can, and turning to analytics can allow you to view the data
                    in a new and potentially illuminating way. Analytics is increasingly being
                    applied to sports; teams in the MLB have large sabermetrics and statistics
                    departments, for example. I’m on the women’s lacrosse team at Harvard and
                    we even have a mathematician who evaluates our practices!"),
                    br(),
                    p("I decided to
                    choose NHL data for my project because I have grown up watching the
                    Philadelphia Flyers and attending games with my family. I really wanted
                    to apply this type of analysis to a team I’ve followed my whole life to
                    perhaps see how their goals, power plays, save percentage, and more have
                    had an effect (if any) on their record and standings over the years.")
          ),
          
          # Create the team data page
          
          tabItem(tabName = "teams",
                  h1("Team Data"),
                  
          # Add a drop down bar for which statistic the user wants to view
          
                  selectInput("y",
                              "Statistic:",
                              choices = c("Goals For" = "goals_for",
                                          "Goals Against" = "goals_against",
                                          "Face-Off Win %" = "fow_perc",
                                          "Shots For" = "shots_per_gp",
                                          "Shots Against" = "shots_against_per_gp",
                                          "Powerplay %" = "powerplay_perc",
                                          "Penalty Kill %" = "penalty_kill")),
            
          # Add a slider that allows the user to select a season start and end
          # Right now the years have commas in them -- still trying to figure
          # out how to fix that.
          
                  sliderInput("season", 
                              "Season(s)",
                              min = 1917,
                              max = 2019,
                              value = c(1917,2019)
                              ),
          
          # Plot my graph of the statistic vs. team
          
                  plotOutput("TeamPlot")
                              ),
          
          # Create player data page
          
          tabItem(tabName = "players",
                  h1("Player Data"),
            
          # Allow user to select a team to view player stats for
          # In my final project I might allow the user to view more than
          # one team at once. I also want the user to be able to select
          # either a current NHL team or a past NHL team, but couldn't 
          # figure out how to get them both working simultaneously yet.
                  
                  selectInput("team1",
                              "Select Current NHL Team:",
                              choices = c("Anaheim Ducks" = "ANA",
                                          "Arizona Coyotes" = "ARI",
                                          "Boston Bruins" = "BOS",
                                          "Buffalo Sabres" = "BUF",
                                          "Carolina Hurricanes" = "CAR",
                                          "Columbus Blue Jackets" = "CBJ",
                                          "Calgary Flames" = "CGY",
                                          "Chicago Blackhawks" = "CHI",
                                          "Colorado Avalanche" = "COL",
                                          "Dallas Stars" = "DAL",
                                          "Detroit Red Wings" = "DET",
                                          "Edmonton Oilers" = "EDM",
                                          "Florida Panthers" = "FLA",
                                          "Los Angelos Kings" = "LAK",
                                          "Minnesota Wild" = "MIN",
                                          "Montreal Canadiens" = "MTL",
                                          "New Jersey Devils" = "NJD",
                                          "Nashville Predators" = "NSH",
                                          "New York Islanders" = "NYI",
                                          "New York Rangers" = "NYR",
                                          "Ottawa Senators" = "OTT",
                                          "Philadelphia Flyers" = "PHI",
                                          "Pittsburgh Penguins" = "PIT",
                                          "San Jose Sharks" = "SJS",
                                          "St. Louis Blues" = "STL",
                                          "Tampa Bay Lightening" = "TBL",
                                          "Toronto Maple Leafs" = "TOR",
                                          "Vancouver Canucks" = "VAN",
                                          "Vegas Golden Knights" = "VGK",
                                          "Winnipeg Jets" = "WPG",
                                          "Washington Capitals" = "WSH")),
                  selectInput("team",
                            "Select Past NHL Team:",
                            choices = c("Toronto Arenas",
                                        "Montreal Canadiens",
                                        "Ottawa Senators (1917)",
                                        "Montreal Wanderers",
                                        "Toronto St. Patricks",
                                        "Quebec Bulldogs",
                                        "Hamilton Tigers",
                                        "Montreal Maroons",
                                        "New York Americans",
                                        "Pittsburgh Pirates",
                                        "Detroit Cougars",
                                        "Detroit Falcons",
                                        "Philadelphia Quakers",
                                        "St. Louis Eagles",
                                        "Brooklyn Americans",
                                        "Minnesota North Stars",
                                        "Oakland Seals",
                                        "California Golden Seals",
                                        "Atlanta Flames",
                                        "Kansas City Scouts",
                                        "Cleveland Barons",
                                        "Colorado Rockies",
                                        "Hartford Whalers",
                                        "Quebec Nordiques",
                                        "Winnipeg Jets (1979)",
                                        "Phoenix Coyotes",
                                        "Atlanta Thrashers")),
          
            # Make a drop down bar of the statistic the user wants to 
            # look at
          
                      selectInput("z",
                                  "Statistic:",
                                  choices = c("Goals" = "goals",
                                              "Assists" = "assists",
                                              "Points" = "points",
                                              "Plus-Minus" = "plus_minus",
                                              "Penalty Minutes" = "pim",
                                              "Points per Game Played" = "points_per_gp",
                                              "Power Play Goals" = "ppg",
                                              "Shorthanded Goals" = "shg",
                                              "Shorthanded Points" = "shp",
                                              "Game Winning Goals" = "gwg",
                                              "Overtime Goals" = "otg",
                                              "Shots" = "shots",
                                              "Shot %" = "shot_perc",
                                              "Time on Ice per Game Played" = "toi_gp",
                                              "Shifts per Game Played" = "shifts_gp",
                                              "Faceoff Win %" = "fow_perc"
                                              )),
            # Plot the graph of my statistic vs. players for a certain team
          
                                        plotOutput("PlayerPlot")
                  )
        )
      )
                      )

# Define server logic required to make my application

server <- function(input, output) {
  
  # define my data as reactive so it can take the inputs of the 
  # user into the plots
  
  teamreact <- reactive({
    teams %>% 
      
      # Filter for the season the user inputs. Group by the team and sum
      # the selected statistic for each team. 
      
      filter(start >= input$season[1] & end <= input$season[2]) %>% 
      group_by(team) %>% 
      summarize(sum = sum(!! rlang:: sym(input$y))) %>% 
      arrange(desc(sum)) %>% 
      mutate(team = fct_reorder(team,sum))
  })
  playerreact <- reactive({
    players %>%
      
      # Filter for the team the user inputs. Group by the player and sum
      # the selected statistic for each player. 
      
      filter(team == input$team1) %>%
      group_by(name) %>% 
      summarize(total = sum(!! rlang:: sym(input$z))) %>%
      arrange(desc(total)) %>% 
      slice(1:20)
  })
  
      # Make the ggplot for the specifications the user selected.
      # Rotate the x axis team/player names so they're legible.
      # Add axes labels.
  
    output$TeamPlot <- renderPlot({
        teamreact() %>% 
        ggplot(aes(x = team, y = sum)) +
        geom_col() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("Team") +
        ylab("Statistic")
    })
    
    output$PlayerPlot <- renderPlot({
      playerreact() %>%
        ggplot(aes(x = name, y = total)) +
        geom_col() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("Player") +
        ylab("Statistic")
    })
    
}

# Run the application 

shinyApp(ui = ui, server = server)
