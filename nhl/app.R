#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(shinythemes)
library(shinyWidgets)
library(ggrepel)
library(tidyverse)
library(shinydashboard)
library(forcats)

teams <- read_rds("teams.rds")
players <- read_rds("players.rds")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = 'black',
      dashboardHeader(title = "NHL Statistics"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Home", tabName = "home"),
          menuItem("Team Stats", tabName = "teams"),
          menuItem("Player Stats", tabName = "players")
        )
      ),
      dashboardBody(
        tabItems(
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
          tabItem(tabName = "teams",
                  h1("Team Data"),
                  selectInput("y",
                              "Statistic:",
                              choices = c("Goals For" = "goals_for",
                                          "Goals Against" = "goals_against",
                                          "Face-Off Win %" = "fow_perc",
                                          "Shots For" = "shots_per_gp",
                                          "Shots Against" = "shots_against_per_gp",
                                          "Powerplay %" = "powerplay_perc",
                                          "Penalty Kill %" = "penalty_kill")),
                  sliderInput("season", 
                              "Season(s)",
                              min = 1917,
                              max = 2019,
                              value = c(1917,2019)
                              ),
                  plotOutput("TeamPlot")
                              ),
          tabItem(tabName = "players",
                  h1("Player Data"),
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
                                        plotOutput("PlayerPlot")
                  )
        )
      )
                      )

# Define server logic required to draw a histogram
server <- function(input, output) {
  teamreact <- reactive({
    teams %>% 
      filter(start >= input$season[1] & end <= input$season[2]) %>% 
      group_by(team) %>% 
      summarize(sum = sum(!! rlang:: sym(input$y))) %>% 
      arrange(desc(sum)) %>% 
      mutate(team = fct_reorder(team,sum))
  })
  playerreact <- reactive({
    players %>%
      filter(team == input$team1) %>%
      group_by(name) %>% 
      summarize(total = sum(!! rlang:: sym(input$z))) %>%
      arrange(desc(total)) %>% 
      slice(1:20)
  })
  
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
