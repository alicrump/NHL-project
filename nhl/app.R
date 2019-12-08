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
library(leaflet)

# Load in the cleaned data into Shiny

teams <- read_rds("teams.rds")
players <- read_rds("players.rds")
arenas<- read_rds("arenas.rds")

flyers <- teams %>% 
  filter(team == "Philadelphia Flyers") %>% 
  filter(season == "2019-20")

#logo <- makeIcon(iconUrl = "https://www.google.com/url?sa=i&rct=j&q=&esrc=s&source=images&cd=&cad=rja&uact=8&ved=2ahUKEwjpjJL4k5jmAhXOJt8KHWWlCBQQjRx6BAgBEAQ&url=%2Furl%3Fsa%3Di%26rct%3Dj%26q%3D%26esrc%3Ds%26source%3Dimages%26cd%3D%26ved%3D%26url%3Dhttps%253A%252F%252Fen.wikipedia.org%252Fwiki%252FNational_Hockey_League%26psig%3DAOvVaw1GY6AJY7pKjV0Q-Yh2mZvU%26ust%3D1575417024037823&psig=AOvVaw1GY6AJY7pKjV0Q-Yh2mZvU&ust=1575417024037823", iconWidth = 5, iconHeight = 5)

# Define UI for application
# Use a black theme because that is the NHL's color in their logo

ui <- dashboardPage(skin = 'black',
                    
      # Add the title of my application
      
      dashboardHeader(title = "NHL Statistics"),#, icon = icon("logo")),
      
      # Create the sidebar navigation
      
      dashboardSidebar(
        sidebarMenu(
          menuItem("Home", tabName = "home", icon = icon("home")),
          menuItem("Current NHL Team Stats", tabName = "teams"),
          menuItem("Player Stats", tabName = "players"),
          menuItem("Models", tabName = "model"),
          menuItem("Defunct NHL Team Stats", tabName = "defunct"),
          menuItem("Glossary", tabName = "glossary")
        )
      ),
      dashboardBody(
        tabItems(
          
          # Creating the Home page in my dashboard
          
          tabItem(tabName = "home",
                  h1("Visualizing the NHL"),
                  tags$div(class = "widget-user-header bg-blue-active text-center",    
                  h5("Founded in 1917, the National Hockey League (NHL) is a professional ice hockey league in North America. In the NHL, there are currently 
                    24 teams based in the United States and 7 based in Canada."),
                  p("Select any team icon to view the team name and location of the team's stadium!")),
                  leafletOutput("mymap", height = "500"),
                  h3("Content"),
                  p("Thank you for viewing my project! You can navigate the side bar to view team stastistics
                    for current and defunct NHL teams, player statistics, and predictions for the Philadelphia
                    Flyers' 2019-20 season. 
                    The statistics range from more typical ice
                    hockey statistics such as goals, assists, and points to more unique statistics like penalty-kill percentage, faceoff win percentage, and 
                    game-winning goals."),
                  h3("The Data"),
                  p("The National Hockey League (", tags$a("NHL", href = "http://www.nhl.com/"), ") recently published the",
                  tags$a("complete digitization of their statistical records", href = "http://www.nhl.com/stats/"), "dating back 100 years to
                    the first season in 1917-1918 (although unfortunately there is no data for the
                    2004-05 season due to the lockout!). This data includes team statistics and
                    player statistics as well as visualization and filtering tools. The page includes more than
                    15 million player and game data points, with those from 1917 to 1987
                    previously unavailable. However, the data is not available for download
                    on their website, so I had to extract the data from the website using
                    webscraping software from", tags$a("webscraper.io", href = "https://webscraper.io/"),".
                    This software is an extension for Google Chrome and allows
                    you to select the data on the website you’re interested in, and then
                    it navigates multiple pages of the site until it has extracted all of
                    the data into a downloadable format."),
                  h3("About me"),
                  p("My name is",
                    tags$a("Ali Crump", href = "https://www.linkedin.com/in/ali-crump-627503183/"),
                    "and I am a junior at Harvard studying Applied Mathematics in Government. I love
                    the idea of using analytics in sports because it ramps up the competition
                    factor; each team is trying to get an advantage over the others in any
                    way that they can, and turning to analytics can allow you to view your team's 
                    habits and tendencies
                    in a new and potentially illuminating way."),
                  br(),
                  p("I decided to
                    choose NHL data for my project because I've always been fascinated by ice hockey. 
                    I grew up watching my brothers play and attending Flyers games with my family. I really wanted
                    to apply analyses I've learned in the classroom to a team I’ve followed my whole life."),
                  br(),
                  p("You can find my source code", tags$a("here", href = "https://github.com/alicrump/project")," 
                    and contact me at alisoncrump@college.harvard.edu.")
    
          ),
          
          # Create the team data page
          
          tabItem(tabName = "teams",
                  h1("Team Stats"),
                  h5("Select which team and statistic you'd like to look at!"),
            
          # Add a slider that allows the user to select a season start and end
          # Right now the years have commas in them -- still trying to figure
          # out how to fix that.
  
          
          selectInput("favorite_team", 
                                 "Your Favorite Team:",
                      choices = c("Anaheim Ducks",
                                  "Arizona Coyotes",
                                  "Boston Bruins",
                                  "Buffalo Sabres",
                                  "Carolina Hurricanes",
                                  "Columbus Blue Jackets",
                                  "Calgary Flames",
                                  "Chicago Blackhawks",
                                  "Colorado Avalanche",
                                  "Dallas Stars",
                                  "Detroit Red Wings",
                                  "Edmonton Oilers",
                                  "Florida Panthers",
                                  "Los Angeles Kings",
                                  "Minnesota Wild",
                                  "Montréal Canadiens",
                                  "New Jersey Devils",
                                  "Nashville Predators",
                                  "New York Islanders",
                                  "New York Rangers",
                                  "Ottawa Senators",
                                  "Philadelphia Flyers",
                                  "Pittsburgh Penguins",
                                  "San Jose Sharks",
                                  "St. Louis Blues",
                                  "Tampa Bay Lightening",
                                  "Toronto Maple Leafs",
                                  "Vancouver Canucks",
                                  "Vegas Golden Knights",
                                  "Winnipeg Jets",
                                  "Washington Capitals"
                      )),
          
          # Add a drop down bar for which statistic the user wants to view
          
          selectInput("y",
                      "Statistic:",
                      choices = c("Points" = "points",
                                  "Wins" = "wins",
                                  "Losses" = "losses",
                                  "Goals For" = "goals_for",
                                  "Goals Against" = "goals_against",
                                  "Face-Off Win %" = "fow_perc",
                                  "Shots For" = "shots_per_gp",
                                  "Shots Against" = "shots_against_per_gp",
                                  "Powerplay %" = "powerplay_perc",
                                  "Penalty Kill %" = "penalty_kill_perc")),
          
          # Plot my graph of the statistic vs. team
          
                  plotOutput("TeamPlot"),
          
          tabItem(tabName = "team_description",
                  h3("Description"),
                  p("Teams are colored based on the number of points they
                    scored that season. The lighter colors of blue are the 
                    teams which finished the season with the most points, 
                    and the darker colors of blue are the teams who finished 
                    the season with the least points. The teams are arranged
                    in descending order of how they performed on the selected
                    statistic in the selected season. (The top team in points
                    (lightest color) is
                    not necessarily the the team who won the Stanley Cup)."))
                              ),
          
          # Create player data page
          
          tabItem(tabName = "players",
                  h1("Player Data"),
                  h5("Select your favorite NHL team and a statistic
                     you'd like to visualize!"),
                  
            
          # Allow user to select a team to view player stats for
          # In my final project I might allow the user to view more than
          # one team at once. I also want the user to be able to select
          # either a current NHL team or a past NHL team, but couldn't 
          # figure out how to get them both working simultaneously yet.
                  
          
                  selectInput("team1",
                              "Current NHL Team:",
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
                                          "Los Angeles Kings" = "LAK",
                                          "Minnesota Wild" = "MIN",
                                          "Montréal Canadiens" = "MTL",
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
                                          "Washington Capitals" = "WSH"
                                          # "All Teams" = c("ANA","ARI","BOS",
                                          # "BUF","CAR","CBJ","CGY","CHI","COL",
                                          # "DAL","DET","EDM","FLA","LAK","MIN",
                                          # "MTL","NJD","NSH","NYI","NYR","OTT",
                                          # "PHI","PIT","SJS","STL","TBL","TOR",
                                          # "VAN","VGK","WPG","WSH"), selected = "ANA"
                                          )),
                
          
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
                                              "Faceoff Win %" = "fow_perc"), selected = "points"
                                              ),
          selectInput("season2", 
                      "Season:",
                      choices = c(
                        "1917-1918" = "1917-18",
                        "1918-1919" = "1918-19",
                        "1919-1920" = "1919-20",
                        "1920-1921" = "1920-21",
                        "1921-1922" = "1921-22",
                        "1922-1923" = "1922-23",
                        "1923-1924" = "1923-24",
                        "1924-1925" = "1924-25",
                        "1925-1926" = "1925-26",
                        "1926-1927" = "1926-27",
                        "1927-1928" = "1927-28",
                        "1928-1929" = "1928-29",
                        "1929-1930" = "1929-30",
                        "1930-1931" = "1930-31",
                        "1931-1932" = "1931-32",
                        "1932-1933" = "1932-33",
                        "1933-1934" = "1933-34",
                        "1934-1935" = "1934-35",
                        "1935-1936" = "1935-36",
                        "1936-1937" = "1936-37",
                        "1937-1938" = "1937-38",
                        "1938-1939" = "1938-39",
                        "1939-1940" = "1939-40",
                        "1940-1941" = "1940-41",
                        "1941-1942" = "1941-42",
                        "1942-1943" = "1942-43",
                        "1943-1944" = "1943-44",
                        "1944-1945" = "1944-45",
                        "1945-1946" = "1945-46",
                        "1946-1947" = "1946-47",
                        "1947-1948" = "1947-48",
                        "1948-1949" = "1948-49",
                        "1949-1950" = "1949-50",
                        "1950-1951" = "1950-51",
                        "1951-1952" = "1951-52",
                        "1952-1953" = "1952-53",
                        "1953-1954" = "1953-54",
                        "1954-1955" = "1954-55",
                        "1955-1956" = "1955-56",
                        "1956-1957" = "1956-57",
                        "1957-1958" = "1957-58",
                        "1958-1959" = "1958-59",
                        "1959-1960" = "1959-60",
                        "1960-1961" = "1960-61",
                        "1961-1962" = "1961-62",
                        "1962-1963" = "1962-63",
                        "1963-1964" = "1963-64",
                        "1964-1965" = "1964-65",
                        "1965-1966" = "1965-66",
                        "1966-1967" = "1966-67",
                        "1967-1968" = "1967-68",
                        "1968-1969" = "1968-69",
                        "1969-1970" = "1969-70",
                        "1970-1971" = "1970-71",
                        "1971-1972" = "1971-72",
                        "1972-1973" = "1972-73",
                        "1973-1974" = "1973-74",
                        "1974-1975" = "1974-75",
                        "1975-1976" = "1975-76",
                        "1976-1977" = "1976-77",
                        "1977-1978" = "1977-78",
                        "1978-1979" = "1978-79",
                        "1979-1980" = "1979-80",
                        "1980-1981" = "1980-81",
                        "1981-1982" = "1981-82",
                        "1982-1983" = "1982-83",
                        "1983-1984" = "1983-84",
                        "1984-1985" = "1984-85",
                        "1985-1986" = "1985-86",
                        "1986-1987" = "1986-87",
                        "1987-1988" = "1987-88",
                        "1988-1989" = "1988-89",
                        "1989-1990" = "1989-90",
                        "1990-1991" = "1990-91",
                        "1991-1992" = "1991-92",
                        "1992-1993" = "1992-93",
                        "1993-1994" = "1993-94",
                        "1994-1995" = "1994-95",
                        "1995-1996" = "1995-96",
                        "1996-1997" = "1996-97",
                        "1997-1998" = "1997-98",
                        "1998-1999" = "1998-99",
                        "1999-2000" = "1999-00",
                        "2000-2001" = "2000-01",
                        "2001-2002" = "2001-02",
                        "2002-2003" = "2002-03",
                        "2003-2004" = "2003-04",
                        
                        "2005-2006" = "2005-06",
                        "2006-2007" = "2006-07",
                        "2007-2008" = "2007-08",
                        "2008-2009" = "2008-09",
                        "2009-2010" = "2009-10",
                        "2010-2011" = "2010-11",
                        "2011-2012" = "2011-12",
                        "2012-2013" = "2012-13",
                        "2013-2014" = "2013-14",
                        "2014-2015" = "2014-15",
                        "2015-2016" = "2015-16",
                        "2016-2017" = "2016-17",
                        "2017-2018" = "2017-18",
                        "2018-2019" = "2018-19",
                        "2019-2020" = "2019-20"
                      ), selected = "2018-19"),
          
          h5("(Displaying the top 15 for each statistic)"),
          
          
            # Plot the graph of my statistic vs. players for a certain team
          
                                        plotOutput("PlayerPlot"),
          tabItem(tabName = "player_description",
                  h1("Description"),
                  p("Players are colored based on the number of points they
                    scored that season. The lighter colors of blue are the 
                    players who finished the season with the most points, 
                    and the darker colors of blue are the players who finished 
                    the season with the least points. The teams are arranged
                    in descending order of how they performed on the selected
                    statistic in the selected season."))
                  ),
          tabItem(tabName = "model",
                  h1("2019-2020 Season Predictions for Philadelphia Flyers"),
                  selectInput(inputId = "model_stat",
                              "Statistic:",
                              choices = c(#"Goals For" = "goals_for",
                                          "Goals For Per Game Played" = "goalsf_per_gp",
                                          "Goals Against Per Game Played" = "goalsa_per_gp",
                                          "Shots per Game Played" = "shots_per_gp",
                                          "Shots against per Game Played" = "shots_against_per_gp",
                                          #"Goals Against" = "goals_against",
                                          "Face-Off Win %" = "fow_perc",
                                          "Shots For" = "shots_per_gp",
                                          "Shots Against" = "shots_against_per_gp",
                                          "Powerplay %" = "powerplay_perc",
                                          "Penalty Kill %" = "penalty_kill_perc")),
                  plotOutput("model"),
                  h3("Description"),
                  p("Above is a linear model which displays the relationship between the 
                    selected statistic and the number of points an NHL team has. If the
                    slope of our regression line is positive, we say that the statistic and 
                    points are positively correlated. If the slope is negative, we say that
                    the statistic and points are negatively correlated."),
                  br(),
                  p("If the data points are closely clustered around our regression line,
                    we might say that our model is a good predictor of an NHL team's points
                    for a particular statistic.")
                  ),
          tabItem(tabName = "defunct",
                  h1("Past NHL Team Stats"),
                  h5("There are 19 defunct and relocated NHL teams. Some relocated teams
                     include the Hartford Whalers and the Quebec Nordiques, which became 
                     the Carolina Hurricanes and Colorado Avalanche, respectively. Some 
                     teams like the Philadelphia Quakers, St. Louis Eagles, and the Montreal 
                     Maroons became defunct due to the Great Depression."),
                  selectInput("defunct_team",
                              "Past NHL Team:",
                              choices = c("Toronto Arenas",
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
                                          "Atlanta Thrashers"), selected  = "Hartford Whalers"),
                  selectInput(inputId = "statistic",
                              "Statistic:",
                              choices = c("Points" = "points",
                                          "Goals For" = "goals_for",
                                          "Goals Against" = "goals_against",
                                          "Face-Off Win %" = "fow_perc",
                                          "Shots For" = "shots_per_gp",
                                          "Shots Against" = "shots_against_per_gp",
                                          "Powerplay %" = "powerplay_perc",
                                          "Penalty Kill %" = "penalty_kill_perc"), selected = "Points"),
                  h5("Warning: If the plot is blank that means there was no data available for that particular statistic or team!"),
                  plotOutput("DefunctPlot"),
                  h3("Description"),
                  p("This plot shows the relationship between the selected defunct teams' statistic over
                    the history of their particular franchise. Unlike the team stats plot, we only see
                    one team at a time since these defunct teams were not around at the same times.")),
          tabItem(tabName = "glossary",
                  h1("Glossary"),
                  h5("(From http://www.nhl.com/stats/glossary)"),
                  h3("Assists"),
                  p("Assists can be awarded to a maximum of two players touching the puck before the 
                    goal scorer, provided the opposing team has not controlled the puck between the 
                    potential assists and the goal being scored. Unless otherwise specified, assist 
                    totals are for all situations (even strength, power play, shorthanded) combined. 
                    The last teammate to touch the puck before the goal scorer gets is awarded a primary 
                    assist. The preceding teammate to touch the puck before the first assisting player 
                    is awarded a secondary assist. Goals scored without teammates passing or otherwise 
                    touching the puck before it gets to the goal scorer are said to be “unassisted” and 
                    no assists are awarded."),
                  h3("Faceoff Win Percentage"),
                  p("After every whistle, a faceoff is taken between two players on opposing teams. 
                    Faceoff Win Percentage is the percentage of times a particular team or player won
                    the faceoff for their team."),
                  h3("Goals"),
                  p("The last player to touch the puck before it fully crosses the opponent’s goal line
                    is awarded a goal scored. In rare cases where an opposing team’s skater directs the
                    puck into his own goal, the closest player on the scoring team is awarded the goal. 
                    Unless specified, goal totals are for all situations (even strength, power play, 
                    shorthanded) combined."),
                  h3("Penalty Minutes"),
                  p("Penalty minutes are a total of all penalty minutes, whether those penalties caused
                    an opposition power play or not."),
                  h3("Plus-Minus"),
                  p("Plus-minus is a team’s goal differential while a particular player is on the ice, 
                    excluding the power play but including empty net situations. All the skaters on the
                    ice receive a plus or minus when an even strength goal or shorthanded goal is scored
                    depending on which team scored; plus-minus is not tracked for goalies. However, 
                    plus-minus is heavily influenced by the strength of a player’s teammates and 
                    goaltending, as well as small sample variances"),
                  h3("Points per Game Played"),
                  p("In comparing the point production of different players, point totals can be divided
                    by games played e.g. to account for players who have missed games due to injury or 
                    who have not spent the entire season in the NHL."),
                  h3("Shooting Percentage"),
                  p("The percentage of shots on goal (by a team or player) that go in the net, or S% = G/S.
                    Shooting percentage does not take missed shots or blocked shots into consideration, 
                    only shots that were saved by the goalie or scored a goal."),
                  h3("Shots"),
                  p("Shots are the number of shots on goal taken by a player or team. Attempts blocked and
                    missed shots are not included. Shots are also called shots on goal, or SOG."),
                  h3("Time on Ice per Game Played, TOI/GP"),
                  p("Time On Ice is a player’s playing time in all situations (even strength, power play, 
                    shorthanded). Consequently, TOI = EVTOI + PPTOI + SHTOI. See “Even strength time on 
                    ice”, “Power play time on ice”, and “Shorthanded time on ice”."))
                  
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
      
      filter(team == input$favorite_team, season != "2019-20") %>% 
      # group_by(team) %>% 
      mutate(stat1 = !! rlang:: sym(input$y)) %>% 
      filter(stat1 != is.na(stat1)) %>% 
      select(team, season, stat1, color, points)
      # arrange(desc(points))
  })
  playerreact <- reactive({
    players %>%
      
      # Filter for the team the user inputs. Group by the player and sum
      # the selected statistic for each player. 
      
      filter(team == input$team1) %>% 
      filter(season == input$season2) %>%
      group_by(name) %>% 
      mutate(total = sum(!! rlang:: sym(input$z))) %>%
      arrange(desc(total)) %>% 
      ungroup(name) %>% 
      top_n(15)
  })
  
  modelreact <- reactive({
    teams %>%
      mutate(stat = (!! rlang:: sym(input$model_stat))) %>%
      filter(stat != 0)
  })
  
  defunctreact <- reactive({
    teams %>% 
    filter(team == input$defunct_team) %>% 
    mutate(statistic = (!! rlang:: sym(input$statistic)))
    
  })
  
      # Make the ggplot for the specifications the user selected.
      # Rotate the x axis team/player names so they're legible.
      # Add axes labels.
  
    output$TeamPlot <- renderPlot({
        teamreact() %>% 
        ggplot(aes(x = season, y = stat1, fill = points)) +
        geom_col() +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        xlab("Season") +
        ylab("Statistic") +
        geom_text(aes(label = stat1), color = "white", nudge_y = -3)
        
        
        
    })
    
    output$PlayerPlot <- renderPlot({
      playerreact() %>%
        ggplot(aes(x = reorder(name, -total), y = total, fill = points)) +
        geom_col() +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("Player") +
        ylab("Statistic") +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = 0))+
        geom_text(aes(label = total), color = "white", nudge_y = -1) 
      
    })
    
    output$mymap <- renderLeaflet({
      arenas %>% 
      leaflet() %>% 
        addTiles() %>%
        addMarkers(popup = arenas$label)
    })
    
    output$model <- renderPlot({
      highlight <- modelreact() %>% filter(season == "2019-20",
                                           team == "Philadelphia Flyers")
      modeldata <- 
        modelreact() %>% 
        filter(season != "2019-20")
      ggplot() +
        geom_point(data=modeldata, aes(x = stat, y = points)) +
        geom_smooth(data=modeldata, 
                    aes(x = stat, y = points),
                    method = "lm", se = FALSE) +
        theme_bw() +
        geom_vline(data=highlight, aes(xintercept = stat), color = "chocolate1") +
        labs(x = "Statistic", y = "Points") +
        theme(legend.position = "none")
        
    })
    
    output$DefunctPlot <- 
      renderPlot({
        defunctreact() %>% 
          ggplot(aes(x = season, y = statistic)) +
          geom_col() +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(x = "Season", y = "Statistic")
      })
    
}

# Run the application 

shinyApp(ui = ui, server = server)
