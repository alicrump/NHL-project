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
          menuItem("Player Stats", tabName = "players"),
          menuItem("Predictions", tabName = "model"
          )
        )
      ),
      dashboardBody(
        tabItems(
          
          # Creating the Home page in my dashboard
          
          tabItem(tabName = "home",
                  leafletOutput("mymap", height = "500"),
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
                                          "Penalty Kill %" = "penalty_kill_perc")),
            
          # Add a slider that allows the user to select a season start and end
          # Right now the years have commas in them -- still trying to figure
          # out how to fix that.
          
                  selectInput("season1", 
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
                              )),
          
          # Plot my graph of the statistic vs. team
          
                  plotOutput("TeamPlot"),
          tabItem(tabName = "player_description",
                  h1("Description"),
                  p("Teams are ordered based on the number of points they
                    scored that season. I used points as a method of 
                    arranging the teams, so the top team in points might
                    not necessarily be the team who won the Stanley Cup."))
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
                      )),
            # Plot the graph of my statistic vs. players for a certain team
          
                                        plotOutput("PlayerPlot"),
          tabItem(tabName = "player_description",
                  h1("Description"),
                  p("Players are ordered based on the number of points they scored that season."))
                  ),
          tabItem(tabName = "model",
                  h1("2019-2020 Season Predictions"))
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
      
      filter(season == input$season1) %>% 
      group_by(team) %>% 
      mutate(sum = sum(!! rlang:: sym(input$y))) %>% 
      arrange(desc(points)) # %>% 
      # mutate(team = fct_reorder(team, sum))
  })
  playerreact <- reactive({
    players %>%
      
      # Filter for the team the user inputs. Group by the player and sum
      # the selected statistic for each player. 
      
      filter(team == input$team1, season == input$season2) %>%
      group_by(name) %>% 
      mutate(total = sum(!! rlang:: sym(input$z))) %>%
      arrange(desc(total)) %>% 
      slice(1:20)
  })
  
      # Make the ggplot for the specifications the user selected.
      # Rotate the x axis team/player names so they're legible.
      # Add axes labels.
  
    output$TeamPlot <- renderPlot({
        teamreact() %>% 
        ggplot(aes(x = reorder(team, -points), y = sum, fill = color)) +
        geom_col() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("Team") +
        ylab("Statistic") +
        theme(legend.position = "none") +
        geom_text(aes(label = sum), nudge_y = -10)
    })
    
    output$PlayerPlot <- renderPlot({
      playerreact() %>%
        ggplot(aes(x = reorder(name, -points), y = total)) +
        geom_col() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        xlab("Player") +
        ylab("Statistic") +
        theme(legend.position = "none")
    })
    
    output$mymap <- renderLeaflet({
      arenas %>% 
      leaflet() %>% 
        addTiles() %>%
        # setView(lng = -93.85, lat = 37.45, zoom = 3)
        addMarkers(popup = arenas$label)
    })
    
}

# Run the application 

shinyApp(ui = ui, server = server)
