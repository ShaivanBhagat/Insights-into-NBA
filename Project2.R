# Load all essential libraries which might be used for analyses here
library(tidyverse)
library(lubridate)
library(readr)
library(rvest)
library(dplyr)
library(ggplot2)
library(readxl)
library(viridis)
library(ggmap)
library(leaflet)
library(viridisLite)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(reshape2)

# Read in files which were obtained from either Kaggle or Github based on 
# the databse "Basketball-reference.com"

# Dataset on the players
players_1 <- read.csv("NBA_Data/player_data.csv")

# Dataset on players obtained from a different file to increase the number of observations and variables 
# to make the data more reliable
players_2 <- read.csv("NBA_Data/Players.csv")

# Dataset on the weekly awards given to individual players and statistics regarding the seasons
Player_week <- read.csv("NBA_Data/NBA_player_of_the_week.csv")

# Season stats obtained from two different files 
Season_stats1 <- read.csv("NBA_Data/Seasons_Stats.csv")
Season_stats2 <- read.csv("NBA_Data/all_seasons.csv")

# Dataset on the salaries of players
Salaries <- read_excel("NBA_Data/Player - Salaries per Year (1990 - 2017).xlsx")

# Renaming variable headers 
players_1 <- players_1 %>% 
  rename(
    Player = name)

# Creating a new dataframe with observations from both player datasets based on the name of the player
Players_Data <- left_join(players_1,players_2, by = "Player")

# Renaming variable for ease of use
Season_stats2 <- Season_stats2 %>% 
  rename(
    Player = player_name
  )

# Creating a new dataframe which summarizes the number of points scored per year
points_year <- Season_stats1 %>% 
  group_by(Year) %>% 
  summarise(
    total = sum(PTS)
  )

# Creating a scatterplot to see the number of points scored over the years
ggplot(points_year, aes(Year, total)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  theme_classic() +
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000, 250000, 300000),
                     labels = c("50k", "100k", "150k", "200k", "250k", "300k")) +
  ggtitle('Points scored per season') +
  ylab("Total points scored per season")

# Creating a new set with the number of three pointers over the years
# The values for 3 pointers are only available after the year 1980, thus adding a filter
three_pointers <- Season_stats1 %>% 
  filter(
    Year > 1980
  ) %>% 
  group_by(Year) %>% 
  summarise(total = sum(X3PA))

# Plotting the evolution of 3 pointers scored over the years
ggplot(three_pointers, aes(Year, total)) +
  geom_point() +
  geom_smooth() +
  geom_bar(stat = "identity", fill = "lightblue", alpha = 0.7) +
  scale_y_continuous(breaks = c(20000, 40000, 60000, 80000)) +
  ggtitle('Evolution in three-pointers attempted by year') +
  theme_classic() +
  ylab('3pt attempts')



# Creating two new datasets with contains the name of the teams based on if they 
# are in the east or west conference
east_teams <- Player_week %>% filter(Conference == 'East') %>% distinct(Team)
west_teams <- Player_week %>% filter(Conference == 'West') %>% distinct(Team)

# Manually checking if all observations have a conference value, if not adding it and 
# manually adding the team Washingtom Bullets since it is not given in the dataset used
Player_week <- Player_week %>%
  mutate(
    Conference = if_else(
      Conference == '',
      if_else(Team %in% east_teams$Team | Team == 'Washington Bullets', 'East', 'West'),
      Conference
    )
  )

# Creating a new dataset which contains the season, the conference, and the number of weekly
# awards received 
Conference_awards <- Player_week %>% 
  group_by(
    Conference, Season.short
  ) %>% 
  summarise(
    awards = n()
  )

# since two awards are given in a week: 1 East, and 1 West,
# Real_value is being used otherwise a two observations would be incluuded for the same variable
Conference_awards <- Player_week %>%
  group_by(Conference, Season.short, Real_value) %>%
  summarise(count = sum(Real_value)) %>%
  mutate(newCount = ifelse(Conference == 'East', 1 * count, -1 * count))

# Plotting the East vs West conference weekly awards as a bargraph on opposite scales
ggplot(Conference_awards, aes(x = Season.short, y = newCount, fill = Conference)) +
  geom_bar(stat = "identity", color = 'white') +
  theme_minimal() +
  ggtitle('Number of Weekly Awards per Conference') +
  xlab('Season') +
  ylab('Number of Awards') +
  scale_fill_manual(name = '', values = c('blue', 'red')) +
  scale_x_continuous(breaks = seq(1985, 2018, by = 6)) +
  scale_y_continuous(breaks = seq(-20, 20, 5), labels = abs)

# Creating a dataset which summarizes the number of awards won per team
Team_awards <- Player_week %>% 
  group_by(
    Team
  ) %>% 
  summarize(
    awards = n()
  )

# Plotting the number of awards in decreasing order for each team
ggplot(Team_awards, aes(x = reorder(Team, -awards), y = awards)) +
  geom_bar(stat = "identity", fill = "#1F78B4") +
  theme_minimal() +
  ggtitle('Number of Awards by Team') +
  xlab('Team') +
  ylab('Number of Awards') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Creating a dataset which summarizes the number of awards per player
# only selecting the top 15, otherwise there are too many players
Player_awards <- Player_week %>% 
  group_by(Player) %>% 
  summarise(
    awards = n()
  ) %>% 
  arrange(desc(awards))
Player_awards <- Player_awards[1:15, ]

# Plotting the number of awards per player in the descending order 
ggplot(Player_awards, aes(x = reorder(Player, -awards), y = awards)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_gray() +
  ggtitle('Number of Awards by Player') +
  xlab('Player') +
  ylab('Number of Awards') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Counts the number of player observations per country 
CountryCount <- Season_stats2 %>% group_by(country) %>% count(Player)

# Counts the number of players from each country in descending order
CountryDist <- CountryCount %>% group_by(country) %>% count() %>% arrange(desc(n))

# Including all international players 
# and summing them together as one region instead of separate countries
Foreign <- CountryDist %>% filter(!country %in% c('USA'))
Foreign <- data.frame(country=c("Foreign"), n=sum(Foreign$n))

# CountryPrc <- rbind(USA, Foreign) %>% ungroup() %>% mutate(perc= scales::percent(`n` / sum(`n`)))
# head(CountryPrc)

# Filtering for all non-Americans in a dataset
ForeignGrowth <- Season_stats2 %>% filter(!country %in% c('USA'))

# Grouping them by season (years) 
ForeignGrowth <- ForeignGrowth %>% group_by(season) %>% count()

# Using only one value for the year instead of a season
# Example: using 2022 as the value for the 2022-2023 season
ForeignGrowth <- data.frame(year = c(substr(ForeignGrowth$season, 1, 4)), 
                            total = c(ForeignGrowth$n))

# making sure the values are in a numeric format
ForeignGrowth$year <- as.numeric(ForeignGrowth$year)

# Plotting the number of internationals over the years 
ggplot(ForeignGrowth, aes(year, total)) +
  geom_line(color="blue", size = 1) +
  geom_point(color="orange")+
  labs(title="Internationals in the NBA") +
  theme_light()

# Creating a dataset which contains players only from USA and US territories
# Filtering such that they have been playing in the NBA from before 2018
# Summarizing the number of points each player scored based on season average
# Selecting the top 10 players for visualization
USA <- Season_stats2 %>% 
  filter(country=="USA" | country == "US Virgin Islands",
         draft_year <= 2018) %>% 
  group_by(Player) %>% 
  summarise(
    PPG = mean(pts, na.rm = TRUE)
  ) %>% 
  arrange(desc(PPG)) %>% 
  slice_head(n=10)

# Creating a similar dataset as the one before but this time filtering for international players
World <-  Season_stats2 %>% 
  filter(country!="USA" & country!= "US Virgin Islands",
         draft_year <= 2018) %>% 
  group_by(Player) %>% 
  summarise(
    PPG = mean(pts, na.rm = TRUE)
  ) %>% 
  arrange(desc(PPG)) %>% 
  slice_head(n=10)

# Adding the teams of the top 10 American players manually
# Adding the coordinates for the states the respective team is manually
USA_additionalData <- c("GSW","CLE","PHI","ATL","POR","CHI","UTA","HOU","PHX","LAL")
USA_Latitudes <- c("37.768650142575304","41.48599601222295","39.951834419274455","33.744288880476496","45.51654445618419",
                   "41.8781","40.7608","29.7604","33.4484","34.0522")
USA_Longitudes <- c("-122.38756606105558","-81.71619042839632","-75.17442227766462","-84.4023231072451","-122.68330909845938",
                    "-87.6298","-111.8910","-95.3698","-112.0740","-118.2437")

# Adding the above mentioned additional data to the USA dataframe
USA <- USA %>% 
  mutate(
    Team = USA_additionalData,
    Lat = USA_Latitudes,
    Long = USA_Longitudes
  )

# Adding the countries and coordinates for the countries for the international players manually by creating new dataframes
World_additionalData <- c("Slovenia", "Cameroon", "Australia", "Greece", "Canada",
                            "Serbia", "Germany", "Latvia", "Canada", "China ")
World_Latitudes <- c(46.1512, 5.9631, -25.2744, 39.0742, 56.1304, 44.0165, 51.1657, 56.8796, 56.1304, 39.9042)
World_Longitude <- c(14.9955, 10.1595, 133.7751, 21.8243, -106.3468, 21.0059, 10.4515, 24.6032, -106.3468, 116.4074)

# Adding the additional data into the World dataframe
World <- World %>% 
  mutate(
    Country = World_additionalData,
    Lat = World_Latitudes,
    Long = World_Longitude
  )

# Convert Lat and Long to numeric values to ensure it can be read as numeric values for visualisation
World$Lat <- as.numeric(as.character(World$Lat))
World$Long <- as.numeric(as.character(World$Long))
USA$Lat <- as.numeric(as.character(USA$Lat))
USA$Long <- as.numeric(as.character(USA$Long))

# Confirming that the variables are in the desired format
str(World)
str(USA)

# Load world map data in a variable "world"
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filtering regions as either USA, or other regions
World$Region <- ifelse(World$Country == "USA", "USA", "International")
USA$Region <- "USA"

# Creating a geospatial plot which contains the number of points scored (PPG) for the top 10 players from 
# USA and for internationals
ggplot() +
  geom_sf(data = world, fill = "lightgray", color = "white", size = 0.2) +
  geom_point(data = World, aes(x = Long, y = Lat, size = PPG, color = Region), alpha = 0.7) +
  geom_point(data = USA, aes(x = Long, y = Lat, size = PPG, color = Region), alpha = 0.7) +
  scale_size_continuous(range = c(2, 10), breaks = seq(20, 35, by = 5), labels = scales::number_format()) +
  scale_color_manual(values = c("USA" = "blue", "International" = "red"), name = "Region") +
  theme_void() +
  ggtitle("World Map with PPG Points") +
  labs(subtitle = "Players from USA vs Other Regions")


# Creating an interactive map using leaflet of the same visualisation
world_map <- leaflet() %>%
  addTiles() %>%
  addCircles(data = World, lng = ~Long, lat = ~Lat, weight = 1, radius = ~PPG * 1000,
             color = ~Region, fillColor = ~Region, fillOpacity = 0.7,
             popup = paste("Player: ", World$Player, "<br>PPG: ", World$PPG)) %>%
  addCircles(data = USA, lng = ~Long, lat = ~Lat, weight = 1, radius = ~PPG * 1000,
             color = ~Region, fillColor = ~Region, fillOpacity = 0.7,
             popup = paste("Player: ", USA$Player, "<br>PPG: ", USA$PPG)) %>%
  addLegend("bottomright", colors = c("blue", "red"), labels = c("USA", "International"), title = "Region") %>%
  addMeasure(primaryLengthUnit = "kilometers", secondaryAreaUnit = NULL)
# Display the map
world_map


# Making sure that both USA and World datasets have the same column names
colnames(USA) <- colnames(World)

# Combine USA and World data frames into a new dataset 
combined_data <- rbind(
  mutate(USA, Location = "USA"),
  mutate(World, Location = "International")
)

# Create a more polished histogram/bar graph which allows a better visualisation and 
# allows us to make conclusions
histogram <- ggplot(combined_data, aes(x = PPG, fill = Location)) +
  geom_histogram(binwidth = 1, position = "identity", alpha = 0.7, color = "black") +
  labs(title = "PPG for USA vs International Players",
       x = "Points Per Game (PPG)", y = "Frequency") +
  scale_fill_manual(values = c("USA" = "#1f78b4", "International" = "#e41a1c")) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12))

# Print the histogram
print(histogram)

# Renaming variable in the dataset for ease of use and summarizing based on total player salaries over the years
# Using total salary over the years instead of average salary per season since this reflects the longetivity of the 
# player as well. More time played in the NBA -> more money earned
Salaries <- Salaries %>%  
  rename(
    Player = `Player Name`) %>% 
  group_by(Player) %>% 
  summarize(
    salary = sum(`Salary in $`)
  )

# Creating a new dataset which contains season, player and salary statistics
Stats <- left_join(Salaries,Season_stats2, by = "Player")

# Filtering this for Lebron James and Michael Jordan 
# using the salary on a logarithmic scale instead since the value for salaries is 
# much higher than the other statistics and skews the axis 
Stats_GOAT <- Stats %>% 
  filter(
    Player == "LeBron James" | Player == "Michael Jordan") %>% 
  group_by(Player) %>% 
  summarise(
    salary = log(mean(salary)),
    PPG = mean(pts),
    REB = mean(reb),
    ASST = mean(ast)
  )

# Melt the data frame for easy plotting
melted_players <- Stats_GOAT %>%
  pivot_longer(cols = -Player, names_to = "Attribute", values_to = "Value")

# Create a bar plot comparing the two players and statistics in consideration
ggplot(melted_players, aes(x = Player, y = Value, fill = Attribute)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Comparison of LeBron James and Michael Jordan",
       x = "Player", y = "Value") +
  theme_linedraw() +
  theme(legend.position = "top")

# Filter through the Stats dataset to create a new dataset 
# This only includes players that have score more than 22 points, 7 rebounds, 6 assists,
# and played for more than 75 matches in a season
# Then summarizes based on mean salary, points, rebounds, and assists per player
Best_player <- Stats %>% 
  filter(
    pts >= 22,
    reb >= 7,
    ast >= 6,
    gp >= 75
  ) %>% 
  group_by(Player) %>% 
  summarise(
    salary = log(mean(salary)),
    PPG = mean(pts),
    REB = mean(reb),
    ASST = mean(ast)
  )

# Melt the data for plotting
Best_player <- reshape2::melt(Best_player, id.vars = "Player")

# Plot the graph to show the ideal best player
ggplot(Best_player, aes(x = Player, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Comparison of Statistics for NBA Players",
       x = "Player", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


