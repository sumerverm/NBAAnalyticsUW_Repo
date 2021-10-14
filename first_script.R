#install packages
install.packages("tidyverse")

install.packages("devtools")
devtools::install_github("abresler/nbastatR")

install.packages("BasketballAnalyzeR")

#The jsonlite package is a JSON parser/generator optimized for the web. 
#Its main strength is that it implements a bidirectional mapping 
#between JSON data and the most important R data types.

install.packages("jsonlite")
#janitor package is a R package that has simple functions for examining and cleaning dirty data
install.packages("janitor")
#Make true type fonts easy to use with pdf
install.packages("extrafont")
#repel overlapping text labels
install.packages("ggrepel")
#Scale customize appearance of axis and legend labels on graphs
install.packages("scales")
#team colors provides color pallate for pro sports teams
install.packages("teamcolors")
#zoo  class with methods for totally ordered indexed observations, vectors matrices and factors
install.packages("zoo")
#future provide a lightweight and unified Future API 
#for sequential and parallel processing of R expression via futures.
install.packages("future")
#Lubridate makes it easier to do the things 
#R does with date-times and possible to do the things R does not
install.packages("lubridate")


###############
#LOAD Libraries
####
library(tidyverse)
library(nbastatR)
library(BasketballAnalyzeR)
library(jsonlite)
library(janitor)
library(extrafont)
library(ggrepel)
library(scales)
library(teamcolors)
library(zoo)
library(future)
library(lubridate)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

#Get Game ID's and Gamelog Data

# Select seasons from 1949 and after
selectedSeasons <- c(1996:1998)
# Get game IDs for Regular Season and Playoffs
gameIds_Reg <- suppressWarnings(seasons_schedule(seasons = selectedSeasons, season_types = "Regular Season") %>% select(idGame, slugMatchup))
gameIds_PO <- suppressWarnings(seasons_schedule(seasons = selectedSeasons, season_types = "Playoffs") %>% select(idGame, slugMatchup))
gameIds_all <- rbind(gameIds_Reg, gameIds_PO)
# Peek at the game IDs
head(gameIds_all)
tail(gameIds_all)
#####################
## Retrieve gamelog data for players and teams
#####################
# Get player gamelogs
P_gamelog_reg <- suppressWarnings(game_logs(seasons = selectedSeasons, league = "NBA", result_types = "player", season_types = "Regular Season"))
P_gamelog_po <- suppressWarnings(game_logs(seasons = selectedSeasons, league = "NBA", result_types = "player", season_types = "Playoffs"))
P_gamelog_all <- rbind(P_gamelog_reg, P_gamelog_po)
View(head(P_gamelog_all))
# Get team gamelogs
T_gamelog_reg <- suppressWarnings(game_logs(seasons = selectedSeasons, league = "NBA", result_types = "team", season_types = "Regular Season"))
T_gamelog_po <- suppressWarnings(game_logs(seasons = selectedSeasons, league = "NBA", result_types = "team", season_types = "Playoffs"))
T_gamelog_all <- rbind(T_gamelog_reg, T_gamelog_po)
View(head(T_gamelog_all))


