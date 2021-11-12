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
selectedSeasons <- c(2010:2020)
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

#####################
## Use Regular Season data
#####################
# Create Tbox (Team boxscore) for each Regular Season
Tbox <- T_gamelog_reg %>%
  group_by("Season"=yearSeason, "Team"=slugTeam) %>%
  dplyr::summarise(GP=n(), MIN=sum(round(minutesTeam/5)),
                   PTS=sum(ptsTeam),
                   W=sum(outcomeGame=="W"), L=sum(outcomeGame=="L"),
                   P2M=sum(fg2mTeam), P2A=sum(fg2aTeam), P2p=P2M/P2A,
                   P3M=sum(fg3mTeam), P3A=sum(fg3aTeam), P3p=P3M/P3A,
                   FTM=sum(ftmTeam), FTA=sum(ftaTeam), FTp=FTM/FTA,
                   OREB=sum(orebTeam), DREB=sum(drebTeam), AST=sum(astTeam),
                   TOV=sum(tovTeam), STL=sum(stlTeam), BLK=sum(blkTeam),
                   PF=sum(pfTeam), PM=sum(plusminusTeam)) %>%
  as.data.frame()
# Create Obox (Opponent Team boxscore) for each Regular Season
Obox <- T_gamelog_reg %>%
  group_by("Season"=yearSeason, "Team"=slugOpponent) %>%
  dplyr::summarise(GP=n(), MIN=sum(round(minutesTeam/5)),
                   PTS=sum(ptsTeam),
                   W=sum(outcomeGame=="L"), L=sum(outcomeGame=="W"),
                   P2M=sum(fg2mTeam), P2A=sum(fg2aTeam), P2p=P2M/P2A,
                   P3M=sum(fg3mTeam), P3A=sum(fg3aTeam), P3p=P3M/P3A,
                   FTM=sum(ftmTeam), FTA=sum(ftaTeam), FTp=FTM/FTA,
                   OREB=sum(orebTeam), DREB=sum(drebTeam), AST=sum(astTeam),
                   TOV=sum(tovTeam), STL=sum(stlTeam), BLK=sum(blkTeam),
                   PF=sum(pfTeam), PM=sum(plusminusTeam)) %>%
  as.data.frame()
# Create Pbox (Player boxscore) for each Regular Season
Pbox <- P_gamelog_reg %>%
  group_by("Season"=yearSeason, "Team"=slugTeam, "Player"=namePlayer) %>%
  dplyr::summarise(GP=n(), MIN=sum(minutes), PTS=sum(pts),
                   P2M=sum(fg2m), P2A=sum(fg2a), P2p=100*P2M/P2A,
                   P3M=sum(fg3m), P3A=sum(fg3a), P3p=100*P3M/P3A,
                   FTM=sum(ftm), FTA=sum(fta), FTp=100*FTM/FTA,
                   OREB=sum(oreb), DREB=sum(dreb), AST=sum(ast),
                   TOV=sum(tov), STL=sum(stl), BLK=sum(blk),
                   PF=sum(pf)) %>%
  as.data.frame()
View(Pbox[Pbox$Player=="Stephen Curry",])

# Bar plots
#####################

teamSelected <- "GSW"
Pbox.sel <- subset(Pbox, Team==teamSelected &
                     MIN>=1000)
seasonSelected <- 2016
barline(data=Pbox.sel[Pbox.sel$Season==seasonSelected,], id="Player",
        bars=c("P2M","P3M","FTM"), line="PTS",
        order.by="PTS", labels.bars=c("2PM","3PM","FTM"),
        title=teamSelected)


##
# Create Tbox (Team boxscore) for each Regular Season
Tbox <- T_gamelog_reg %>%
  group_by("Season"=yearSeason, "Team"=slugTeam) %>%
  dplyr::summarise(GP=n(), MIN=sum(round(minutesTeam/5)),
                   PTS=sum(ptsTeam),
                   W=sum(outcomeGame=="W"), L=sum(outcomeGame=="L"),
                   P2M=sum(fg2mTeam), P2A=sum(fg2aTeam), P2p=P2M/P2A,
                   P3M=sum(fg3mTeam), P3A=sum(fg3aTeam), P3p=P3M/P3A,
                   FTM=sum(ftmTeam), FTA=sum(ftaTeam), FTp=FTM/FTA,
                   OREB=sum(orebTeam), DREB=sum(drebTeam), AST=sum(astTeam),
                   TOV=sum(tovTeam), STL=sum(stlTeam), BLK=sum(blkTeam),
                   PF=sum(pfTeam), PM=sum(plusminusTeam)) %>%
  as.data.frame()

## This scatter displays the relationship between 
# assists and turnovers, each dot represents a player categorical variable

teamSelected <- "GSW"
Pbox.sel <-subset(Pbox,Team==teamSelected & MIN>=1000)
attach(Pbox.sel)
X <- data.frame(AST,TOV,PTS)/MIN
detach(Pbox.sel)
mypal <- colorRampPalette(c("blue","yellow","red"))

scatterplot(X, data.var=c("AST","TOV"), z.var="PTS",
            labels=paste(Pbox.sel$Player,", ",Pbox.sel$Season), palette=mypal)

scatterplot(X, data.var=c("AST","TOV"), z.var="PTS",
            labels=paste(Pbox.sel$Player,", ",Pbox.sel$Season), palette=mypal,
            zoom=c(0.08,0.16,0.05,0.10))


# This bubble plot shows the relationshio between 2 point percentage 
# and 3 point percentage, attempted shots are the circle size, free throw percentage is
# represented as the color of the circle
seasonSelected <- 2016
Tbox.sel <- subset(Tbox_all,Season==seasonSelected)

attach(Tbox.sel)
X <- data.frame(T=Team, P2p, P3p, FTp, AS=P2A+P3A+FTA)
detach(Tbox.sel)
labs <- c("2-point shots (% made)",
          "3-point shots (% made)",
          "free throws (% made)",
          "Total shots attempted")
bubbleplot(X, id="T", x="P2p", y="P3p", col="FTp",
           size="AS", labels=labs, title=paste0("NBA - ", seasonSelected))

## This bubble plot shows the relationship between rebounds and steals, 
# with the total minutes played representing the circle size, the blocks 
# are depicted with the color of the circle
# these are the best performing teams f 2016
# shows that stephen curry was amazing at offense not much at defense

teamsSelected <- c("HOU", "LAC", "SAS", "GSW")
seasonSelected <- 2016
Pbox.sel <- subset(Pbox, Team %in% teamsSelected & MIN>=1500 & Season==seasonSelected)

attach(Pbox.sel)
X <- data.frame(ID=Player, Team, V1=DREB/MIN, V2=STL/MIN,
                V3=BLK/MIN, V4=MIN)
detach(Pbox.sel)
labs <- c("Defensive Rebounds","Steals","Blocks",
          "Total minutes played")
bubbleplot(X, id="ID", x="V1", y="V2", col="V3",
           size="V4", text.col="Team", labels=labs,
           title=paste0("NBA Players in ", seasonSelected),
           text.legend=TRUE, text.size=3.5, scale=FALSE)


##
teamsSelected <- c("MIL", "MIA", "TOR", "BOS")
seasonSelected <- 2020
Pbox.sel <- subset(Pbox, Team %in% teamsSelected & MIN>=1500 & Season==seasonSelected)

attach(Pbox.sel)
X <- data.frame(ID=Player, Team, V1=DREB/MIN, V2=STL/MIN,
                V3=BLK/MIN, V4=MIN)
detach(Pbox.sel)
labs <- c("Defensive Rebounds","Steals","Blocks",
          "Total minutes played")
bubbleplot(X, id="ID", x="V1", y="V2", col="V3",
           size="V4", text.col="Team", labels=labs,
           title=paste0("NBA Players in ", seasonSelected),
           text.legend=TRUE, text.size=3.5, scale=FALSE)

## cluster analysis offensive efficency ratios
## Four factors to success Score, Protect, Crsh and attack
## Effective Field goal percentage
## Turnover Ratio
## rebound Percentage
# Free Throw Rate

#Hierarchical Clustering of NBA players










Tvisual <- Tbox %>%
  group_by(Season)%>%
  summarise(avgP3p = mean(P3p),avgFTp = mean(FTp))%>%
  ggplot(aes(x = Season))+
  geom_line(aes(y = avgP3p))+
  geom_line(aes(y = avgFTp))

