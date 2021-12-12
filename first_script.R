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
install.packages('gganimate')
install.packages('gifski')
install.packages('png')
install.packages("ggplot2")
install.packages("dplyr")
install.packages("stringr")


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
library(ggplot2)
library(gganimate)
library(gifski)
library(png)
library(dplyr)
library(stringr)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

#Get Game ID's and Gamelog Data

# Select seasons from 1949 and after
selectedSeasons <- c(2004:2020)
# Get game IDs for Regular Season and Playoffs

#####################
## Retrieve gamelog data for players and teams
#####################
# Get player gamelogs
P_gamelog_reg <- suppressWarnings(game_logs(seasons = selectedSeasons, league = "NBA", result_types = "player", season_types = "Regular Season"))
# Get team gamelogs
T_gamelog_reg <- suppressWarnings(game_logs(seasons = selectedSeasons, league = "NBA", result_types = "team", season_types = "Regular Season"))

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
        bars=c("P2M","P3M"), line="PTS",
        order.by="PTS", labels.bars=c("2PM","3PM"),
        title=teamSelected)



## This scatter displays the relationship between 
# assists and turnovers, each dot represents a player categorical variable
## account for perentages
teamSelected <- "GSW"
seasonSelected <- 2016
Pbox.sel <-subset(Pbox,Team==teamSelected & MIN>=1000)%>% 
  filter(Season==seasonSelected)
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
# and 3 point percentage, attempted shots are the circle size, winpercentage is
# represented as the color of the circle

seasonSelected <- c(2010:2016)
Tbox.sel <- filter(Tbox,Season%in%seasonSelected)
attach(Tbox.sel)
X <- data.frame(T=Team, P2p, P3p, W, AS=P2A+P3A+FTA, Season)
detach(Tbox.sel)
labs <- c("2-point shots (% made)",
          "3-point shots (% made)",
          "Wins",
          "Total shots attempted")
p <- bubbleplot(X, id="T", x="P2p", y="P3p", col="W",
           size="AS", labels=labs, title=paste0("NBA - ", seasonSelected))
p <- ggplot(X, aes(x = P2p, y = P3p, color= W,
                   size = AS)) + 
  geom_point() + 
  labs(x ="2-point shots (% made)",
          y = "3-point shots (% made)",
          color="Wins",
          size = "Total shots attempted", title = "Team Wins in relation to 3-pt and 2-pt percentage 2010-2016")
q <- p + transition_states(Season)
animate(q)
plot(q)





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
           title=paste0("4 secondary factors of success in the NBA, 
                        Players on top teams by wins of ", seasonSelected),
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
           title=paste0("4 secondary factors of success ", seasonSelected),
           text.legend=TRUE, text.size=3.5, scale=FALSE)

## cluster analysis offensive efficency ratios
## Four factors to success Score, Protect, Crsh and attack
## Effective Field goal percentage
## Turnover Ratio
## rebound Percentage
# Free Throw Rate

#Hierarchical Clustering of NBA players

gamedata <- game_logs(seasons = 2020)
plot2 <-ggplot(gamedata, aes(minutes,pts))+
  geom_point(aes(alpha= 1/10))+
  geom_smooth(method = 'loess')+
  facet_wrap(~nameTeam)+ 
  theme(legend.position = "none")
plot2
## attempts vs made 

## 3 pointers made and true shooting percentage

## make a list of the top three point shooters from 1996-2020 and just make scatter plot of 3 point makes and year scatter plot

Threepointleaders <- read.csv("3ptleaders.csv",TRUE,",")%>%
  mutate(Player = str_squish(Player), Year = str_trunc(str_squish(Year),4,"right",ellipsis = ""))%>% 
  mutate(Year = as.Date(Year, "%Y"))
class(Threepointleaders)
head(Threepointleaders)

print(Threepointleaders)

p1 <- ggplot(aes(y = X3p, x = (Year)),data = Threepointleaders) + geom_line(aes())+geom_point(aes(color = Player))

p1 + labs(title = "NBA Annual 3 Point Leaders 1979 to 2020", x = NULL, y = NULL)+ 
  ##scale_x_date()+ 
  theme(legend.position = "none")





