#Sharif Amlani
#R 4.1.1
#Winter 2021

######################## Code Summary ##################

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################
'%!in%' <- function(x,y)!('%in%'(x,y))

#Turns a Regression into a data frame
Model.DF <- function(Model, Robust.SE = NULL) {
  
  #Extract Coefficients
  Model.Output <- as.data.frame(coef(summary(Model)))
  Model.Output$Label <- rownames(Model.Output)
  rownames(Model.Output) <- NULL
  
  #Generate Confidence Intervals
  CI <- as.data.frame(confint(Model, variable.names(Model), level=0.95))
  CI$Label <- rownames(CI)
  rownames(CI) <- NULL
  
  #Merge Model and CIs together 
  Model.Output.Final <- merge(x = Model.Output, y = CI, by =c("Label"))
  
  #Name the columns numeric
  colnames(Model.Output.Final) <- c("Label", "Coeff", "SE", "t.value", "P.Value", "lower", "upper")
  
  Model.Output.Final$Sig.05 <- ifelse(Model.Output.Final$P.Value <= .05, 1,0)
  Model.Output.Final$Sig.10 <- ifelse(Model.Output.Final$P.Value <= .10, 1,0)
  
  #Adjusted R Squared
  Model.Output.Final$AdJ.R2 <- summary(Model)$adj.r.squared
  
  #Dependent Variable
  Model.Output.Final$DV <- all.vars(formula(Model))[1]
  
  #Check for NA's in Model
  for(n in names(coef(Model))){
    if(is.na(Model$coefficients[[n]]) == T){
      newRow <- data.frame(Label=n, 
                           Coeff = NA, 
                           SE = NA, 
                           t.value = NA,
                           P.Value = NA,
                           lower = NA,
                           upper = NA,
                           AdJ.R2 = NA, 
                           Sig.05 = NA,
                           Sig.10 = NA,
                           DV=all.vars(formula(Model))[1])
      
      Model.Output.Final <- rbind(Model.Output.Final, newRow)
      
    }
  }
  
  #Option for Robust Standard Errors
  if(is.null(Robust.SE) == F){
    library(sandwich)
    x<- coeftest(Model, vcov = sandwich::vcovHC(Model, type=Robust.SE))
    xr<- setNames(data.frame(x[1:dim(x)[1], 2]), c("Robust Standard Errors"))
    xr$Label<- rownames(xr); rownames(xr) <- NULL
    
    Model.Output.Final <- merge(Model.Output.Final, xr, by = "Label")
    
  }
  
  return(Model.Output.Final)
  
}
######################### Library #####################
library(spotifyr)
library(lubridate)
library(tidyverse)
library(knitr)
library(margins)
library(ggridges)

######################## Upload API Key ##################

#Set Working Directory
setwd("C:/Users/Shari/OneDrive/R-Scripts/API/Spotify")

#Upload Data
load(file = "Spotify API Key.rda")

#################### Connect to Spotify API ##################

Sys.setenv(SPOTIFY_CLIENT_ID = clientID)
Sys.setenv(SPOTIFY_CLIENT_SECRET = clientSecret)

access_token <- get_spotify_access_token()

#################### Download Elton John Data ##################
EJ.Pure <- get_artist_audio_features('elton john'); EJ.1 <- EJ.Pure

#################### Data Management ##################
#Create Decade Variable
EJ.1$decade <- EJ.1$album_release_year - EJ.1$album_release_year %% 10

#Code Live Tracks
EJ.1$Live <- ifelse(grepl("live", tolower(EJ.1$track_name), fixed = TRUE) == T, "Live", "Studio")
table(EJ.1$Live)
unique(with(EJ.1, data.frame(album_name, Live)))


#Code Hits
EJ.1$Hits <- ifelse(EJ.1$track_name %in% c("Your Song", "Tiny Dancer", 	"Rocket Man (I Think It's Going to Be a Long, Long Time)", "Honky Cat", "Crocodile Rock", "Daniel", "Saturday Night's Alright (For Fighting)", "Goodbye Yellow Brick Road", "Candle in the Wind", "Bennie and the Jets",
                                           "The Bitch Is Back", "Philadelphia Freedom", "Island Girl", "Someone Saved My Life Tonight", "Don't Go Breaking My Heart",
                                           "Sorry Seems to Be the Hardest Word", "Little Jeannie", "Song for Guy" , "Blue Eyes",
                                           "I'm Still Standing", "I Guess That's Why They Call It the Blues", 	"Sad Songs (Say So Much)", "Nikita", "I Don't Wanna Go on with You Like That",
                                           "Sacrifice", "Don't Let the Sun Go Down on Me", 	"Something About the Way You Look Tonight", 	"I Want Love", "Can You Feel the Love Tonight",
                                           "Are You Ready for Love", 	"Electricity", 	"Home Again", "Looking Up", "Circle of Life", "Skyline Pigeon", "Lucy in the Sky with Diamonds",
                                           "Pinball Wizard", 	"Mama Can't Buy You Love", 	"Part-Time Love", 	"Victim of Love", 	"Empty Garden (Hey Hey Johnny)","Kiss the Bride",
                                           "That's What Friends Are For", 	"The One", 	"True Love", 	"Believe", "Live Like Horses", "Written in the Stars", 	"This Train Don't Stop There Anymore",
                                           "Good Morning to the Night",	"Step Into Christmas"),
                    "Hit","Regular")
table(EJ.1$Hits)
EJ.1$Hits <- factor(EJ.1$Hits, levels = c("Regular", "Hit"))

#Code Length
EJ.1$duration <- EJ.1$duration_ms/6000 #Convert to Minutes

#################### Subset ##################

#Drop Weird Albums
EJ.2 <- subset(EJ.1, album_name %!in% c("Me - Elton John Official Autobiography (Unabridged)",
                                        "The Circle Of Life / Bach Improvisations On Themes By Elton John", "Ik (Onverkort)"))



################# What Separates Elton Johns Best Songs From His Regular Song ##################
#Measures
#"valence" to measure whether a song is likely to make someone feel happy (higher valence) or sad (lower valence).The metric is measured on a scale from 0.0 to 1.0. 
#"Danceability" refers to how suitable a track is for dancing based on a combination of musical elements. A value of 0.0 is least danceable and 1.0 is most danceable.
#Speechiness detects the presence of spoken words in a track. The more words in a track, the closer the measure will be to 1.0. Less words will lead to a value closer to 0.0. 
#Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity.


#################### Descriptive Analysis ##################
#Whats Elton John's Favorite Key:
Fav_Key.1 <- data.frame(table(EJ.2$key_name)) 
Fav_Key.1$Percent <- (Fav_Key.1$Freq / sum(Fav_Key.1$Freq)) 

P1 <- ggplot(Fav_Key.1, aes(x = Var1,  y =Percent, fill = Var1)) +
  geom_bar(stat="identity") +
  labs(x = "Key Signatures",
       # y = "Percent",
       title = "Distribution of Key Signatures in Elton John Songs",
       caption = "Note: Data Downloaded from Spotify's API") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        axis.title.y=element_blank()) +
  geom_text(aes(label=paste(round(Percent*100,0), "%", sep = "")), position=position_dodge(width=0.9), vjust=-0.25);P1


#Whats Are Elton John's Hit Songs Written in Compared to His Regular Songs:

Fav_Key.2 <- NULL
Fav_Key.1 <- data.frame(table(EJ.2$Hits, EJ.2$key_name))
for(i in unique(Fav_Key.1$Var1)){
  Fav_Key.loop.1 <- subset(Fav_Key.1, Var1 == i)
  Fav_Key.loop.1$Percent <- (Fav_Key.loop.1$Freq / sum(Fav_Key.loop.1$Freq)) 
  
  Fav_Key.2 <- rbind(Fav_Key.2, Fav_Key.loop.1)
}

P2<- ggplot(Fav_Key.2, aes(x = Var2,  y =Percent, fill = Var1)) +
  geom_bar(stat="identity", width=.7, position = "dodge") + 
#facet_grid(rows = vars(Var1),  scales = "free") +
  labs(x = "Key Signatures",
       # y = "Percent",
       title = "Distribution of Key Signatures in Elton John Songs",
       caption = "Note: Data Downloaded from Spotify's API",
       fill = "Hit Song") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        axis.title.y=element_blank()) +
  coord_cartesian( ylim = c(0, .25)) +
  geom_text(aes(label=paste(round(Percent*100,0), "%", sep = "")), position = position_dodge(width = .7), vjust = -0.4);P2

#################### Descriptive Analysis ##################

EJ.3<- with(EJ.2, data.frame(artist_name, track_name, album_name, danceability, energy, loudness, acousticness, valence, tempo))
EJ.4<- reshape2::melt(EJ.3, id.vars = c("artist_name", "album_name", "track_name"))
EJ.4$variable <- stringr::str_to_title(EJ.4$variable)


P3 <- ggplot(EJ.4, aes(x = value,  fill = variable)) +
  geom_density(alpha = .5) +
  facet_wrap(variable ~  .,  scales = "free") +
  labs(#x = "Spotify's Music Metrics",
        y = "Density",
       title = "Distribution of Music Metrics",
       subtitle = "Elton John Songs",
       caption = "Note: Data Downloaded from Spotify's API") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        axis.title.x=element_blank()) 

################# Compare Elton to Other Greats ##################

Music.Pure <- rbind(get_artist_audio_features('elton john'),
                    get_artist_audio_features('the beatles'),
                    get_artist_audio_features('elvis presley'),
                    get_artist_audio_features('queen'))

Music.1 <- Music.Pure

################### Data Management ##################
#Create Decade Variable
Music.1$decade <- Music.1$album_release_year - Music.1$album_release_year %% 10

#Code Live Tracks
Music.1$Live <- ifelse(grepl("live", tolower(Music.1$track_name), fixed = TRUE) == T, "Live", "Studio")
table(Music.1$Live)

#Code Remastered
Music.1$Remastered <- "Original"
Music.1$Remastered[grepl("remastered", tolower(Music.1$track_name), fixed = TRUE)] <- "Remastered"
Music.1$Remastered[grepl("remaster", tolower(Music.1$track_name), fixed = TRUE)] <- "Remastered"
Music.1$Remastered[grepl("remastered", tolower(Music.1$album_name), fixed = TRUE)] <- "Remastered"
Music.1$Remastered[grepl("remaster", tolower(Music.1$album_name), fixed = TRUE)] <- "Remastered"

table(Music.1$Remastered)

Music.1$duration <- Music.1$duration_ms/6000 #Convert to Minutes


################### Subset ##################

#Drop Non OG Songs
Music.2 <- subset(Music.1, Live == "Studio" &
                    Remastered == "Original" &
                    album_name %!in% c("Me - Elton John Official Autobiography (Unabridged)",
                                       "The Circle Of Life / Bach Improvisations On Themes By Elton John", "Ik (Onverkort)"))

unique(factor(Music.2$album_name))

################### Distribution of Key Signatures ##################

Fav_Key.2 <- NULL
Fav_Key.1 <- data.frame(table(Music.2$artist_name, Music.2$key_name))
for(i in unique(Fav_Key.1$Var1)){
  Fav_Key.loop.1 <- subset(Fav_Key.1, Var1 == i)
  Fav_Key.loop.1$Percent <- (Fav_Key.loop.1$Freq / sum(Fav_Key.loop.1$Freq)) 
  
  Fav_Key.2 <- rbind(Fav_Key.2, Fav_Key.loop.1)
}

P4<- ggplot(Fav_Key.2, aes(x = Var2,  y =Percent, fill = Var2)) +
  geom_bar(stat="identity") +
  facet_wrap(Var1 ~. ,  scales = "free") +
  labs(x = "Key Signatures",
       # y = "Percent",
       title = "Distribution of Key Signatures",
       caption = "Note: Data Downloaded from Spotify's API") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        axis.title.y=element_blank()) +
  coord_cartesian( ylim = c(0, .25)) +
  geom_text(aes(label=paste(round(Percent*100,0), "%", sep = "")), position=position_dodge(width=0.9), vjust=-0.25)


################### Compare Metrics ##################

Music.3<- with(Music.2, data.frame(artist_name, track_name, album_name, danceability, energy, loudness, acousticness, valence, tempo))
Music.4<- reshape2::melt(Music.3, id.vars = c("artist_name", "album_name", "track_name"))

Music.4$variable <- stringr::str_to_title(Music.4$variable)

P5 <- ggplot(Music.4, aes(x = value, y = artist_name, fill = artist_name)) +
  facet_wrap(variable ~  .,  scales = "free") +
  geom_density_ridges(scale = 4, alpha = 0.5) + 
  theme_minimal() +
  labs(#x = "Music Metrics",
        y = "Density",
       title = "Distribution of Music Metrics",
       subtitle = "Elton John, Elvis, Queen, and The Beatles",
       caption = "Note: Data Downloaded from Spotify's API") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption= element_text(hjust = 0),
        axis.title.x=element_blank()) 



############## Save Plots ##################
setwd("C:/Users/Shari/OneDrive/University of California, Davis/Research Projects/Spotify/Elton John")

 ggsave(P1, 
        file = "Figure 1 - Elton John.png",
        width = 5, height = 4,  dpi = 300)
 
 ggsave(P2, 
        file = "Figure 2 - Elton John.png",
        width = 8, height = 5,  dpi = 300)
 
 ggsave(P3, 
        file = "Figure 3 - Elton John.png",
        width = 7, height = 6,  dpi = 300)
 
 ggsave(P4, 
        file = "Figure 4 - Elton John.png",
        width = 7, height = 6,  dpi = 300)
 
 ggsave(P5, 
        file = "Figure 5 - Elton John.png",
        width = 9, height = 6,  dpi = 300)
 
