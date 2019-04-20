library(tidyverse)


seed.order = c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
#Listing the games as they are listed going down the standard bracket

# round 1
seeds.gm01 <- c(1,16)
seeds.gm02 <- c(8,9)
seeds.gm03 <- c(5,12)
seeds.gm04 <- c(4,13)
seeds.gm05 <- c(6,11)
seeds.gm06 <- c(3,14)
seeds.gm07 <- c(7,10)
seeds.gm08 <- c(2,15)
# round 2
seeds.gm09 <- c(seeds.gm01,seeds.gm02)
seeds.gm10 <- c(seeds.gm03,seeds.gm04)
seeds.gm11 <- c(seeds.gm05,seeds.gm06)
seeds.gm12 <- c(seeds.gm07,seeds.gm08)
# round 3
seeds.gm13 <- c(seeds.gm09,seeds.gm10)
seeds.gm14 <- c(seeds.gm11,seeds.gm12)
# round 4
seeds.gm15 <- c(seeds.gm13,seeds.gm14)
#########################################################################################################################


#Here we are rearranging how we state who played "game 1", "game 2", etc. so that when we make the graph 
#that shows the rate at which brackets become "busted", and we have the games that should be the most 
#predictible first (1 v 16, 2 v 15, etc.), so that we can theoretically keep the most people with a 
#non-busted bracket in as long as possible.

# round 1
streak.gm01 <- seeds.gm01
streak.gm02 <- seeds.gm08
streak.gm03 <- seeds.gm06
streak.gm04 <- seeds.gm04
streak.gm05 <- seeds.gm03
streak.gm06 <- seeds.gm05
streak.gm07 <- seeds.gm07
streak.gm08 <- seeds.gm02
# round 2
streak.gm09 <- seeds.gm09
streak.gm10 <- seeds.gm12
streak.gm11 <- seeds.gm11
streak.gm12 <- seeds.gm10
# round 3
streak.gm13 <- seeds.gm13
streak.gm14 <- seeds.gm14
# round 4
streak.gm15 <- seeds.gm15

perfect_region <- c(1,2,3,4,5,6,7,8,1,2,3,4,1,2,1)
#########################################################################################################################


#Reading in the dataset we want to access
mydata <- read.csv("C:/Users/bryan/OneDrive/Desktop/NCAA Tournament Reseach Project/Actual Project Files/Sources & Data Used/Data Files/Fivethirtyeight/fivethirtyeight_ncaa_forecasts_2015.csv")

region_name <- "Midwest"
tournament_year <- 2015

#Specifying the setions of the data that we only want to look at.
#The date that is needed to be put here for "forcast_date" is the date of the tournament that is after the play-in games
#and when the tournament has its 64 teams.
#FOR YEARS 20.18-20.16
regiondata <- mydata %>% filter(gender=="mens",team_region==region_name,rd1_win == 1, forecast_date=="3/17/2019") %>%
              mutate(team_seed_num = as.numeric(gsub("[^0-9\\.]", "", team_seed))) %>%
              arrange(team_seed_num) %>% select(-playin_flag,-team_alive,-team_id,-team_slot,-team_rating)


#For 20.15 Data only   ################################################################################################
# regiondata <- mydata %>% filter(team_region==region_name,rd1_win == 1, forecast_date=="3/17/2019") %>%
# mutate(team_seed_num = as.numeric(gsub("[^0-9\\.]", "", team_seed))) %>%
# arrange(team_seed_num) %>% select(-playin_flag,-team_alive)
# ######################################################################################################################


#Reading in the probabilities that were from the Excel sheet that we read in.
######################################################################################################################
#Want to get this to work eventually


# game.p <- c(regiondata[seeds.gm01,"rd2_win"], regiondata[seeds.gm02,"rd2_win"], regiondata[seeds.gm03,"rd2_win"],
#             regiondata[seeds.gm04,"rd2_win"], regiondata[seeds.gm05,"rd2_win"], regiondata[seeds.gm06,"rd2_win"],
#             regiondata[seeds.gm07,"rd2_win"], regiondata[seeds.gm08,"rd2_win"], regiondata[seeds.gm09,"rd3_win"],
#             regiondata[seeds.gm10,"rd3_win"], regiondata[seeds.gm11,"rd3_win"], regiondata[seeds.gm12,"rd3_win"],
#             regiondata[seeds.gm13,"rd4_win"], regiondata[seeds.gm14,"rd4_win"], regiondata[seeds.gm15,"rd5_win"])
gm.p01 <- regiondata[seeds.gm01,"rd2_win"]
gm.p02 <- regiondata[seeds.gm02,"rd2_win"]
gm.p03 <- regiondata[seeds.gm03,"rd2_win"]
gm.p04 <- regiondata[seeds.gm04,"rd2_win"]
gm.p05 <- regiondata[seeds.gm05,"rd2_win"]
gm.p06 <- regiondata[seeds.gm06,"rd2_win"]
gm.p07 <- regiondata[seeds.gm07,"rd2_win"]
gm.p08 <- regiondata[seeds.gm08,"rd2_win"]
gm.p09 <- regiondata[seeds.gm09,"rd3_win"]
gm.p10 <- regiondata[seeds.gm10,"rd3_win"]
gm.p11 <- regiondata[seeds.gm11,"rd3_win"]
gm.p12 <- regiondata[seeds.gm12,"rd3_win"]
gm.p13 <- regiondata[seeds.gm13,"rd4_win"]
gm.p14 <- regiondata[seeds.gm14,"rd4_win"]
gm.p15 <- regiondata[seeds.gm15,"rd5_win"]

####

sim_bracket <- function ()  {
winner.gm15 <- sample(seeds.gm15,1,prob=gm.p15,replace=T)

if (winner.gm15 %in% seeds.gm13)  {
  winner.gm13 <- winner.gm15
  winner.gm14 <- sample(seeds.gm14,1,prob=gm.p14,replace=T)
} else {
  winner.gm14 <- winner.gm15
  winner.gm13 <- sample(seeds.gm13,1,prob=gm.p13,replace=T)
}

if (winner.gm14 %in% seeds.gm11)  {
  winner.gm11 <- winner.gm14
  winner.gm12 <- sample(seeds.gm12,1,prob=gm.p12,replace=T)
} else {
  winner.gm12 <- winner.gm14
  winner.gm11 <- sample(seeds.gm11,1,prob=gm.p11,replace=T)
}

if (winner.gm13 %in% seeds.gm09)  {
  winner.gm09 <- winner.gm13
  winner.gm10 <- sample(seeds.gm10,1,prob=gm.p10,replace=T)
} else {
  winner.gm10 <- winner.gm13
  winner.gm09 <- sample(seeds.gm09,1,prob=gm.p09,replace=T)
}

if (winner.gm12 %in% seeds.gm07) {
  winner.gm07 <- winner.gm12
  winner.gm08 <- sample(seeds.gm08,1,prob=gm.p08,replace=T)
} else {
  winner.gm08 <- winner.gm12
  winner.gm07 <- sample(seeds.gm07,1,prob=gm.p07,replace=T)
}

if (winner.gm11 %in% seeds.gm05) {
  winner.gm05 <- winner.gm11
  winner.gm06 <- sample(seeds.gm06,1,prob=gm.p06,replace=T)
} else {
  winner.gm06 <- winner.gm11
  winner.gm05 <- sample(seeds.gm05,1,prob=gm.p05,replace=T)
}

if (winner.gm10 %in% seeds.gm03) {
  winner.gm03 <- winner.gm10
  winner.gm04 <- sample(seeds.gm04,1,prob=gm.p04,replace=T)
} else {
  winner.gm04 <- winner.gm10
  winner.gm03 <- sample(seeds.gm03,1,prob=gm.p03,replace=T)
}

if (winner.gm09 %in% seeds.gm01) {
  winner.gm01 <- winner.gm09
  winner.gm02 <- sample(seeds.gm02,1,prob=gm.p02,replace=T)
} else {
  winner.gm02 <- winner.gm09
  winner.gm01 <- sample(seeds.gm01,1,prob=gm.p01,replace=T)
}

#The new ordering that we want the games so that we have the highest rate of survival for the simulated brackets
#before they get busted (new ordering of gm01 = 1 v 16, gm02 = 2 v 15, gm03 = 3 v 14, etc.)
return(c(winner.gm01,winner.gm08,winner.gm06,winner.gm04,winner.gm03,
         winner.gm05,winner.gm07,winner.gm02,winner.gm09,winner.gm12,
         winner.gm11,winner.gm10,winner.gm13,winner.gm14,winner.gm15))

}


####
#The "forecast_date" here designates the date for the tournament
regionenddata <- mydata %>% filter(team_region==region_name,forecast_date =="2019-04-04",rd1_win=="1") %>% 
                     mutate(team_seed_num = as.numeric(gsub("[^0-9\\.]", "", team_seed))) %>%
                         arrange(team_seed_num) %>% select(-rd6_win,-rd7_win,-playin_flag)


actual <- c(sum(regionenddata[seeds.gm01,"rd2_win"]*seeds.gm01), sum(regionenddata[seeds.gm02,"rd2_win"]*seeds.gm02), sum(regionenddata[seeds.gm03,"rd2_win"]*seeds.gm03),
            sum(regionenddata[seeds.gm04,"rd2_win"]*seeds.gm04), sum(regionenddata[seeds.gm05,"rd2_win"]*seeds.gm05), sum(regionenddata[seeds.gm06,"rd2_win"]*seeds.gm06),
            sum(regionenddata[seeds.gm07,"rd2_win"]*seeds.gm07), sum(regionenddata[seeds.gm08,"rd2_win"]*seeds.gm08), sum(regionenddata[seeds.gm09,"rd3_win"]*seeds.gm09), 
            sum(regionenddata[seeds.gm10,"rd3_win"]*seeds.gm10), sum(regionenddata[seeds.gm11,"rd3_win"]*seeds.gm11), sum(regionenddata[seeds.gm12,"rd3_win"]*seeds.gm12),
            sum(regionenddata[seeds.gm13,"rd4_win"]*seeds.gm13), sum(regionenddata[seeds.gm14,"rd4_win"]*seeds.gm14), sum(regionenddata[seeds.gm15,"rd5_win"]*seeds.gm15))



# # round 1
# actual.gm01 <- sum(regionenddata[seeds.gm01,"rd2_win"]*seeds.gm01)
# actual.gm02 <- sum(regionenddata[seeds.gm02,"rd2_win"]*seeds.gm02)
# actual.gm03 <- sum(regionenddata[seeds.gm03,"rd2_win"]*seeds.gm03)
# actual.gm04 <- sum(regionenddata[seeds.gm04,"rd2_win"]*seeds.gm04)
# actual.gm05 <- sum(regionenddata[seeds.gm05,"rd2_win"]*seeds.gm05)
# actual.gm06 <- sum(regionenddata[seeds.gm06,"rd2_win"]*seeds.gm06)
# actual.gm07 <- sum(regionenddata[seeds.gm07,"rd2_win"]*seeds.gm07)
# actual.gm08 <- sum(regionenddata[seeds.gm08,"rd2_win"]*seeds.gm08)
# # round 2
# actual.gm09 <- sum(regionenddata[seeds.gm09,"rd3_win"]*seeds.gm09)
# actual.gm10 <- sum(regionenddata[seeds.gm10,"rd3_win"]*seeds.gm10)
# actual.gm11 <- sum(regionenddata[seeds.gm11,"rd3_win"]*seeds.gm11)
# actual.gm12 <- sum(regionenddata[seeds.gm12,"rd3_win"]*seeds.gm12)
# # round 3
# actual.gm13 <- sum(regionenddata[seeds.gm13,"rd4_win"]*seeds.gm13)
# actual.gm14 <- sum(regionenddata[seeds.gm14,"rd4_win"]*seeds.gm14)
# # round 4
# actual.gm15 <- sum(regionenddata[seeds.gm15,"rd5_win"]*seeds.gm15)
# 
# actual <- c(actual.gm01,actual.gm08,actual.gm06,actual.gm04,actual.gm03,
#             actual.gm05,actual.gm07,actual.gm02,actual.gm09,actual.gm12,
#             actual.gm11,actual.gm10,actual.gm13,actual.gm14,actual.gm15)
# ####

total.sim <- 5000
bracket.sim <- data.frame(t(replicate(total.sim,sim_bracket())))

colnames(bracket.sim) <- c("gm01","gm02","gm03","gm04","gm05",
                           "gm06","gm07","gm08","gm09","gm10",
                           "gm11","gm12","gm13","gm14","gm15")


                      ##### Added for Project 2019 Evaluation #####
#South
possible1 <- bracket.sim %>% mutate(Madness1 = Madness1) %>% group_by(gm01,gm02,gm03,gm04,gm05,gm06,gm07,gm08,gm09,gm10,gm11,gm12,gm13,gm14,gm15,Madness1) %>%
                             filter(gm01 == 1, gm02 == 2, gm03 == 3, gm04 == 4, gm05 == 5, gm07 == 7, gm09 == 1, gm10 == 2, gm12 == 5, gm13 == 1, gm14 != 6)
possible1 <- unique(possible1)
possible1 <- possible1 %>% filter(Madness1 > 5)





############################################################################################################

# bracket.sim <- bracket.sim %>% group_by(gm01,gm02,gm03,gm04,gm05,gm06,gm07,gm08,gm09,gm10,gm11,gm12,gm13,gm14,gm15) %>% 
#          summarise(sim.n = n()) %>% arrange(desc(sim.n)) %>% mutate(sim.prop = sim.n/total.sim)

##########################################################################################
##########################################################################################
##########################################################################################

#Precision is the value generated from the distance formula between the two vectors the actual region results and 
#the person's (generated) predicted/chosen results. The lower the Precision value, the more accurate the person
#was with their bracket, with a value of 0 meaning they guessed all games in that region correctly. Therfore,
#having a Precision value of 0 is what one utimately desires.
Precision <- sqrt(((bracket.sim$gm01 - actual[1])^2) + ((bracket.sim$gm02 - actual[2])^2) + ((bracket.sim$gm03 - actual[3])^2) +
                      ((bracket.sim$gm04 - actual[4])^2) + ((bracket.sim$gm05 - actual[5])^2) + ((bracket.sim$gm06 - actual[6])^2) +
                      ((bracket.sim$gm07 - actual[7])^2) + ((bracket.sim$gm08 - actual[8])^2) + ((bracket.sim$gm09 - actual[9])^2) +
                      ((bracket.sim$gm10 - actual[10])^2) + ((bracket.sim$gm11 - actual[11])^2) + ((bracket.sim$gm12 - actual[12])^2) +
                      ((bracket.sim$gm13 - actual[13])^2) + ((bracket.sim$gm14 - actual[14])^2) + ((bracket.sim$gm15 - actual[15])^2))



#The Madness1 is the value generated from the distance formula between the two vectors the "perfect" region and the
#person's (generated) predicted/chosen results. The lower the Madness1 value, the lower the amount of 
#"madness"/upsets/unexpectedness the person thought that the region would have. Therefore, having a Madness1 value
#of 0 would mean that the person did not expect an upsets to happen and for each match up, the higher seeded team 
#would win. A high value for Madness1 would mean that the person expected a high amount of upsets.
Madness1 <- sqrt(((bracket.sim$gm01 - perfect_region[1])^2)  + ((bracket.sim$gm02 - perfect_region[2])^2)  + ((bracket.sim$gm03 - perfect_region[3])^2) +
                 ((bracket.sim$gm04 - perfect_region[4])^2)  + ((bracket.sim$gm05 - perfect_region[5])^2)  + ((bracket.sim$gm06 - perfect_region[6])^2) +
                 ((bracket.sim$gm07 - perfect_region[7])^2)  + ((bracket.sim$gm08 - perfect_region[8])^2)  + ((bracket.sim$gm09 - perfect_region[9])^2) +
                 ((bracket.sim$gm10 - perfect_region[10])^2) + ((bracket.sim$gm11 - perfect_region[11])^2) + ((bracket.sim$gm12 - perfect_region[12])^2)+
                 ((bracket.sim$gm13 - perfect_region[13])^2) + ((bracket.sim$gm14 - perfect_region[14])^2) + ((bracket.sim$gm15 - perfect_region[15])^2))


#    average.madness <- sum(madness_score)/total.sim


#
##########################################################################################

bracket.sim <- bracket.sim %>% mutate(Madness1 = Madness1, Precision)

##evaluate used to be x_first
evaluate <- bracket.sim %>% group_by(gm01,gm02,gm03,gm04,gm05,gm06,gm07,gm08,gm09,gm10,gm11,gm12,gm13,gm14,gm15,
           Madness1,Precision) %>% arrange(Precision)
group.brackets <- evaluate %>% summarise(count = n()) %>% mutate(sim.prop = count/total.sim)

ranking <-  evaluate %>% mutate(id = row_number())


z.top_10_per.2019South <- evaluate %>% head(500)
z.group.2019South <- z.top_10_per.2019South %>% summarise(count = n()) %>% mutate(sim.prop = count/total.sim)
min_madness <- min(z.group.2019South$Madness1)
max_madness <- max(z.group.2019South$Madness1)
min_precision <- min(z.group.2019South$Precision)
max_precision <- max(z.group.2019South$Precision)



# write.csv(evaluate, "C:/Users/bryan/OneDrive/Desktop/NCAA Tournament Reseach Project/Actual Project Files/Good Working R Files/Bracket_Survival_R_Files/Simulation Results/Precision_2019_South")

mean(evaluate$Madness1)
sd(evaluate$Madness1)

plot(Precision, Madness1, pch = 19, cex = .75)
title(main = paste(region_name,tournament_year), font.main=3)
abline(lm(evaluate$Madness1~evaluate$Precision), col="red", lwd = 2)


# png("2019South.png")
# #Returns all the entries that have a Madness value in the same range as those brackets that were the top 10% most precise
# plot(Precision, Madness1, pch = 19, cex = .75, col=ifelse(Madness1<min_value,"black",ifelse(Madness1>max_value,"black","red")))
# title(main = paste(region_name,tournament_year), font.main=3)
# abline(lm(evaluate$Madness1~evaluate$Precision), lwd = 2, col="blue")
# dev.off()





#######
#What is this stuff?
# streak.calc <- function(x,y)  {
#   
#   value <- sum(cumprod(x == y))
#   value
# }
# #######
### find the streaks
temp <- apply(evaluate[,1:15],1,streak.calc,actual)
evaluate$streak <- temp

# proportion of simulations to predict n games in a row correctly, for given region
survive01_prop <- sum(evaluate[evaluate$streak >= 1,"sim.prop"])
survive02_prop <- sum(evaluate[evaluate$streak >= 2,"sim.prop"])
survive03_prop <- sum(evaluate[evaluate$streak >= 3,"sim.prop"])
survive04_prop <- sum(evaluate[evaluate$streak >= 4,"sim.prop"])
survive05_prop <- sum(evaluate[evaluate$streak >= 5,"sim.prop"])
survive06_prop <- sum(evaluate[evaluate$streak >= 6,"sim.prop"])
survive07_prop <- sum(evaluate[evaluate$streak >= 7,"sim.prop"])
survive08_prop <- sum(evaluate[evaluate$streak >= 8,"sim.prop"])
#Predicted first round, of given region
survive09_prop <- sum(evaluate[evaluate$streak >= 9,"sim.prop"])
survive10_prop <- sum(evaluate[evaluate$streak >= 10,"sim.prop"])
survive11_prop <- sum(evaluate[evaluate$streak >= 11,"sim.prop"])
survive12_prop <- sum(evaluate[evaluate$streak >= 12,"sim.prop"])
#Predicted second round, of given region
survive13_prop <- sum(evaluate[evaluate$streak >= 13,"sim.prop"])
survive14_prop <- sum(evaluate[evaluate$streak >= 14,"sim.prop"])
#Predicted third round, of given region
survive15_prop <- sum(evaluate[evaluate$streak >= 15,"sim.prop"])
#Predicted whole region correct


games_survived <- c(survive01_prop,survive02_prop,survive03_prop,survive04_prop,survive05_prop,
                    survive06_prop,survive07_prop,survive08_prop,survive09_prop,survive10_prop,
                    survive11_prop,survive12_prop,survive13_prop,survive14_prop,survive15_prop)


#Plot the percentage of brackets that survived for each of the games played, for one region
# plot(games_survived, type="o", col="blue", xlab="Game",ylab="Precentage of Brackets Remaining")
# title(main=paste(region_name,tournament_year), font.main=3)
# axis(1, at=1:15, lab=c("1 v 16","2 v 15","3 v 14","4 v 13","5 v 12","6 v 11","7 v 10","8 v 9","actual.gm01 v actual.gm02",
#                        "actual.gm07 v actual.gm08","actual.gm05 v actual.gm06","actual.gm03 v actual.gm04",
#                        "actual.gm09 v actual.gm10","actual.gm11 v actual.gm12","actual.gm13 v actual.gm14"))




##     Find The Best Range of Madness To Have Your Bracket In To Have the Best Precision
South2019.Precison0.0_2.0   <- evaluate %>% filter(Madness1 < 2,  Madness1 >= 0)  %>% summary(Precision)
South2019.Precison2.0_4.0   <- evaluate %>% filter(Madness1 < 4,  Madness1 >= 2)  %>% summary(Precision)
South2019.Precison4.0_6.0   <- evaluate %>% filter(Madness1 < 6,  Madness1 >= 4)  %>% summary(Precision)
South2019.Precison6.0_8.0   <- evaluate %>% filter(Madness1 < 8,  Madness1 >= 6)  %>% summary(Precision)
South2019.Precison8.0_10.0  <- evaluate %>% filter(Madness1 < 10, Madness1 >= 8)  %>% summary(Precision)
South2019.Precison10.0_12.0 <- evaluate %>% filter(Madness1 < 12, Madness1 >= 10) %>% summary(Precision)
South2019.Precison12.0_14.0 <- evaluate %>% filter(Madness1 < 14, Madness1 >= 12) %>% summary(Precision)
South2019.Precison14.0_16.0 <- evaluate %>% filter(Madness1 < 16, Madness1 >= 14) %>% summary(Precision)
South2019.Precison16.0_18.0 <- evaluate %>% filter(Madness1 < 18, Madness1 >= 16) %>% summary(Precision)
South2019.Precison18.0_20.0 <- evaluate %>% filter(Madness1 < 20, Madness1 >= 18) %>% summary(Precision)
South2019.Precison20.0_22.0 <- evaluate %>% filter(Madness1 < 22, Madness1 >= 20) %>% summary(Precision)
South2019.Precison22.0_24.0 <- evaluate %>% filter(Madness1 < 24, Madness1 >= 22) %>% summary(Precision)
South2019.Precison24.0_26.0 <- evaluate %>% filter(Madness1 < 26, Madness1 >= 24) %>% summary(Precision)
South2019.Precison26.0_28.0 <- evaluate %>% filter(Madness1 < 28, Madness1 >= 26) %>% summary(Precision)
South2019.Precison28.0_30.0 <- evaluate %>% filter(Madness1 < 30, Madness1 >= 28) %>% summary(Precision)

South2019.Precison0.0_2.0
South2019.Precison2.0_4.0
South2019.Precison4.0_6.0
South2019.Precison6.0_8.0
South2019.Precison8.0_10.0
South2019.Precison10.0_12.0
South2019.Precison12.0_14.0
South2019.Precison14.0_16.0
South2019.Precison16.0_18.0
South2019.Precison18.0_20.0
South2019.Precison20.0_22.0
South2019.Precison22.0_24.0
South2019.Precison24.0_26.0
South2019.Precison26.0_28.0
South2019.Precison28.0_30.0

# do.call(cbind, lapply(mydf, summary))
# do.call(cbind, lapply(mydf, summary))

ifmean(evaluate$Madness1)
# x_second <- bracket.sim %>% 
#   mutate(streak = streak.calc(c(gm01,gm02,gm03,gm04,gm05,gm06,gm07,gm08,gm09,gm10,gm11,gm12,gm13,gm14,gm15),actual)) 









#Workspace
#To do over break....I need to run the model on the 2019 and 2014 data, and then make graphs for each
#I

