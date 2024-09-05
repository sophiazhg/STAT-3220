##setwd("/Users/sophia/Downloads/STAT 3220/Project")
#install.packages("dplyr")
#library(dplyr)
#install.packages("writexl")
#library("writexl")

### Part 1

#read in orginial data sets
advancedNBA <- read.csv("sportsref_download.csv",na="")
salariesNBA <- read.csv("nba_salaries.csv",na="")

#combining data sets
names(salariesNBA)[names(salariesNBA) == "Player.Name"] <- "Player"
combinedNBA <- inner_join(advancedNBA, salariesNBA, by = "Player")

#removing players
uniqueNBA <- combinedNBA[!duplicated(combinedNBA$Player),] #duplicate players (who switch teams) !!! note in disclaimer
uniqueNBA2 <- na.omit(uniqueNBA) #players w/o complete info (N/A's) !!! note in disclaimer

#getting 100 observations
randomNBA <- sample_n(uniqueNBA2, 100) #careful! runs new set of 100 players everytime
randomPlayerNBA <- print(randomNBA$Player)

#filling final statistics data set
statsNBA <- data.frame(matrix(nrow=100,ncol=10))
colnames(statsNBA) <- c("Player","Salary (mils)","College (in US)","Position","Team",
                        "Games Played","Age","Free Throw %","3 Point %",
                        "True Shootings %")
statsNBA$Player <- randomNBA[,2]
statsNBA$`Salary (mils)` <- randomNBA[,29]/1000000 #salary changed to "in millions"
statsNBA$`College (in US)` <- playerColleges
statsNBA$Position <- randomNBA[,3]
statsNBA$Team <- randomNBA[,5]
statsNBA$`Games Played` <- randomNBA[,33]
statsNBA$Age <- randomNBA[,4]
statsNBA$`Free Throw %` <- randomNBA[,48]
statsNBA$`3 Point %` <- randomNBA[,41]
statsNBA$`True Shootings %` <- randomNBA[,9]

#adding colleges to vector
playerColleges <- c("North Carolina","Notre Dame","UCLA","Virginia",
                          "Ohio State","Stanford","Syracuse","Nebraska",
                          "Kentucky","None","None","Purdue", "Alabama",
                          "Washington","Florida State","Oklahoma State","None",
                          "Iowa","Arizona","None","St. Joseph's (PA)","Tulsa",
                          "Murray State","None","Kentucky","None","Arizona State",
                          "Washington","Florida State","Oregon State","Villanova",
                          "Maryland","Florida","Oregon","Rhode Island","Michigan",
                          "Kansas State","Oregon","Louisiana State","Texas",
                          "Texas Tech","North Carolina","Oregon","Washington",
                          "Nevada","St. Louis","None","Kansas","Butler",
                          "Florida State","Gonzaga","Tennessee","Memphis","Stanford",
                          "Duke","Kentucky","Gonzaga","None","North Carolina",
                          "None","None","Creighton","Texas","Florida State",
                          "Syracuse","Virgina","Louisville","Southern California",
                          "Kentucky","Virginia","Seton Hall","None","Bucknell",
                          "Michigan","Drexel","Georgia Tech","Iowa State","Texas",
                          "Texas Tech","Marquette","Iowa State",
                          "Indiana-Purdue Fort Wayne", "Indiana-Purdue Indianapolis",
                          "Wake Forest","None","North Carolina","Nevada","Oklahoma",
                          "Kentucky","Vanderbilt","Villanova","Xavier","Florida",
                          "Wake Forest","None","Colorado","Memphis",
                          "Tennessee State","South Carolina Upstate","Florida")

#response variable histogram of salaries in mils for sample of 100 observations
hist(statsNBA$`Salary (mils)`, breaks=5, xlab="Player Salaries (in millions)", 
     main="Histogram of NBA Salaries (2022-2023)")

#qualitative EDA
boxplot(`Salary (mils)`~`College (in US)` , statsNBA, ylab="Salary (in millions)")
boxplot(`Salary (mils)`~Position, statsNBA, ylab="Salary (in millions)")
boxplot(`Salary (mils)`~Team, statsNBA, ylab="Salary (in millions)")

tapply(statsNBA$`Salary (mils)`, statsNBA$`College (in US)`, summary)
tapply(statsNBA$`Salary (mils)`, statsNBA$Position, summary)
tapply(statsNBA$`Salary (mils)`, statsNBA$Team, summary)

#quantitative EDA & correlations
for(i in names(statsNBA)[6:10]){
  plot(statsNBA[,i], statsNBA$`Salary (mils)`, xlab=i, ylab="Average Salary")
}

round(cor(statsNBA[6:10],statsNBA$`Salary (mils)`),3)

save(statsNBA4, file = "statsNBA.Rdata")

write.csv(statsNBA4, "/Users/sophia/Downloads/STAT 3220/Project/nbaStats.csv")
