
# Assignment 1
#---------------
#Database Management and Sample size estimation
#----------------------------------------------
#Name: Somtoo Henry
#------------------
#B00#: B00796180
#-----------------


#---------------------------------------------------------------
# Q1
#---------------------------------------------------------------
#Install the packages tidyverse, haven and dplyr if you do not have it
install.packages("tidyverse")
install.packages("haven")
install.packages("dplyr")

#load the packages tidyverse, haven and dplyr
library(tidyverse)
library(haven)
library(dplyr)

#Import the dataset avengers
avengers <- read.csv("avengers.csv") #import the dataset and assign to object

#View the data avengers
head(avengers) #display the first 6 rows of the dataset 
view(avengers) #view the dataset avengers

# Find the total number of missing values in the data avengers
sum(is.na(avengers)) #sum the total missing values

#Find the number of missing values per column in the data avengers
colSums(is.na(avengers)) #sum the missing values per column

#----------------------------------------------------------------
# Q2
#----------------------------------------------------------------

#Clean the data to remove missing values and 
#retain complete cases and assign to a new object

Cleanset <- avengers %>% #used to transfer or connect the dataset to the function
  na.omit()  #clean the dataset by removing missing values

head(Cleanset)#Display the first 6 rows of the cleaned dataset
view(Cleanset)#View the dataset

#Subset the data to include Combateffectiveness include CombatEffectiveness 
#and assign it to the object Cleandata

Cleandata <- avengers %>% #transfer the dataset to the function
  na.omit() %>%   # Remove the missing values and tranfer the dataset to mutate 
  mutate(CombatEffectiveness=(agility+speed+strength+willpower)) #Adds a new variable to the dataset

View(Cleandata)#view the dataset
head(Cleandata)#Display the first 6 rows of the dataset

#----------------------------------------------------------
# Q3
#----------------------------------------------------------

#Create a new dataset of avengers that did not have superpowers 
#and have died to export in csv and SPSS format

Datacopy<- Cleandata%>%  #Transfer the dataset to filter
  filter(superpower=="no"& died=="yes") #filter avengers with no superpowers and those that died
view(Datacopy)#view the dataset

# Export the new dataset of avengers that
# did not have superpowers and died in csv format.
write.csv(Datacopy,"Datacopy.csv",row.names = FALSE,na="")#Export dataset in csv format

# Export the new dataset of avengers that
# did not have superpowers and died in SPSS format
write_sav(Datacopy,"Datacopy.sav") #Export dataset in SPSS format

#Summarise the new dataset "Datacopy" to report the
# mean, SD, and range values for the overall sample 
#---------------------------------------------------------
Generalsum <-Datacopy %>% #transfer dataset to summarise function
  summarise (
    N = n(), #The total number of avengers in Datacopy
    
    # Find the average of combat effectiveness
    avg_combat = mean(CombatEffectiveness, na.rm = TRUE), 
    # Find the SD of combat effectiveness
    sd_combat = sd(CombatEffectiveness, na.rm = TRUE),    
    # Find the Minimum range of combat effectiveness
    min_combat = min(CombatEffectiveness, na.rm = TRUE),  
    # Find the Maximum range combat effectiveness
    max_combat = max(CombatEffectiveness, na.rm = TRUE), 
    
    # Find the average of kills
    avg_kills = mean(kills, na.rm = TRUE),
    #Find the SD of kills
    sd_kills = sd(kills, na.rm = TRUE),
    # Find the minimum range of kills
    min_kills = min(kills, na.rm = TRUE),
    #Find the maximum range of kills
    max_kills = max(kills, na.rm = TRUE),                
    
    # Find the average of injuries
    avg_injuries = mean(injuries, na.rm = TRUE),
    # Find the SD of injuries
    sd_injuries = sd(injuries, na.rm = TRUE),
    #Find the Minimum range of injuries
    min_injuries = min(injuries, na.rm = TRUE),
    # Find the Maximum range of injuries
    max_injuries = max(injuries, na.rm = TRUE))           

print(Generalsum) #Display the data
                  #assigned to object "Generalsum" in the console


#Summarise the new dataset "Datacopy" based on 
#battlefield location #(i.e., north vs. south)
#-------------------------------------------------------------------
Sumlocation <- Datacopy %>% #transfer the dataset to group by function
  group_by(north_south) %>% # Group the dataset "Datacopy" by battlefield location ie north and south
  summarise(
    N = n(),# Total number per group
    
    # Find the average of combat effectiveness
    avg_combat = mean(CombatEffectiveness, na.rm = TRUE), 
    # Find the SD of combat effectiveness
    sd_combat = sd(CombatEffectiveness, na.rm = TRUE),    
    # Find the Minimum range of combat effectiveness
    min_combat = min(CombatEffectiveness, na.rm = TRUE),  
    # Find the Maximum range combat effectiveness
    max_combat = max(CombatEffectiveness, na.rm = TRUE), 
    
    # Find the average of kills
    avg_kills = mean(kills, na.rm = TRUE),
    #Find the SD of kills
    sd_kills = sd(kills, na.rm = TRUE),
    # Find the minimum range of kills
    min_kills = min(kills, na.rm = TRUE),
    #Find the maximum range of kills
    max_kills = max(kills, na.rm = TRUE),                
    
    # Find the average of injuries
    avg_injuries = mean(injuries, na.rm = TRUE),
    # Find the SD of injuries
    sd_injuries = sd(injuries, na.rm = TRUE),
    #Find the Minimum range of injuries
    min_injuries = min(injuries, na.rm = TRUE),
    # Find the Maximum range of injuries
    max_injuries = max(injuries, na.rm = TRUE))           

print(Sumlocation)#Display the data assigned to object Sumlocation 

#Export the summary stats for overall data and 
# based on location in csv

write.csv(Generalsum,"Generalsum.csv",row.names = FALSE,na="")   #Export generalsum in csv format
write.csv(Sumlocation,"Sumlocation.csv",row.names = FALSE,na="") #Export sumlocation in csv format

#-----------------------------------------------------------
# Q4
#-----------------------------------------------------------

#Find the battlefield that is most effective in combat 
#based on the battlefield location.

Highestcombat <- Sumlocation %>%       # Select row with max avg combat from the
filter(avg_combat == max(avg_combat))  #dataset Sumlocation and assign to object
  
print(Highestcombat)#Display the data assigned to object Highestcombat


#Find the battlefield with the most injuries based on the battlefield location.

Highestinjuries <- Sumlocation  %>%       #Select row with max avg injuries from the 
filter(avg_injuries == max(avg_injuries)) #dataset Sumlocation and assign to object

print(Highestinjuries)       #Display the data assigned to object Highestinjuries


#--------------------------------------------------------
# Q5
#--------------------------------------------------------

#Find the most erroneous variable in the mean model

Dataerror <- Datacopy %>%   # connect the dataset to the summarise function
  summarise(                        #Summarise the mean,standard deviation and Coefficient of 
                                    #Variance for Combateffectiveness,kills and injuries 
    #The average of combat          #and asssign to object.
    avg_combat = mean(CombatEffectiveness, na.rm = TRUE),
    #The of SD CombateEffectiveness
    sd_combat = sd(CombatEffectiveness, na.rm = TRUE),
    #Find the cv for CombatEffectiveness
    cv_combat = (sd_combat / avg_combat),
    
    #The average of kill
    avg_kills = mean(kills, na.rm = TRUE),
    #The SD of kills
    sd_kills = sd(kills, na.rm = TRUE),
    #Find the cv for kills
    cv_kills = (sd_kills / avg_kills),
    
    #The average of injuries
    avg_injuries = mean(injuries, na.rm = TRUE),
    #The SD of injuries
    sd_injuries = sd(injuries, na.rm = TRUE),
    #Find the CV for injuries
    cv_injuries = (sd_injuries / avg_injuries))

print(Dataerror)#Display the data assigned to object dataerror

write.csv(Dataerror,"Dataerror.csv",row.names = FALSE,na="") #Export dataerror in csv format

#create a Vector of CV values from dataerror and assign to object
Datacv <- c(Combat = Dataerror$cv_combat,     #Extract cv for combat from dataerror
            Kills = Dataerror$cv_kills,       #Extract cv for kills from dataerror
            Injuries = Dataerror$cv_injuries)  #Extract cv for injuries from dataerror

#find the variable with the most error
Datacv[which.max(Datacv)]                                     
    

#-------------------------------------------------------------
# Q8
#-------------------------------------------------------------
#Carry out a power analysis with estimate target effect size

#Install power package if you do not have it
install.packages(pwr)

library(pwr)    #Load the package pwr

# Run a Power analysis to determine required sample size 
Powertest <- pwr.t.test(    #function for power analysis test
  n=NULL,                     #Observation in each group
  d = 0.5,                    # Cohens.d hypothetical Effect size (Medium)
  sig.level = 0.05,           # Type I probability
  power = 0.80,               # Desired power (1 minus Type II probability)
  type = "two.sample",        # Two samples t-test
  alternative = "two.sided")  # Two-sided t- test

# Display power analysis results
print(Powertest) #Display the result assigned to the object Powertest

#-------------------------------------------------------------
#  Q9
#-------------------------------------------------------------

#Carry out an equivalence test to confirm enough power

#install the package "TOSTER" if you do not have it
 install.packages("TOSTER")#Code to install package

library(TOSTER)  #load the package TOSTER

# Calculate the required sample size for 90% power equivalence test
powerTOSTtwo(
  alpha             = 0.05,   #Significance level
  statistical_power = 0.90,   # power for equivalence testing
  low_eqbound_d     = -0.5,   #low equivalent bound (Cohen d "medium")
  high_eqbound_d    =  0.5)   #high equivalent bound (Cohen d "medium")

# Calculate the achieved power
powerTOSTtwo(
  alpha             = 0.05,   ## Type I error rate (significance level)
  N                 = 406,    # n per group (812/2)
  low_eqbound_d     = -0.5,   #low equivalent bound (Cohen d "medium")
  high_eqbound_d    =  0.5)   #high equivalent bound(Cohen d "medium")
#----------------------------------------------------------------
# Q10
#----------------------------------------------------------------
#Calculate effect-size with 95% confidence interval and give  
# the effect a qualitative label using Cohen scale. 

#Install package "effectsize" if you do not have it
install.packages("effectsize") #code to install package 

library(effectsize) #load package "effectsize"

# Effect size from t-statistic

t_stat  <- 4.25 #t-statistic
n1      <- 406  #group 1
n2      <- 406  #group 2  

#Calculate Cohen d

CohensD <- t_stat * sqrt((n1 + n2) / (n1 * n2)) #calculate cohen d and assign result to the object
cat("Cohen's d =", round(CohensD, 3), "\n")

# Calculate  Cohen d with 95% CI

CIresult <- t_to_d(   
  t        = 4.25,        # observed t-statistic
  df_error = 812 - 2,     # degrees of freedom (N - 2)
  paired   = FALSE,       # independent samples t-test
  ci       = 0.95)        # 95% confidence interval)

print(CIresult) #Display CIresult

# Description of Cohen d qualitative label

cohen.ES(test = "t", size = "small")  #small effect 
cohen.ES(test = "t", size = "medium") #medium effect
cohen.ES(test = "t", size = "large")  #large effect


