################################################################################
#                       Ilex Species Abundance
#Goal: to crete data table that includes state abundance info for each select Ilex species
#Erin MacMonigle
#11 December 2024

##library
#install.packages("dplyr")
#install.packages("readxl")

#load packages
library(dplyr)
library(readxl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#read in data
species <- c("I_ambigua",
             "I_amelanchier",
             "I_cassine",
             "I_collina",
             "I_coriacea",
             "I_cuthbertii",
             "I_decidua",
             "I_glabra",
             "I_krugiana",
             "I_laevigata",
             "I_longipes",
             "I_montana",
             "I_mucronata",
             "I_myrtifolia",
             "I_opaca",
             "I_opacaArenicola",
             "I_verticillata",
             "I_vomitoria")

#create for loop to read in all csv files
for (i in 1:length(species)) {
  filename <- paste0(species)
  wd <- paste0("species_data/", filename[i], "_DistributionData.csv")
  assign(filename[i], read.csv(wd))
}

#merge all files into one big file
I_all <- rbind(I_ambigua,
               I_amelanchier,
               I_cassine,
               I_collina,
               I_coriacea,
               I_cuthbertii,
               I_decidua,
               I_glabra,
               I_krugiana,
               I_laevigata,
               I_longipes,
               I_montana,
               I_mucronata,
               I_myrtifolia,
               I_opaca,
               I_opacaArenicola,
               I_verticillata,
               I_vomitoria
               )

#save combined species data into one csv
write.csv(I_all, "species_data/I_all.csv")

#read in county and state names
readCounties <- read_xls("species_data/US_county_names.xls")

countyNames <- cbind(readCounties$FID, readCounties$STATE_NAME, readCounties$COUNTY_NAME)
colnames(countyNames) <- c("OID", "State", "County")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  #cleaning data sets for county level observations


#create list with all species name in code
speciesCode <- unique(I_all$Symbol)

#remove observations without county level obs
dim(I_all) #4428 obs
I_all <- subset(I_all, I_all$County.FIP != "na")
dim(I_all) #4193 obs, removed 235 state only observations

#create a data frame with one row for each state and county
#create empty data frame to summarize presance in state by species 
abundanceMatrix <- data.frame(matrix(rep(0, 18*3142),  ncol=18, nrow=3142))

#passing column names as species and row names as states
colnames(abundanceMatrix) <- c(speciesCode)

abundanceMatrix <- cbind(abundanceMatrix, countyNames)

#~~~~~~~~~~~~~~~filling in presence data for each species~~~~~~~~~~~~~~~~~~~~~~~

#working for loop to fill in county presence for each species

##Test inputs
n <- "ILVE"
##s <- "Louisiana"
##c <- "East Feliciana"

n

#WARNING: will run for about 5 minutes, theres a lot of steps to process
#I_all
for (i in 1:nrow(I_all)){
  n <- I_all$Symbol[i]
  s <- I_all$State[i]
  c <- I_all$County[i]
  
  #subset readCounties by state so that we can find unique county
  readCounties <- subset(readCounties, readCounties$STATE_NAME == s)
  r <- match(c, readCounties$COUNTY_NAME)
  f <-  readCounties$FID[r]
  r2 <- which(abundanceMatrix$OID == f)
  
  abundanceMatrix[r2 ,match(n, speciesCode)] = 1
  
    #reset readCounties for next entry
  readCounties <- read_xls("species_data/US_county_names.xls")
}

#create a summary column of number of species by county
abundanceMatrix$sum <- rowSums(abundanceMatrix[,1:18])

#save matrix with species presence data by state into csv
write.csv(abundanceMatrix, "species_data/Ilex_species_abundance_county.csv")


#gave up on creating double nested for-loop :/
###in progress for loop to run through state and species
##for (n in 1:length(species)){
##  assignSpecies <- paste0(species[n])
##  for (i in 1:length(stateNames)){
##    if(stateNames[i] %in% assign(species[n], select(assignSpecies, 3))) states[i, n] = 4 
##    
##  }
##}
