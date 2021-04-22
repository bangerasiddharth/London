#L00157101
#London-crime-data.csv dataset is downloaded from Assignments & tests folder in Blackboard
#This dataset contains an overview of the crime records in London Borough Wise.
#The columns available are lsoa_code,	borough,	major_category,	minor_category,	value,	year and	month.
#New project London has been created and it has beenn synced with the Github Repository.

#---
#Q1|
#---
london_crime <- read.csv("london-crime-data.csv", na = "")                      #reading the london-crime-data.csv file into the london_crime dataframe, replacing values with empty spaces with NA
head(london_crime, 5)                                                           #displaying first 5 records from dataframe to ensure data was loaded as expected
str(london_crime)                                                               #displaying the structure of the fields in london_crime dataframe

Date <- paste("01", london_crime$month, london_crime$year, sep = "/")           #amalgamating month and year fields into a new field called Date. We have added '01' which acts as a day field, to ensure the entire day-month-year is generated
Date                                                                            #verifying that the Date column has been populated as expected

#---
#Q2|
#---
names(london_crime)                                                             #displaying names of variables in london_crime dataframe
names(london_crime)[2] <- "Borough"                                             #Renaming column borough as Borough
names(london_crime)[3] <- "MajorCategory"                                       #Renaming column Major_category as MajorCategory
names(london_crime)[4] <- "SubCategory"                                         #Renaming column Minor_category as SubCategory
names(london_crime)[5] <- "Value"                                               #Renaming column value as Value
names(london_crime)[8] <- "CrimeDate"                                           #Renaming column Date as CrimeDate
names(london_crime)                                                             #verifying that the column names have been renamed as expected

attach(london_crime)                                                            #attach is used to access used to access the variables present in the data framework without calling the  ufo_data dataframe                                                           
london_crime <- subset(
  london_crime, select=c("Borough", 
                         "MajorCategory", 
                         "SubCategory", 
                         "Value", 
                         "CrimeDate"))                                          #selecting only the required columns and discarding other columns from london_crime dataframe
detach(london_crime)
head(london_crime,5)                                                            #verifying that only the required columns and respective data has been populated in london_crime dataset

#---
#Q3|
#---
london_crime$Date <- as.Date(Date, "%d/%m/%Y")                                  #converting the data type of Date field from char to Date
str(london_crime)                                                               #viewing the structure and verifying that the Date field has Date datatype

#---
#Q4|
#---
london_crime$Borough <- factor(london_crime$Borough)                            #converting it to a factor variable                   
str(london_crime$Borough)                                                       #verifying the structure
plot(london_crime$Borough, main="No. of Crime by Borough", 
     xlab='Borough', 
     ylab='Count of crimes',
     col='#00b8e6')                                                             #Plotting the data

summary(london_crime$Borough)                                                   #Summary of data. From this we can answer the below key questions

# Key points:
# 1. Borough having the highest number of crime: Croydon - 5226
# 2. Borough having the highest number of crime: City of London - 86

#---
#Q5|
#---
london_crime$MajorCategory <- factor(london_crime$MajorCategory)                #converting it to a factor variable
str(london_crime$MajorCategory)                                                 #viewing and verifying the structure
summary(london_crime$MajorCategory)                                             #Summary of data. From this we can answer the below key questions
major_category <- summary(london_crime$MajorCategory)                       

pie(major_category,  main="% crime by Major Category")                          #Plotting the data in form of a pie chart

#colors = c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan") 
#pie(major_category,  main="The percentage of crime by major category", col = colors)

# Key points:
# 1. Major category having the highest level of crimes: Theft and Handling - 33759
# 2. Major category having the lowest level of crimes:  Sexual Offences - 917

#---
#Q6|
#---
#Assigning Regions to the respective Borough
london_crime$Region[london_crime$Borough == 'Barking and Dagenham'] <- 'East'
london_crime$Region[london_crime$Borough == 'Barnet'] <- 'North'
london_crime$Region[london_crime$Borough == 'Bexley'] <- 'East'
london_crime$Region[london_crime$Borough == 'Brent'] <- 'West'
london_crime$Region[london_crime$Borough == 'Bromley'] <- 'South'
london_crime$Region[london_crime$Borough == 'Camden'] <- 'North'
london_crime$Region[london_crime$Borough == 'Croydon'] <- 'South'
london_crime$Region[london_crime$Borough == 'Ealing'] <- 'West'
london_crime$Region[london_crime$Borough == 'Enfield'] <- 'North'
london_crime$Region[london_crime$Borough == 'Greenwich'] <- 'East'
london_crime$Region[london_crime$Borough == 'Hackney'] <- 'North'
london_crime$Region[london_crime$Borough == 'Hammersmith and Fulham'] <- 'West'
london_crime$Region[london_crime$Borough == 'Haringey'] <- 'North'
london_crime$Region[london_crime$Borough == 'Harrow'] <- 'West'
london_crime$Region[london_crime$Borough == 'Havering'] <- 'East'
london_crime$Region[london_crime$Borough == 'Hillingdon'] <- 'West'
london_crime$Region[london_crime$Borough == 'Hounslow'] <- 'West'
london_crime$Region[london_crime$Borough == 'Islington'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Kensington and Chelsea'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Kingston upon Thames'] <- 'East'
london_crime$Region[london_crime$Borough == 'Lambeth'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Lewisham'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Merton'] <- 'South'
london_crime$Region[london_crime$Borough == 'Newham'] <- 'East'
london_crime$Region[london_crime$Borough == 'Redbridge'] <- 'East'
london_crime$Region[london_crime$Borough == 'Richmond upon Thames'] <- 'West'
london_crime$Region[london_crime$Borough == 'Southwark'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Sutton'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Tower Hamlets'] <- 'South'
london_crime$Region[london_crime$Borough == 'Waltham Forest'] <- 'Central'
london_crime$Region[london_crime$Borough == 'Wandsworth'] <- 'East'
london_crime$Region[london_crime$Borough == 'Westminster'] <- 'Central'

london_crime[is.na(london_crime$Region),]                                       #Check to verify if there are any regions with NA values
#We can see that City Of London has NA values since it has no Region specified in the above list
#We will assign 'Central' as the region for Borough 'City of London'

london_crime$Region[london_crime$Borough == 'City of London'] <- 'Central'      # Assigning Region as 'Central' for Borough 'City of London'
london_crime[is.na(london_crime$Region),]                                       #Verifying again that there are no Borughs with Regions as NA
#as evident, the code returns 0 records, indicating all the Boroughs have been assigned a respective Region

#---
#Q7|
#---
london_crime$Region <- factor(london_crime$Region)
plot(london_crime$Region, 
     main="Number of Crimes by Region", 
     xlab='Region', 
     ylab='No. of crime',
     col='Maroon')
summary(london_crime$Region)                                                    #summarizing the number of crimes per Region

# Key Points:
# 1. Region having the highest number of crimes: Central - 27864
# 2. Region having the lowest number of crimes: South - 16214

#---
#Q8|
#---
highest_crime_rate <- subset(london_crime, london_crime$Region == 'Central')    #Here we are extracting the subset of data having the highest number of crimes and storing it in highest_crime_rate dataframe
lowest_crime_rate <- subset(london_crime, london_crime$Region == 'South')       #Here we are extracting the subset of data having the lowest number of crimes and storing it in lowest_crime_rate dataframe

highest_crime_rate$Region <- factor(highest_crime_rate$MajorCategory)         
lowest_crime_rate$Region <- factor(lowest_crime_rate$MajorCategory)

summary(factor(highest_crime_rate$MajorCategory))
summary(factor(lowest_crime_rate$MajorCategory))

# Key points:
# From the above derived summaries for regions with highest crime rate and regions with lowest crime rates we can easily conclude that
# Theft and Handling remains the highest and sexual assault the lowest in terms of crime figures

#---
#Q9|
#---
summarizing_highcrimes <- summary(highest_crime_rate$MajorCategory)             #storing the summary in summarizing_highcrimes dataframe
summarizing_lowcrimes <- summary(lowest_crime_rate$MajorCategory)               #storing the summary in summarizing_lowcrimes dataframe

combined_dataset <- cbind(summarizing_highcrimes, summarizing_lowcrimes)        #combining both the datasets

barplot(combined_dataset, beside=T)                                             #plotting the dataset
title(xlab = "Central Region vs South Region")
title(ylab = "Crime Count")


#---
#Q10|
#---
write.csv(london_crime, file = 'london-crime-modified.csv')
