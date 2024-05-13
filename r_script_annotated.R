################### R SCRIPT ################### 

## This R script can only be running with the dataset from CBS
## The input data is accessible only by CBS permission
## The input data has information from smartphones and administrative data as follows:
## 1. user id: a unique identifier for each user 
## 2. start time: a start time for each activity (stop or track) of each user (yyyy-mm-dd hh:mm:ss)
## 3. end time: an end time for each activity (stop or track) of each user (yyyy-mm-dd hh:mm:ss) 
## 4. label (stop or track): generated label from the app to identify whether the person is moving (track) or not(stop)
## 5. origin: the origin divides into 3 categories; (1) classifier_insert: if the label is originally generated from the app and not changed by the users, (2) mutation: if the users change the modality purpose to one of the options, and (3) user_insert: if the users added the missing stops or tracks from the options
## 6. modality purposes: 11 labels for travel modes and purposes, including "others" option
## 7. latitude and longitude: location data of specific stops
## 8. gender: categorical variable male (1) and female (2)
## 9. age: continuous variable
## 10. hh_npersons: the number of household members
## 11. hh_type: type of household (categorical data)
## 12. hh_income: household income in percentile
## 13. hh_income_standar: standardization of household income in percentile
## 14. soc_eco: socio-economic status (categorical data)
## 15. lease_car: car leasing yes (1) and no (0)
## 16. fte_work: number of fte (from 0 to 1)

### INPUT DATA ###
# Import dataset
cleaned <- read.csv ("INPUT_DATA.csv")

# Keep unique user id
library(gtsummary)
library(dplyr)
unique_user <- cleaned %>% distinct(user_id, .keep_all = TRUE)

# Make groups of age, income, and fte work
unique_user$age_group <- cut(unique_user$age,c(16,30,45,60,75, Inf),c("16-30","31-45","46-60","61-75",">75"),include.lowest = TRUE)
unique_user$income_group <- cut(unique_user$hh_income,c(0,25,50,75,100),c("Q1","Q2","Q3","Q4"),include.lowest = TRUE)
unique_user$fte_group <- cut(unique_user$fte_work,c(0,.2,.4,.6,.8,1),c("0-0.2","0.21-0.4","0.41-0.6","0.61-0.8","0.81-1"),include.lowest = TRUE)

# Change data type of socio-economic and lease car to factor
unique_user$soc_eco <- as.factor(unique_user$soc_eco)
unique_user$lease_car <- as.factor(unique_user$lease_car)

# Summary of the data
unique_user %>% 
  dplyr::select (c(user_id,modality_purpose,origin,gender,hh_npersons,hh_type,hh_income,hh_income_standard,soc_eco,lease_car,age_group,income_group,fte_group)) %>%
  tbl_summary()


### CHANGE MISSING INFORMATION OF ADMINISTRATIVE DATA ###
## Unknown category was changed to 999 because some of the categories might not include in both train and test later therefore numerical data is needed.
## Data from 38 users is completely missing

# Change all missing data from each column of administrative data to 999 (unknown)
library(tidyverse)
cleaned <- cleaned %>% mutate(
  gender = ifelse(is.na(gender), 999, gender),
  age = ifelse(is.na(age), 999, age),
  hh_npersons = ifelse(is.na(hh_npersons), 999, hh_npersons),
  hh_type = ifelse(is.na(hh_type), 999, hh_type),
  hh_income = ifelse(is.na(hh_income), 999, hh_income),
  hh_income_standard = ifelse(is.na(hh_income_standard), 999, hh_income_standard),
  soc_eco = ifelse(is.na(soc_eco), 999, soc_eco),
  lease_car = ifelse(is.na(lease_car), 999, lease_car),
  fte_work = ifelse(is.na(fte_work), 999, fte_work))


### DURATION AND DISTANCE ###
## This data does not have information about distance and duration of the trips therefore, it should manually added. First, the start and end timestamp should be changed to POSIXlt format to calculate duration of the trips.
## After the start and end time are in the correct format then the time difference can be calculated. The duration of the trips are calculated based on the time difference between start and end time. 
## The distance of each trip will also be calculated. The distance is defined according to the Haversine great circle distance or the shortest distance between two points. This method assumes a spherical earth and ignoring ellipsoidal effects.

# Remove unnecessary characters 
library(stringr)
rep_str <- c("T"=" ","Z"="")
cleaned$start_timestamp <- str_replace_all(cleaned$start_timestamp, rep_str) #delete all "T" and "Z" characters in start time
cleaned$end_timestamp <- str_replace_all(cleaned$end_timestamp, rep_str) #delete all "T" and "Z" characters in start time

# Change the format of start and end time
cleaned$start_timestamp <- strptime(cleaned$start_timestamp,"%Y-%m-%d %H:%M:%S") #change format of start time
cleaned$end_timestamp <- strptime(cleaned$end_timestamp,"%Y-%m-%d %H:%M:%S") #change format of end time

# Adding time difference in minutes as duration
cleaned$duration <- difftime(cleaned$end_timestamp,cleaned$start_timestamp,units="mins")

# Create a numeric vector for distance
distance <- numeric()

# Calculate distance based on latitude and longitude of each trip
library(geosphere)
for (i in 2:nrow(cleaned)) {
  distance [i] <- ifelse(cleaned$label [i] == "track",(distHaversine(cleaned[i-1,7:8],cleaned[i+1,7:8],r=6378137)),NA)
}
str(distance)

# Adding distance to dataset
cleaned$distance <- distance

# Replace all NAs to 0 
cleaned$distance <- replace(cleaned$distance,is.na(cleaned$distance),0)

## All distances only calculate track between two stops (single trip), if there is a track not in between two stops then it becomes 0
## All NAs values changed to 0


### CHECK MULTIPLE TRACKS AND STOPS ###
# Check the unique id
unique_id <- c()
for (i in 1:nrow(cleaned)){
  unique_id [i] <- ifelse(cleaned$user_id[i]==cleaned$user_id[i+1],1,0)
}
cleaned$id <- unique_id
prop.table(table(unique_id)) 

# Check the multiple label
multi <- c()
for (i in 1:nrow(cleaned)){
  multi [i] <- ifelse(cleaned$label[i] == cleaned$label[i+1],1,0)
}
cleaned$multi <- multi

# Combine unique id and multiple labels
check <- c()
for (i in 1:nrow(cleaned)){
  check <- ifelse(cleaned$id == 1 & cleaned$multi == 1, "yes","no")
}
cleaned$check <- check
prop.table(table(check)) 


### ADDING MORE INFORMATION ###
# Time of the day (from 1 to 24)
library(lubridate)
cleaned$tod <- hour(cleaned$start_timestamp)
barplot(table(cleaned$tod)) #to show that most activities are being done during the day

# Day of the week (from monday to sunday)
cleaned$dow <- wday(cleaned$start_timestamp)
barplot((table(cleaned$dow))) # to get more information about which day is the most and least busy day 

# Merge the transport modes and/or transport purpose
cleaned$modality_purpose <- as.factor(cleaned$modality_purpose)
levels(cleaned$modality_purpose)<-c("pick-up","pick-up","NA","education","other","transit","sport","home","visits","work","work","shopping","stop","track","other","car","car","car","bus","bike","bike","metro","tram","train","foot")
## Because some of the labels are not containing many data, some of the categories are merged
## Later, the variable of interest is only for the travel purpose


### ADDING INFORMATION FROM THE PREVIOUS TRIP BEFORE A STOP ###
# Add transport mode as a new column
mode <- c()
for (i in 2:nrow(cleaned)){
  mode [i] <- ifelse(cleaned$label [i] == "stop", as.character(cleaned$modality_purpose)[i-1],NA)
  mode [i] <- ifelse(cleaned$id [i-1] == 0, NA, mode[i])
}
cleaned$mode <- mode

# Add duration of the track before stop
duration_mode <- c()
for (i in 1:nrow(cleaned)){
  duration_mode [i] <- ifelse(!(is.na(cleaned$mode[i])), cleaned$duration [i-1], NA)
}
cleaned$duration_mode <- duration_mode

# Add distance of the track before stop
distance_mode <- c()
for (i in 1:nrow(cleaned)){
  distance_mode [i] <- ifelse(!(is.na(cleaned$mode[i])), cleaned$distance [i-1], NA)
}
cleaned$distance_mode <- distance_mode


### DATA SUMMARY ###
# Summary of modality purpose, origin, and label of the data
prop.table(table(cleaned$modality_purpose))
prop.table(table(cleaned$origin))
prop.table(table(cleaned$label))


### DURATION OF THE STOPS AND TRACKS ###
# Calculate max, min, median, and mean of each modality purpose's duration
library(tidyverse)
duration_time <- cleaned %>% 
  group_by(modality_purpose) %>% 
  summarize(max = max(duration), min = min(duration), median = median(duration), mean = mean(duration))
duration_time

# Change the categorical data type to factor
cleaned$label <- as.factor(cleaned$label)
cleaned$origin <- as.factor(cleaned$origin)
cleaned$modality_purpose <- as.factor(cleaned$modality_purpose)

# Change the difftime category to numeric (per minutes)
cleaned$duration <- as.numeric(cleaned$duration)

# See the data distribution of trip purposes
library(ggplot2)
stop <- cleaned[cleaned$label == "stop",] #select only stops
stop <- stop[stop$modality_purpose != c("stop"),,drop = FALSE] #remove unspecified stops (stop)
stop <- stop[stop$modality_purpose != c("NA"),,drop = FALSE] #remove undefined stops (NA)

# Plot of duration of each stop
library(scales)
ggplot(stop)+
  geom_boxplot(mapping = aes(x=modality_purpose,
                             y=duration),
               color = "orange", fill = "orange", 
               alpha = 0.1) +
  labs(y="Duration (in minutes)", x="Travel purpose") +
  scale_y_continuous(labels = label_comma()) + 
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))

# See the data distribution of travel mode
track <- cleaned[cleaned$label == "track",] #select only trips
track <- track[track$modality_purpose != "track",,drop = TRUE] #remove unspecified stops (track)
track <- track[track$modality_purpose != "R: NA",,drop = TRUE] #remove undefined stops (NA)

# Plot of duration of each mode
ggplot(track)+
  geom_boxplot(mapping = aes(x=modality_purpose,
                             y=duration),
               color = "green", fill = "green", 
               alpha = 0.1) +
  labs(y="Duration (in minutes)", x="Travel mode") +
  scale_y_continuous(labels = label_comma()) + 
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))


# Remove the duration more than 24 hours (1440 minutes)
remove_duration_track <- track[-which(track$duration>1440),]

# Plot of cleaned duration of tracks
ggplot(remove_duration_track)+
  geom_boxplot(mapping = aes(x=modality_purpose,
                             y=duration),
               color = "red", fill = "red", 
               alpha = 0.1) +
  labs(y="Duration (in minutes)", x="Travel mode")+
  scale_y_continuous(labels = label_comma()) + 
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))


### DISTANCE OF THE TRACKS ###
# Combine stop and trip data
data <- rbind(remove_duration_track,stop)

# Check the number of stops and trips for each unique user
stop_trip <- data %>% 
  group_by(user_id) %>% 
  summarize(stop = sum(label=="stop"),
            track = sum(label=="track")) %>%
  arrange(track)
stop_trip

# The number of unique users
length(unique(stop_trip$user_id))

# Select trips only 
data_trip <- data[data$label == "track",]

# Summary of travel distance
distance_in_meter <- data_trip %>% 
  group_by(modality_purpose) %>% 
  summarize(max = max(distance), 
            min = min(distance), 
            median = median(distance), 
            mean = mean(distance))
distance_in_meter

# Plot of the travel distance
ggplot(data_trip)+
  geom_boxplot(mapping = aes(x=modality_purpose,
                             y=distance),
               color = "blue", fill = "blue", 
               alpha = 0.1) +
  labs(y="Distance (in meter)", x="Travel mode") +
  scale_y_continuous(labels = label_comma()) + 
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))

# Plot of distance and duration of transport mode
ggplot(data = data_trip) + 
  geom_point(aes(x = duration, 
                 y = distance, 
                 color = modality_purpose)) + 
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) + 
  labs(x = "duration (in minutes)", 
       y = "distance (in meter)",
       color = "travel mode") 


### RECALCULATION OF THE SAME LOCATION (55 METERS ACCURACY) ###
## After recalculate the the latitude and longitude of the same location, frequency of the visits and the weighting per person per day were added
## The same condition was applied for the same label of trip purpose

# Get location id to keep the order of data
stop$location_id <- c(1:nrow(stop))

# Set 4.5 decimals for latitude and longitude
lon <- ceiling((round(stop$longitude,4))/0.0005)*0.0005
lat <- ceiling((round(stop$latitude,4))/0.0005)*0.0005
stop$uniq_loc <- paste(lat,lon, sep = ",")

# Number of unique location
length(unique(stop$uniq_loc))
nrow(stop)

# Merge the frequency of unique location with the stop data
a <- stop |>
  group_by(user_id,uniq_loc) |>
  summarise(freq = n())
new_stop <- merge(stop,a,all.x = TRUE)
new_stop <- new_stop[order(new_stop$location_id),]

# Check whether all unique locations have the same label or not
library(data.table)
df1 <- new_stop %>% select(user_id,uniq_loc,modality_purpose)
setDT(df1)[,ST2 := c("Match","Not Match")[(uniqueN(uniq_loc) == 1)+1],modality_purpose]
df1 #all of the same locations has the same label

# Add unique date per person per day
new_stop$date <- as.Date(new_stop$start_timestamp,"%Y%m%d")
b <- new_stop |>
  group_by(user_id,date) |>
  summarise(days = n())

# Calculate number of day participation
c <- b |>
  group_by(user_id) |>
  summarise(unique_day = n())

# Merge number of participation with the data
new_stop <- merge(new_stop,c,all.x = TRUE)
new_stop <- new_stop[order(new_stop$location_id),]

# Add order of visits of the same label to the data
d <- new_stop %>%
  group_by(user_id,modality_purpose) %>%
  mutate(visit = match(start_timestamp,unique(start_timestamp))) %>%
  summarise(visit,start_timestamp,modality_purpose)
new_stop <- merge(new_stop,d,all.x = TRUE)

# Add order of visits of the same location to the data
e <- new_stop %>%
  group_by(user_id,uniq_loc) %>%
  mutate(visit_loc = match(start_timestamp,unique(start_timestamp))) %>%
  summarise(visit_loc,start_timestamp,uniq_loc)
new_stop <- merge(new_stop,e,all.x = TRUE)
new_stop <- new_stop[order(new_stop$location_id),]

# Add weights of visits of same label and same location
new_stop$prop_visit <- new_stop$visit/new_stop$unique_day
new_stop$prop_visit_loc <- new_stop$visit_loc/new_stop$unique_day


### NEURAL NETWORK MODELS ###
## 1. Model without OSM data
# Packages
library(caret)
library(tidyverse)

# Reformatting the data
library(data.table)
stop_data_ori <- stop
stop <- new_stop
stop$lag_stop <- shift(stop$modality_purpose) #add lag for previous stop
stop$future_stop <- shift(stop$modality_purpose,-1) #add lag for next stop
stop$duration_mode[is.na(stop$duration_mode)] <- 0 #change NA of duration to 0
stop$distance_mode[is.na(stop$distance_mode)] <- 0 #change NA of distance to 0
stop$lag_stop[1] <- stop$lag_stop[2] #change the first lag with the second lag
stop$mode[1] <- stop$mode[2] #change the first mode with the second mode
stop$future_stop[5069] <- "home" #change the last stop with home

# Add modality and its duration and distance as new columns
ss <- stop %>%
  mutate(mode = ifelse(is.na(mode), lag_stop, mode),
         distance_mode = ifelse(is.na(distance_mode),0,distance_mode),
         duration_mode = ifelse(is.na(duration_mode),0,duration_mode))

# Drop levels of the data
data <- ss
data <- droplevels(data)

# Subset data for training and testing
data <- data %>% select(-c(user_id,uniq_loc,start_timestamp,end_timestamp,label,multi,id,check,location_id))
subset_data <- createDataPartition(y = data$modality_purpose, 
                                   p = .8, #proportion in train set
                                   list = FALSE)
train <- data[subset_data,]
test <- data[-subset_data,]

# Make some combinations for training control
set.seed(123)
grids <- expand.grid(size=seq(from=1,
                              to=5,
                              by=1),
                     decay=seq(from=0,
                               to=0.1,
                               by=0.01))
# size is the number of hidden layers
# decay is regularization parameter to avoid over fitting

# Set seeds for resampling process
set.seed(123)
seeds <- vector(mode = "list", length = 26) # length is nresampling*nrepeats+1
for (i in 1:25) seeds[[i]] <- sample.int(n = 20000, 2000) # 3 is the number of tuning parameter (mtry for rf or ncol-2
set.seed(123)
seeds [[26]] <- sample.int(20000,1)

# Set specification for training process
set.seed(123)
controls <- trainControl(method = "adaptive_LGOCV",#use adaptive leave-group-out cross-validation
                         repeats = 10, #number of folds iterations
                         seeds = seeds) #use seeds from the previous step

# Train model
set.seed(123)
ann_model <- train(form=modality_purpose~.,
                   data=train,
                   preProcess = "range", #normalization
                   method = "nnet", #method for neural network
                   trControl = controls, #resampling for train control
                   tuneGrid = grids, #search over the created grid
                   trace = FALSE) #supress output                     

ann_model
plot(ann_model)

# Make prediction for train and test data
ann_pred_train <- predict(object = ann_model, newdata = train) 
ann_pred_test <- predict(object = ann_model, newdata = test) 

# Create confusion matrix for train and test data
ann_conf_matrix_train <- confusionMatrix(ann_pred_train,as.factor(train$modality_purpose))
ann_conf_matrix_train <- confusionMatrix(data = ann_pred_train,
                                         reference = as.factor(train$modality_purpose), 
                                         positive = "Above",
                                         mode = "everything")
ann_conf_matrix_test <- confusionMatrix(data = ann_pred_test, 
                                        reference = as.factor(test$modality_purpose), 
                                        positive = "Above",
                                        mode = "everything")

# Compare the accuracy of train and test data
cbind(Training = ann_conf_matrix_train$overall,
      Testing = ann_conf_matrix_test$overall)
cbind(Training = ann_conf_matrix_train$byClass,
      Testing = ann_conf_matrix_test$byClass)

# Confusion matrix of testing
ann_conf_matrix_test$table

# Variable importance
varImp(ann_model)

## 2. Model with OSM data from 4 radii (treated as count data)
## The information from OSM was collected in Python
## The Python script is included, but none of the data was provided

# Add POIs from radiuses 25, 35, 50, and 200 meters (data is from analysis in Python)
stop200 <- read.csv("stop200.csv",sep=";")
stop50 <- read.csv("stop50.csv",sep=";")
stop35 <- read.csv("stop35.csv",sep=";")
stop25 <- read.csv("stop25.csv",sep=";")

# Combine all information
stop_poi_all <- cbind(stop25,stop35,stop50,stop200)
stop_poi_all <- stop_poi_all %>% select (-c(54,107,160))
data$location_id <- 1:nrow(data)
data_poi <- merge(data,stop_poi_all,no.dups = FALSE)

# Subset data for training and testing
subset_data_1 <- createDataPartition(y = data_poi$modality_purpose,  p = .8, list = FALSE)
train_1 <- data_poi[subset_data_1,]
test_1 <- data_poi[-subset_data_1,]


# Train model
set.seed(123)
ann_model_1 <- train(form=modality_purpose~.,
                     data=train_1,
                     preProcess = "scale", #normalization
                     method = "nnet", #method for neural network
                     trControl = controls, #resampling for training control
                     tuneGrid = grids, #search over the created grid
                     trace = FALSE) #supress output                     
ann_model_1
plot(ann_model_1)

# Make prediction for train and test data
ann_pred_train_1 <- predict(object = ann_model_1, newdata = train_1) 
ann_pred_test_1 <- predict(object = ann_model_1, newdata = test_1) 

# Create confusion matrix for train and test data
ann_conf_matrix_train_1 <- confusionMatrix(data = ann_pred_train_1,reference = as.factor(train_1$modality_purpose), positive = "Above", mode = "everything")
ann_conf_matrix_test_1 <- confusionMatrix(data = ann_pred_test_1,reference = as.factor(test_1$modality_purpose),positive = "Above", mode = "everything")

# Compare the accuracy of train and test data
cbind(Training = ann_conf_matrix_train_1$overall,
      Testing = ann_conf_matrix_test_1$overall)
cbind(Training = ann_conf_matrix_train_1$byClass,
      Testing = ann_conf_matrix_test_1$byClass)

# Confusion matrix of testing
ann_conf_matrix_test_1$table

# Variable importance
varImp(ann_model_1)

## 3. Model with OSM data from 1 radius (treated as count data)
# Select only one radius per tag
data_poi_1 <- data_poi %>% select(c(origin,modality_purpose,latitude,longitude,gender,age,hh_npersons,hh_type,hh_income,hh_income_standard,soc_eco,lease_car,fte_work,duration,distance,tod,dow,mode,duration_mode,distance_mode,freq,date,unique_day,visit,visit_loc,prop_visit,prop_visit_loc,lag_stop,future_stop,n_25_sports,n_200_routes,n_25_shops,n_50_swim,n_35_sport_centre,n_50_fitness,n_35_railway_land,n_35_retail_land,n_50_residential_land,n_200_industrial_land,n_200_edu_land,n_50_commercial_land,n_200_platform,n_25_bus_stop,n_35_lane,n_25_cycleway,n_50_sidewalk,n_50_path,n_50_footway,n_50_busway,n_35_pedestrian,n_35_tertiary,n_50_secondary,n_35_primary,n_25_trunk,n_25_motorway,n_50_office,n_25_residential,n_25_house,n_50_hotel,n_35_apartment,n_50_police,n_50_cinema,n_25_veterinary,n_25_pharmacy,n_25_hospital,n_50_doctors,n_25_dentist,n_25_bank,n_50_pois,n_25_parking,n_25_bus_st,n_50_university,n_35_restaurant,n_50_cafe,n_50_bar,n_25_aerodrome,n_25_college,n_25_nursing,n_25_pub,n_35_offices,n_25_schools))

# Subset data for training and testing
subset_data_2 <- createDataPartition(y = data_poi_1$modality_purpose,  p = .8, list = FALSE)
train_2 <- data_poi_1[subset_data_2,]
test_2 <- data_poi_1[-subset_data_2,]

# Train model
set.seed(123)
ann_model_2 <- train(form=modality_purpose~.,
                     data=train_2,
                     preProcess = "range", #normalization
                     method = "nnet", #method for neural network
                     trControl = controls, #resampling for train control
                     tuneGrid = grids, #search over the created grid
                     trace = FALSE) #supress output                     
ann_model_2
plot(ann_model_2)

# Make prediction for train and test data
ann_pred_train_2 <- predict(object = ann_model_2, newdata = train_2) 
ann_pred_test_2 <- predict(object = ann_model_2, newdata = test_2) 

# Create confusion matrix for train and test data
ann_conf_matrix_train_2 <- confusionMatrix(data = ann_pred_train_2,reference = as.factor(train_2$modality_purpose), positive = "Above", mode = "everything")
ann_conf_matrix_test_2 <- confusionMatrix(data = ann_pred_test_2,reference = as.factor(test_2$modality_purpose),positive = "Above", mode = "everything")

# Compare the accuracy of train and test data
cbind(Training = ann_conf_matrix_train_2$overall,
      Testing = ann_conf_matrix_test_2$overall)
cbind(Training = ann_conf_matrix_train_2$byClass,
      Testing = ann_conf_matrix_test_2$byClass)

# Confusion matrix
ann_conf_matrix_test_2$table

# Variable importance
varImp(ann_model_2)

## 4. Model with OSM data from 1 radius (treated as count data and category)
## Groups are defined based on the trip purpose labels
## 1. office: offices35, schools25, commercial_land50, industrial_land200, residential_land50, cycle_way25, footway50, tertiary35, secondary50, office50, parking25, hospital25
## 2. education: college25, education_land200, university50
## 3. recreational: pub25, restaurant35, cafe50
## 4. housing: residential_land50, lane35, cycle_way25, sidewalk50, path50, footway50, pedestrian35, tertiary35, secondary50, house25, apartment35
## 5. health: veterinary25, pharmacy25, hospital25, doctor50, dentist25
## 6. transit: bus_stop25, platform200, railway_land35, routes200, footway50, pedestrian35, busway50, secondary50, bus_st25
## 7. shopping: residential_land50, retail35, shops25, cycle_way25, footway50, pedestrian35, tertiary35, secondary50, parking25, restaurant35, cafe50
## 8. sport: fitness50, sport_centre35, sports25

# Grouping the tags 
# 1. Office
data_poi_1$office <- rowSums(data_poi_1 %>% select(c(n_35_offices,n_25_schools,n_50_commercial_land,n_200_industrial_land,n_50_residential_land,n_25_cycleway,n_50_footway,n_35_tertiary,n_50_secondary,n_50_office,n_25_parking,n_25_hospital)))
# 2. Education
data_poi_1$education <- rowSums(data_poi_1 %>% select(c(n_25_college,n_200_edu_land,n_50_university)))
# 3. Housing
data_poi_1$housing <- rowSums(data_poi_1 %>% select(c(n_50_residential_land,n_35_lane,n_25_cycleway,n_50_sidewalk,n_50_path,n_50_footway,n_35_pedestrian,n_35_tertiary,n_50_secondary,n_25_house,n_35_apartment)) )
# 4. Health
data_poi_1$health <- rowSums(data_poi_1 %>% select(c(n_25_veterinary,n_25_pharmacy,n_25_hospital,n_50_doctors,n_25_dentist)))
# 5. Transit
data_poi_1$transit <- rowSums(data_poi_1 %>% select(c(n_25_bus_stop,n_25_bus_st,n_200_platform,n_35_railway_land,n_200_routes,n_50_footway,n_35_pedestrian,n_50_busway,n_50_secondary)))
# 6. Shopping
data_poi_1$shopping <- rowSums(data_poi_1 %>% select(c(n_50_residential_land,n_35_retail_land,n_25_shops,n_25_cycleway,n_50_footway,n_35_pedestrian,n_35_tertiary,n_50_secondary,n_25_parking,n_35_restaurant,n_50_cafe)))
# 7. Recreational 
data_poi$recreational <- rowSums(data_poi_1 %>% select(c(n_25_pub, n_35_restaurant, n_50_cafe)))
# 8. Sport
data_poi_1$sport <- rowSums(data_poi_1 %>% select(c(n_50_fitness,n_35_sport_centre,n_25_sports)))

# Subset data into training and testing
subset_data_3 <- createDataPartition(y = data_poi_1$modality_purpose,  p = .8, list = FALSE)
train_3 <- data_poi_1[subset_data_3,]
test_3 <- data_poi_1[-subset_data_3,]

# Train model
set.seed(123)
ann_model_3 <- train(form=modality_purpose~.,
                     data=train_3,
                     preProcess = "range", #normalization
                     method = "nnet", #method for neural network
                     trControl = controls, #resampling from the train control
                     tuneGrid = grids, #search over the created grid
                     trace = FALSE) #supress output                     
ann_model_3
plot(ann_model_3)

# Make prediction for train and test data
ann_pred_train_3 <- predict(object = ann_model_3, newdata = train_3) 
ann_pred_test_3 <- predict(object = ann_model_3, newdata = test_3) 

# Create confusion matrix for train and test data
ann_conf_matrix_train_3 <- confusionMatrix(data = ann_pred_train_3,reference = as.factor(train_3$modality_purpose), positive = "Above", mode = "everything")
ann_conf_matrix_test_3 <- confusionMatrix(data = ann_pred_test_3,reference = as.factor(test_3$modality_purpose),positive = "Above", mode = "everything")

# Compare the accuracy of train and test data
cbind(Training = ann_conf_matrix_train_3$overall,
      Testing = ann_conf_matrix_test_3$overall)
cbind(Training = ann_conf_matrix_train_3$byClass,
      Testing = ann_conf_matrix_test_3$byClass)

# Confusion matrix
ann_conf_matrix_test_3$table

# Variable importance
varImp(ann_model_3)

## 5. Model with OSM data from 1 radius (treated as category)
# Delete count data from features list
data_poi_2 <- data_poi_1 %>% select(-c(n_25_sports:n_25_schools))

# Subset data for training and testing
subset_data_4 <- createDataPartition(y = data_poi_2$modality_purpose,  p = .8, list = FALSE)
train_4 <- data_poi_2[subset_data_4,]
test_4 <- data_poi_2[-subset_data_4,]

# Train model
set.seed(123)
ann_model_4 <- train(form=modality_purpose~.,
                     data=train_4,
                     preProcess = "range", #normalization
                     method = "nnet", #method  for neural network
                     trControl = controls, #resampling for train control
                     tuneGrid = grids, #search over the created grid
                     trace = FALSE) #supress output                     
ann_model_4
plot(ann_model_4)

# Make prediction for train and test data
ann_pred_train_4 <- predict(object = ann_model_4, newdata = train_4) 
ann_pred_test_4 <- predict(object = ann_model_4, newdata = test_4) 

# Create confusion matrix for train and test data
ann_conf_matrix_train_4 <- confusionMatrix(data = ann_pred_train_4,reference = as.factor(train_4$modality_purpose), positive = "Above", mode = "everything")
ann_conf_matrix_test_4 <- confusionMatrix(data = ann_pred_test_4,reference = as.factor(test_4$modality_purpose),positive = "Above", mode = "everything")

# Compare the accuracy of train and test data
cbind(Training = ann_conf_matrix_train_4$overall,
      Testing = ann_conf_matrix_test_4$overall)
cbind(Training = ann_conf_matrix_train_4$byClass,
      Testing = ann_conf_matrix_test_4$byClass)

# Confusion matrix
ann_conf_matrix_test_4$table

# Variable importance
varImp(ann_model_4)

## 6. Model with OSM data from 1 radius (treated as count data and percentage)
# Add POIs data
data_poi_1 <- data_poi %>% select(c(origin,modality_purpose,latitude,longitude,gender,age,hh_npersons,hh_type,hh_income,hh_income_standard,soc_eco,lease_car,fte_work,duration,distance,tod,dow,mode,duration_mode,distance_mode,freq,date,unique_day,visit,visit_loc,prop_visit,prop_visit_loc,lag_stop,future_stop,n_25_sports,n_200_routes,n_25_shops,n_50_swim,n_35_sport_centre,n_50_fitness,n_35_railway_land,n_35_retail_land,n_50_residential_land,n_200_industrial_land,n_200_edu_land,n_50_commercial_land,n_200_platform,n_25_bus_stop,n_35_lane,n_25_cycleway,n_50_sidewalk,n_50_path,n_50_footway,n_50_busway,n_35_pedestrian,n_35_tertiary,n_50_secondary,n_35_primary,n_25_trunk,n_25_motorway,n_50_office,n_25_residential,n_25_house,n_50_hotel,n_35_apartment,n_50_police,n_50_cinema,n_25_veterinary,n_25_pharmacy,n_25_hospital,n_50_doctors,n_25_dentist,n_25_bank,n_50_pois,n_25_parking,n_25_bus_st,n_50_university,n_35_restaurant,n_50_cafe,n_50_bar,n_25_aerodrome,n_25_college,n_25_nursing,n_25_pub,n_35_offices,n_25_schools))

# 1. Office
data_poi_1$office <- rowSums(data_poi_1 %>% select(c(n_35_offices,n_25_schools,n_50_commercial_land,n_200_industrial_land,n_50_residential_land,n_25_cycleway,n_50_footway,n_35_tertiary,n_50_secondary,n_50_office,n_25_parking,n_25_hospital)))
# 2. Education
data_poi_1$education <- rowSums(data_poi_1 %>% select(c(n_25_college,n_200_edu_land,n_50_university)))
# 3. Housing
data_poi_1$housing <- rowSums(data_poi_1 %>% select(c(n_50_residential_land,n_35_lane,n_25_cycleway,n_50_sidewalk,n_50_path,n_50_footway,n_35_pedestrian,n_35_tertiary,n_50_secondary,n_25_house,n_35_apartment)) )
# 4. Health
data_poi_1$health <- rowSums(data_poi_1 %>% select(c(n_25_veterinary,n_25_pharmacy,n_25_hospital,n_50_doctors,n_25_dentist)))
# 5. Transit
data_poi_1$transit <- rowSums(data_poi_1 %>% select(c(n_25_bus_stop,n_25_bus_st,n_200_platform,n_35_railway_land,n_200_routes,n_50_footway,n_35_pedestrian,n_50_busway,n_50_secondary)))
# 6. Shopping
data_poi_1$shopping <- rowSums(data_poi_1 %>% select(c(n_50_residential_land,n_35_retail_land,n_25_shops,n_25_cycleway,n_50_footway,n_35_pedestrian,n_35_tertiary,n_50_secondary,n_25_parking,n_35_restaurant,n_50_cafe)))
# 7. Recreational 
data_poi_1$recreational <- rowSums(data_poi_1 %>% select(c(n_25_pub, n_35_restaurant, n_50_cafe)))
# 8. Sport
data_poi_1$sport <- rowSums(data_poi_1 %>% select(c(n_50_fitness,n_35_sport_centre,n_25_sports)))

# Group percentage
data_poi_1$perc_office <- data_poi_1$office/data_poi_1$n_50_pois
data_poi_1$perc_education <- data_poi_1$education/data_poi_1$n_50_pois
data_poi_1$perc_recreational <- data_poi_1$recreational/data_poi_1$n_50_pois
data_poi_1$perc_housing <- data_poi_1$housing/data_poi_1$n_50_pois
data_poi_1$perc_health <- data_poi_1$health/data_poi_1$n_50_pois
data_poi_1$perc_transit <- data_poi_1$transit/data_poi_1$n_50_pois
data_poi_1$perc_shopping <- data_poi_1$shopping/data_poi_1$n_50_pois
data_poi_1$perc_sport <- data_poi_1$sport/data_poi_1$n_50_pois

## Impute NA and Inf with 0
data_poi_1$perc_office[which(!is.finite(data_poi_1$perc_office))] <- 0
data_poi_1$perc_education[which(!is.finite(data_poi_1$perc_education))] <- 0
data_poi_1$perc_recreational[which(!is.finite(data_poi_1$perc_recreational))] <- 0
data_poi_1$perc_housing[which(!is.finite(data_poi_1$perc_housing))] <- 0
data_poi_1$perc_health[which(!is.finite(data_poi_1$perc_health))] <- 0
data_poi_1$perc_transit[which(!is.finite(data_poi_1$perc_transit))] <- 0
data_poi_1$perc_shopping[which(!is.finite(data_poi_1$perc_shopping))] <- 0
data_poi_1$perc_sport[which(!is.finite(data_poi_1$perc_sport))] <- 0

# Subset data for training and testing
data_poi_3 <- data_poi_1 %>% select(-c(office:sport))
subset_data_5 <- createDataPartition(y = data_poi_3$modality_purpose,  p = .8, list = FALSE)
train_5 <- data_poi_3[subset_data_5,]
test_5 <- data_poi_3[-subset_data_5,]

# Train model
set.seed(123)
ann_model_5 <- train(form=modality_purpose~.,
                     data=train_5,
                     preProcess = "range", #normalization
                     method = "nnet", #method for neural network
                     trControl = controls, #resampling for train control
                     tuneGrid = grids, #search over the created grid
                     trace = FALSE) #supress output                     
ann_model_5
plot(ann_model_5)

# Make prediction for train and test data
ann_pred_train_5 <- predict(object = ann_model_5, newdata = train_5) 
ann_pred_test_5 <- predict(object = ann_model_5, newdata = test_5) 

# Create confusion matrix for train and test data
ann_conf_matrix_train_5 <- confusionMatrix(data = ann_pred_train_5,reference = as.factor(train_5$modality_purpose), positive = "Above", mode = "everything")
ann_conf_matrix_test_5 <- confusionMatrix(data = ann_pred_test_5,reference = as.factor(test_5$modality_purpose),positive = "Above", mode = "everything")

# Compare the accuracy of train and test data
cbind(Training = ann_conf_matrix_train_5$overall,
      Testing = ann_conf_matrix_test_5$overall)
cbind(Training = ann_conf_matrix_train_5$byClass,
      Testing = ann_conf_matrix_test_5$byClass)

# Confusion matrix
ann_conf_matrix_test_5$table

# Variable importance
varImp(ann_model_5)

## 7. Model with OSM data from 1 radius (treated as percentage)
# Subset data for training and testing
data_poi_6 <- data_poi_3 %>% select(-c(n_25_sports:n_25_schools))
subset_data_6 <- createDataPartition(y = data_poi_6$modality_purpose,  p = .8, list = FALSE)
train_6 <- data_poi_6[subset_data_6,]
test_6 <- data_poi_6[-subset_data_6,]

# Train model
set.seed(123)
ann_model_6 <- train(form=modality_purpose~.,
                     data=train_6,
                     preProcess = "range", #normalization
                     method = "nnet", #method for neural network
                     trControl = controls, #resampling for train control
                     tuneGrid = grids, #search over the created grid
                     trace = FALSE) #supress output                     
ann_model_6
plot(ann_model_6)

# Make prediction for train and test data
ann_pred_train_6 <- predict(object = ann_model_6, newdata = train_6) 
ann_pred_test_6 <- predict(object = ann_model_6, newdata = test_6) 

# Create confusion matrix for train and test data
ann_conf_matrix_train_6 <- confusionMatrix(data = ann_pred_train_6,reference = as.factor(train_6$modality_purpose), positive = "Above", mode = "everything")
ann_conf_matrix_test_6 <- confusionMatrix(data = ann_pred_test_6,reference = as.factor(test_6$modality_purpose),positive = "Above", mode = "everything")

# Compare the accuracy of train and test data
cbind(Training = ann_conf_matrix_train_6$overall,
      Testing = ann_conf_matrix_test_6$overall)
cbind(Training = ann_conf_matrix_train_6$byClass,
      Testing = ann_conf_matrix_test_6$byClass)

# Confusion matrix
ann_conf_matrix_test_6$table

# Variable importance
varImp(ann_model_6)

## 8. Best ANN model (ann_model_6) with weather data
## FG: daily mean wind speed -> information, bad weather stay at home, less overstap
## TG: daily mean temperature
## SQ: sunshine duration -> no difference  between indoor an outdoor activities
## RH: precipitation duration
## NG: hourly average cloud cover -> overstap related to sunshine
## UG: daily mean relative humidity -> humidity related to rain

# Add weather data (from De Bilt)
weather <- read.csv("weather.txt")

# Reformat date type (daily data)
weather$start_timestamp <- as.Date(as.character(weather$YYYYMMDD),"%Y%m%d")

# Add only from Schipol station
weather <- weather[weather$STN == 240,]

# Add five variables from weather data
weather <- weather %>% select(c(STN,start_timestamp, FG,TG,SQ,RH,NG,UG))

# Combine dataset with start time
start_time <- stop %>% select(c(start_timestamp,location_id))
start_time$start_timestamp <- as.Date(start_time$start_timestamp,"%Y%m%d")
data_poi_4 <- merge(data_poi,start_time,by="location_id")
data_poi_4 <- data_poi_4 %>% select(c(origin,modality_purpose,latitude,longitude,gender,age,hh_npersons,hh_type,hh_income,hh_income_standard,soc_eco,lease_car,fte_work,duration,distance,tod,dow,mode,duration_mode,distance_mode,freq,date,unique_day,visit,visit_loc,prop_visit,prop_visit_loc,lag_stop,future_stop,n_25_sports,n_200_routes,n_25_shops,n_50_swim,n_35_sport_centre,n_50_fitness,n_35_railway_land,n_35_retail_land,n_50_residential_land,n_200_industrial_land,n_200_edu_land,n_50_commercial_land,n_200_platform,n_25_bus_stop,n_35_lane,n_25_cycleway,n_50_sidewalk,n_50_path,n_50_footway,n_50_busway,n_35_pedestrian,n_35_tertiary,n_50_secondary,n_35_primary,n_25_trunk,n_25_motorway,n_50_office,n_25_residential,n_25_house,n_50_hotel,n_35_apartment,n_50_police,n_50_cinema,n_25_veterinary,n_25_pharmacy,n_25_hospital,n_50_doctors,n_25_dentist,n_25_bank,n_50_pois,n_25_parking,n_25_bus_st,n_50_university,n_35_restaurant,n_50_cafe,n_50_bar,n_25_aerodrome,n_25_college,n_25_nursing,n_25_pub,n_35_offices,n_25_schools,start_timestamp))

# Combine dataset with weather data
data_poi_5 <- merge(data_poi_4,weather,by = "start_timestamp") 

# Subset data for training and testing
data_poi_7 <- data_poi_5 %>% select(-c(STN,start_timestamp))
subset_data_7 <- createDataPartition(y = data_poi_7$modality_purpose,  p = .8, list = FALSE)
train_7 <- data_poi_7[subset_data_7,]
test_7 <- data_poi_7[-subset_data_7,]

# Train model
set.seed(123)
ann_model_7 <- train(form=modality_purpose~.,
                     data=train_7,
                     preProcess = "range", #normalization
                     method = "nnet", #method for neural network
                     trControl = controls, #resampling for train control
                     tuneGrid = grids, #search over the created grid
                     trace = FALSE) #supress output                     
ann_model_7
plot(ann_model_7)

# Make prediction for train and test data
ann_pred_train_7 <- predict(object = ann_model_7, newdata = train_7) 
ann_pred_test_7 <- predict(object = ann_model_7, newdata = test_7) 

# Create confusion matrix for train and test data
ann_conf_matrix_train_7 <- confusionMatrix(data = ann_pred_train_7,reference = as.factor(train_7$modality_purpose), positive = "Above", mode = "everything")
ann_conf_matrix_test_7 <- confusionMatrix(data = ann_pred_test_7,reference = as.factor(test_7$modality_purpose),positive = "Above", mode = "everything")

# Compare the accuracy of train and test data
cbind(Training = ann_conf_matrix_train_7$overall,
      Testing = ann_conf_matrix_test_7$overall)
cbind(Training = ann_conf_matrix_train_7$byClass,
      Testing = ann_conf_matrix_test_7$byClass)

# Confusion matrix
ann_conf_matrix_test_7$table

# Variable importance
varImp(ann_model_7)


### WEATHER DATA PLOT ###
## The weather data did not improve the model's accuracy (as you can see the plots below), therefore, for model comparison, ann_model_6 was used as the base model
# Daily mean wind speed
ggplot(data_poi_7) +
  geom_boxplot(mapping = aes(x=modality_purpose,
                             y=FG))+
  labs(y="Daily mean wind speed (in 0.1 m/s)", x="Trip Purpose") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.title.y = element_text(vjust = 0.5,
                                    hjust = 1,
                                    size = 8))

# Sunshine duration
ggplot(data_poi_7) +
  geom_boxplot(mapping = aes(x=modality_purpose,
                             y=SQ)) +
  labs(y="Sunshine duration (in 0.1 hour)", x="Trip Purpose") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.title.y = element_text(vjust = 0.5,
                                    hjust = 1,
                                    size = 8))

# Daily precipitation amount
ggplot(data_poi_7) +
  geom_boxplot(mapping = aes(x=modality_purpose,
                             y=RH)) +
  labs(y="Daily precipitation amount (in 0.1 mm)", x="Trip Purpose") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.title.y = element_text(vjust = 0.5,
                                    hjust = 1,
                                    size = 8))

# Daily mean temperature
ggplot(data_poi_7) +
  geom_boxplot(mapping = aes(x=modality_purpose,
                             y=TG))+
  labs(y="Daily mean temperature (in 0.1 degress Celsius)", x="Trip Purpose") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.title.y = element_text(vjust = 0.5,
                                    hjust = 1,
                                    size = 8))

# Mean daily cloud cover
ggplot(data_poi_7) +
  geom_boxplot(mapping = aes(x=modality_purpose,
                             y=NG))+
  labs(y="Mean daily cloud cover (in octans)", x="Trip Purpose")+
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.title.y = element_text(vjust = 0.5,
                                    hjust = 1,
                                    size = 8))

# Daily mean humidity
ggplot(data_poi_7) +
  geom_boxplot(mapping = aes(x=modality_purpose,
                             y=UG))+
  labs(y="Daily mean relative atmospheric humidity (in percents)", x="Trip Purpose") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1),
        axis.title.y = element_text(vjust = 0.5,
                                    hjust = 1,
                                    size = 8))


### MODEL COMPARISON ###
## Random Forest
library(randomForest)

# Train model
set.seed(123)
rf_model <- train(form=modality_purpose~.,
                  data=train_6,
                  preProcess = "range", #normalization
                  method = "rf", #method for random forest
                  trControl = controls, #resampling for train control
                  tuneGrid = expand.grid(.mtry = 1:20), # search over the created grid
                  importance = TRUE,
                  trace = FALSE) #supress output                     
rf_model
plot(rf_model)

# Make prediction for train and test data
rf_pred_train <- predict(object = rf_model, newdata = train_6) 
rf_pred_test <- predict(object = rf_model, newdata = test_6) 

# Create confusion matrix for train and test data
#ann_conf_matrix_train_1 <- confusionMatrix(ann_pred_train_1,as.factor(train_1$modality_purpose))
rf_conf_matrix_train <- confusionMatrix(data = rf_pred_train,reference = as.factor(train_6$modality_purpose), positive = "Above", mode = "everything")
rf_conf_matrix_test <- confusionMatrix(data = rf_pred_test,reference = as.factor(test_6$modality_purpose),positive = "Above", mode = "everything")

# Compare the accuracy of train and test data
cbind(Training = rf_conf_matrix_train$overall,
      Testing = rf_conf_matrix_test$overall)
cbind(Training = rf_conf_matrix_train$byClass,
      Testing = rf_conf_matrix_test$byClass)

# Confusion matrix
rf_conf_matrix_test$table

# Variable importance
varImp(rf_model)

## Extreme Gradient Boosting
library(xgboost)
library(plyr)

# Set grid for extreme gradient boosting
grid_xgb <- expand.grid(
  nrounds=seq(from=1, to=20, by=1),
  max_depth=6,
  eta = 0.3,
  gamma = 0,
  subsample = 1,
  colsample_bytree = 1,
  rate_drop = 0,
  skip_drop = 0,
  min_child_weight = 1)

# Train model
set.seed(123)
xgb_model <- train(form=modality_purpose~.,
                   data=train_6,
                   preProcess = "range", #normalization
                   method = "xgbDART", #method for xgb
                   trControl = controls, #resampling for train control
                   tuneGrid = grid_xgb, #search over specifiec grid
                   importance = TRUE,
                   trace = FALSE) #supress output                     
xgb_model
plot(xgb_model)

# Make prediction for train and test data
xgb_pred_train <- predict(object = xgb_model, newdata = train_6) 
xgb_pred_test <- predict(object = xgb_model, newdata = test_6) 

# Create confusion matrix for train and test data
xgb_conf_matrix_train <- confusionMatrix(data = xgb_pred_train,reference = as.factor(train_6$modality_purpose), positive = "Above", mode = "everything")
xgb_conf_matrix_test <- confusionMatrix(data = xgb_pred_test,reference = as.factor(test_6$modality_purpose),positive = "Above", mode = "everything")

# Compare the accuracy of train and test data
cbind(Training = xgb_conf_matrix_train$overall,
      Testing = xgb_conf_matrix_test$overall)
cbind(Training = xgb_conf_matrix_train$byClass,
      Testing = xgb_conf_matrix_test$byClass)

# Confusion matrix
xgb_conf_matrix_test$table

# Variable importance
varImp(xgb_model)

## Support Vector Machine
## SVM model does not have variable importance
library(e1071)

# Train model
set.seed(123)

svm_model <- train(form=modality_purpose~.,
                   data=train_6,
                   preProcess = "range", #normalization
                   method = "svmLinear2", #method for svm
                   trControl = controls, #resampling for train control
                   tuneGrid = expand.grid(.cost = 2:10), # search over the created grid
                   importance = TRUE,
                   trace = FALSE) #supress output                     
svm_model
plot(svm_model)

# Make prediction for train and test data
svm_pred_train <- predict(object = svm_model, newdata = train_6) 
svm_pred_test <- predict(object = svm_model, newdata = test_6) 

# Create confusion matrix for train and test data
svm_conf_matrix_train <- confusionMatrix(data = svm_pred_train,reference = as.factor(train_6$modality_purpose), positive = "Above", mode = "everything")
svm_conf_matrix_test <- confusionMatrix(data = svm_pred_test,reference = as.factor(test_6$modality_purpose),positive = "Above", mode = "everything")

# Compare the accuracy of train and test data
cbind(Training = svm_conf_matrix_train$overall,
      Testing = svm_conf_matrix_test$overall)
cbind(Training = svm_conf_matrix_train$byClass,
      Testing = svm_conf_matrix_test$byClass)

# Confusion matrix
svm_conf_matrix_test$table

## Naive Bayes
library(naivebayes)

# Set grid for Naive Bayes model
grid_nb <- expand.grid(
  laplace=seq(from=0.01, to=0.1, by=0.01),
  usekernel=TRUE,
  adjust = seq(from=0.5, to=1, by=0.1))

# Train model
set.seed(123)
nb_model <- train(form=modality_purpose~.,
                  data=train_6,
                  preProcess = "range", #normalization
                  method = "naive_bayes", #method for naive bayes
                  trControl = controls, #resampling for train control
                  importance = TRUE,
                  tuneGrid = grid_nb, #search over the created grid
                  trace = FALSE) #supress output                     
nb_model
plot(nb_model)

# Make prediction for train and test data
nb_pred_train <- predict(object = nb_model, newdata = train_6) 
nb_pred_test <- predict(object = nb_model, newdata = test_6) 

# Create confusion matrix for train and test data
nb_conf_matrix_train <- confusionMatrix(data = nb_pred_train,reference = as.factor(train_6$modality_purpose), positive = "Above", mode = "everything")
nb_conf_matrix_test <- confusionMatrix(data = nb_pred_test,reference = as.factor(test_6$modality_purpose),positive = "Above", mode = "everything")

# Compare the accuracy of train and test data
cbind(Training = nb_conf_matrix_train$overall,
      Testing = nb_conf_matrix_test$overall)
cbind(Training = nb_conf_matrix_train$byClass,
      Testing = nb_conf_matrix_test$byClass)

# Confusion matrix
nb_conf_matrix_test$table
