# Data Importing and Exploration ------------------------------------------

data = read.csv('/Users/ziyunli/Desktop/Applied Analytics/APAN 5200/Kaggle/rentlala2021/analysisData.csv', stringsAsFactors = T)
str(data)

## Check the percentage of missing values in columns (examples only)
nrow(is.na(data$host_total_listings_count))/nrow(data)
nrow(data[data$host_response_rate=='N/A',])/nrow(data)
nrow(data[data$host_name == "",])/nrow(data)

## Check the class, levels of variables (examples only)
class(data$host_total_listings_count); table(data$host_total_listings_count); levels(data$host_total_listings_count)

## Check correlation between potential IVs and DV (examples only)
cor(data$review_scores_rating, data$price)


# Data Preparation (analysis data)--------------------------------------------------------

### [host_response_rate -> host_response_rate2; host_acceptance_rate -> host_acceptance_rate2]; Lump categories of "" and N/A with 0%; change class to numeric
library(forcats)
data$host_response_rate1 = data$host_response_rate
data$host_response_rate1 = fct_collapse(data$host_response_rate1, "0%" = c("", "N/A"))

library(readr); library(dplyr)
data$host_response_rate2 = parse_number(as.character(data$host_response_rate1))

data$host_acceptance_rate1 = data$host_acceptance_rate
data$host_acceptance_rate1 = fct_collapse(data$host_acceptance_rate1, "0%" = c("", "N/A"))
data$host_acceptance_rate2 = parse_number(as.character(data$host_acceptance_rate1))

### [host_response_time -> host_response_time1]; Lump categories of "" and N/A into N/A; make sure class is factor
data$host_response_time1 = data$host_response_time
data$host_response_time1 = fct_collapse(data$host_response_time1, "N/A" = "")

### [host_total_listings_count; beds; security_deposit; cleaning_fee; reviews_per_month]; Change NA in numeric to 0, ensure numeric 
data$host_total_listings_count[is.na(data$host_total_listings_count)] <- 0
data$beds[is.na(data$beds)] <- 0
data$security_deposit[is.na(data$security_deposit)] <- 0
data$cleaning_fee[is.na(data$cleaning_fee)] <- 0
data$reviews_per_month[is.na(data$reviews_per_month)] <- 0

### [host_since; first_review; last_review]; Dates parsed into dates from factor class (factor - character - date); Change NA in category to 'N/A'
library(lubridate)
data$host_since_year = as.factor(year(ymd(as.character(data$host_since))))
data$host_since_year[is.na(data$host_since_year)] <- '2014'
levels(data$host_since_year) = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

data$first_review_year = as.factor(year(ymd(as.character(data$first_review))))
data$first_review_year[is.na(data$first_review_year)] <- '2014'
levels(data$first_review_year) = c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

data$last_review_year = as.factor(year(ymd(as.character(data$last_review))))
data$last_review_year[is.na(data$last_review_year)] <- '2015'
levels(data$last_review_year) = c('2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')


# Data Preparation (scoring data) -----------------------------------------
scoringData = read.csv('/Users/ziyunli/Desktop/Applied Analytics/APAN 5200/Kaggle/rentlala2021/scoringData.csv')

### [host_response_rate -> host_response_rate2; host_acceptance_rate -> host_acceptance_rate2]; Lump categories of "" and N/A with 0%; change class to numeric
scoringData$host_response_rate1 = scoringData$host_response_rate
scoringData$host_response_rate1 = fct_collapse(scoringData$host_response_rate1, "0%" = c("", "N/A"))
scoringData$host_response_rate2 = parse_number(as.character(scoringData$host_response_rate1))

scoringData$host_acceptance_rate1 = scoringData$host_acceptance_rate
scoringData$host_acceptance_rate1 = fct_collapse(scoringData$host_acceptance_rate1, "0%" = c("", "N/A"))
scoringData$host_acceptance_rate2 = parse_number(as.character(scoringData$host_acceptance_rate1))

### [host_response_time -> host_response_time1]; Lump categories of "" and N/A into N/A; make sure class is factor
scoringData$host_response_time1 = scoringData$host_response_time
scoringData$host_response_time1 = fct_collapse(scoringData$host_response_time1, "N/A" = "")

### [host_total_listings_count; beds; security_deposit; cleaning_fee; reviews_per_month]; Change NA in numeric to 0, ensure numeric 
scoringData$host_total_listings_count[is.na(scoringData$host_total_listings_count)] <- 0
scoringData$beds[is.na(scoringData$beds)] <- 0
scoringData$security_deposit[is.na(scoringData$security_deposit)] <- 0
scoringData$cleaning_fee[is.na(scoringData$cleaning_fee)] <- 0
scoringData$reviews_per_month[is.na(scoringData$reviews_per_month)] <- 0

### [host_since; first_review; last_review]; Dates parsed into dates from factor class (factor - character - date); Change NA in category to 'N/A'
scoringData$host_since_year = as.factor(year(ymd(as.character(scoringData$host_since))))
scoringData$host_since_year[is.na(scoringData$host_since_year)] <- '2014'
levels(scoringData$host_since_year) = c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

scoringData$first_review_year = as.factor(year(ymd(as.character(scoringData$first_review))))
scoringData$first_review_year[is.na(scoringData$first_review_year)] <- '2014'
levels(scoringData$first_review_year) = c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

scoringData$last_review_year = as.factor(year(ymd(as.character(scoringData$last_review))))
scoringData$last_review_year[is.na(scoringData$last_review_year)] <- '2015'
levels(scoringData$last_review_year) = c('2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')


# Other Data Preparation (level adjustments in both analysis and scoring datasets)--------------------------------------------------

### [property_type, neighbourhood_cleansed]; Create "other" level in both datasets
library(dplyr)
df1 = intersect(fct_lump_min(f = scoringData$property_type, min = 5)%>%table()%>%data.frame()%>%select("."), 
               fct_lump_min(f = data$property_type, min = 5)%>%table()%>%data.frame()%>%select(".")) 
data$property_type = fct_other(data$property_type, keep =df1[,1])

df = intersect(fct_lump_min(f = scoringData$neighbourhood_cleansed, min = 10)%>%table()%>%data.frame()%>%select("."), 
               fct_lump_min(f = data$neighbourhood_cleansed, min = 10)%>%table()%>%data.frame()%>%select(".")) 
data$neighbourhood_cleansed = fct_other(data$neighbourhood_cleansed, keep =df[,1])

scoringData$property_type = fct_other(scoringData$property_type, keep = df1[,1])
scoringData$neighbourhood_cleansed = fct_other(scoringData$neighbourhood_cleansed, keep = df[,1])

### [cancellation_policy, calendar_updated]; Combine levels in both datasets 
data$cancellation_policy = fct_collapse(data$cancellation_policy, 'strict' = c('strict', 'strict_14_with_grace_period', 'super_strict_30', 'super_strict_60'))
scoringData$cancellation_policy = fct_collapse(scoringData$cancellation_policy, 'strict' = c('strict', 'strict_14_with_grace_period', 'super_strict_30', 'super_strict_60'))

data$calendar_updated = fct_collapse(data$calendar_updated, 
                                     'within 1 week' = c('today', 'yesterday', 'a week ago', '1 week ago', '2 days ago', '3 days ago', '4 days ago', '5 days ago', '6 days ago'),
                                    '2-7 weeks ago' = c('2 weeks ago', '3 weeks ago', '4 weeks ago', '5 weeks ago', '6 weeks ago', '7 weeks ago'),
                                    '2-12 months ago' = c('2 months ago', '3 months ago', '4 months ago', '5 months ago','6 months ago', '7 months ago','8 months ago', '9 months ago', '10 months ago', '11 months ago', '12 months ago'),
                                    '13-24 months ago' = c('13 months ago', '14 months ago', '15 months ago','16 months ago', '17 months ago','18 months ago', '19 months ago', '20 months ago', '21 months ago', '22 months ago', '23 months ago', '24 months ago'),
                                   '25-36 months ago' = c('25 months ago', '26 months ago', '27 months ago', '28 months ago','29 months ago', '30 months ago','31 months ago', '32 months ago', '33 months ago', '34 months ago', '35 months ago', '36 months ago'),
                                   '37-48 months ago' = c('37 months ago','38 months ago', '39 months ago', '40 months ago', '41 months ago', '42 months ago', '43 months ago', '44 months ago', '45 months ago', '46 months ago', '47 months ago', '48 months ago'), 
                                    '49-60 months ago' = c('49 months ago', '50 months ago', '51 months ago', '52 months ago', '53 months ago', '54 months ago', '55 months ago', '56 months ago', '57 months ago', '58 months ago', '59 months ago', '60 months ago'), 
                                    'more than 60 months ago' = c('61 months ago', '62 months ago', '63 months ago', '64 months ago', '65 months ago', '66 months ago', '67 months ago', '68 months ago', '69 months ago', '70 months ago', '71 months ago', '72 months ago', '73 months ago', '76 months ago'), 'never' = 'never')

scoringData$calendar_updated = fct_collapse(scoringData$calendar_updated, 
                                            'within 1 week' = c('today', 'yesterday', 'a week ago', '1 week ago', '2 days ago', '3 days ago', '4 days ago', '5 days ago', '6 days ago'),
                                            '2-7 weeks ago' = c('2 weeks ago', '3 weeks ago', '4 weeks ago', '5 weeks ago', '6 weeks ago', '7 weeks ago'),
                                            '2-12 months ago' = c('2 months ago', '3 months ago', '4 months ago', '5 months ago','6 months ago', '7 months ago','8 months ago', '9 months ago', '10 months ago', '11 months ago', '12 months ago'),
                                            '13-24 months ago' = c('13 months ago', '14 months ago', '15 months ago','16 months ago', '17 months ago','18 months ago', '19 months ago', '20 months ago', '21 months ago', '22 months ago', '23 months ago', '24 months ago'),
                                            '25-36 months ago' = c('25 months ago', '26 months ago', '27 months ago', '28 months ago','29 months ago', '30 months ago','31 months ago', '32 months ago', '33 months ago', '34 months ago', '35 months ago', '36 months ago'),
                                            '37-48 months ago' = c('37 months ago','38 months ago', '39 months ago', '40 months ago', '41 months ago', '42 months ago', '43 months ago', '44 months ago', '45 months ago', '46 months ago', '47 months ago', '48 months ago'),
                                            '49-60 months ago'= c('49 months ago', '50 months ago', '51 months ago', '52 months ago', '53 months ago', '54 months ago', '55 months ago', '56 months ago', '57 months ago', '58 months ago', '59 months ago', '60 months ago'),
                                            'more than 60 months ago'= c('61 months ago', '63 months ago', '66 months ago', '67 months ago', '69 months ago', '73 months ago', '75 months ago'), 'never' = 'never')
### [host_since_year, host_response_time1, first_review_year, last_review_year, cancellation_policy]; Turn categorical variables into ordinal 
data$host_since_year = factor(data$host_since_year, order = TRUE, levels = c('2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020'))
data$host_response_time1 = factor(data$host_response_time1, order = TRUE, levels = c('within an hour', 'within a few hours', 'within a day', 'a few days or more', 'N/A'))
data$first_review_year = factor(data$first_review_year, order = TRUE, levels = c('2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020'))
data$last_review_year = factor(data$last_review_year, order = TRUE, levels = c('2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020'))
data$cancellation_policy = factor(data$cancellation_policy, order = TRUE, levels = c('flexible', 'moderate', 'strict'))

scoringData$host_since_year = factor(scoringData$host_since_year, order = TRUE, levels = c('2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020'))
scoringData$host_response_time1 = factor(scoringData$host_response_time1, order = TRUE, levels = c('within an hour', 'within a few hours', 'within a day', 'a few days or more', 'N/A'))
scoringData$first_review_year = factor(scoringData$first_review_year, order = TRUE, levels = c('2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020'))
scoringData$last_review_year = factor(scoringData$last_review_year, order = TRUE, levels = c('2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020'))
scoringData$cancellation_policy = factor(scoringData$cancellation_policy, order = TRUE, levels = c('flexible', 'moderate', 'strict'))

### [amenities, transit]; Extract useful keywords from text
#### [amenities] create new variables: Air conditioning, Wifi, Kitchen, Elevator, TV
library(stringr)
data$amenities_AC = as.factor(str_detect(data$amenities, 'Air conditioning'))
data$amenities_wifi = as.factor(str_detect(data$amenities, 'Wifi'))
data$amenities_kitchen = as.factor(str_detect(data$amenities, 'Kitchen'))
data$amenities_elevator = as.factor(str_detect(data$amenities, 'Elevator'))
data$amenities_tv = as.factor(str_detect(data$amenities, 'TV'))

scoringData$amenities_AC = as.factor(str_detect(scoringData$amenities, 'Air conditioning'))
scoringData$amenities_wifi = as.factor(str_detect(scoringData$amenities, 'Wifi'))
scoringData$amenities_kitchen = as.factor(str_detect(scoringData$amenities, 'Kitchen'))
scoringData$amenities_elevator = as.factor(str_detect(scoringData$amenities, 'Elevator'))
scoringData$amenities_tv = as.factor(str_detect(scoringData$amenities, 'TV'))

### [transit] create new variables: subway, train
data$transit_subway = as.factor(str_detect(data$transit, 'subway'))
scoringData$transit_subway = as.factor(str_detect(scoringData$transit, 'subway'))

data$transit_train = as.factor(str_detect(data$transit, 'train'))
scoringData$transit_train = as.factor(str_detect(scoringData$transit, 'train'))


# Data Transformation for xgboost (analysis data) -----------------------------------------

### Include all numeric factors, exclude DV price
dataNumeric = data %>%
  select(c(host_response_rate2, host_acceptance_rate2, host_total_listings_count, accommodates, 
           bathrooms, bedrooms, beds, security_deposit, cleaning_fee, guests_included, extra_people,
           minimum_nights, maximum_nights, minimum_nights_avg_ntm, maximum_nights_avg_ntm, availability_30,
           availability_60, availability_90, availability_365, number_of_reviews, number_of_reviews_ltm,
           review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin,
           review_scores_communication, review_scores_location, review_scores_value, calculated_host_listings_count,
           calculated_host_listings_count_entire_homes, calculated_host_listings_count_private_rooms,
           calculated_host_listings_count_shared_rooms, reviews_per_month))
#### Make sure df is all numeric
str(dataNumeric)

### Convert factor IVs to dummy variables
dataFactor = cbind(model.matrix(~host_since_year-1, data), 
  model.matrix(~host_response_time1-1, data),
  model.matrix(~host_is_superhost-1, data),
  model.matrix(~host_has_profile_pic-1, data),
  model.matrix(~host_identity_verified-1, data),
  model.matrix(~neighbourhood_cleansed-1, data),
  model.matrix(~neighbourhood_group_cleansed-1, data),
  model.matrix(~is_location_exact-1, data),
  model.matrix(~property_type-1, data),
  model.matrix(~room_type-1, data),
  model.matrix(~bed_type-1, data),
  model.matrix(~first_review_year-1, data),
  model.matrix(~last_review_year-1, data),
  model.matrix(~instant_bookable-1, data),
  model.matrix(~cancellation_policy-1, data),
  model.matrix(~require_guest_profile_picture-1, data),
  model.matrix(~require_guest_phone_verification-1, data),
  model.matrix(~calendar_updated-1, data),
  model.matrix(~amenities_AC-1, data),
  model.matrix(~amenities_wifi-1, data),
  model.matrix(~amenities_kitchen-1, data),
  model.matrix(~amenities_elevator-1, data),
  model.matrix(~amenities_tv-1, data),
  model.matrix(~transit_subway-1, data),
  model.matrix(~transit_train-1, data))

### Combine dataNumeric and dataFactor
data_input = cbind(dataNumeric, dataFactor)

### Transform data frame into matrix
data_input_matrix = data.matrix(data_input)

# Data Transformation for xgboost (scoring data) -----------------------------------------
### Include all numeric factors, exclude DV price
scoringDataNumeric = scoringData %>%
  select(c(host_response_rate2, host_acceptance_rate2, host_total_listings_count, accommodates, 
           bathrooms, bedrooms, beds, security_deposit, cleaning_fee, guests_included, extra_people,
           minimum_nights, maximum_nights, minimum_nights_avg_ntm, maximum_nights_avg_ntm, availability_30,
           availability_60, availability_90, availability_365, number_of_reviews, number_of_reviews_ltm,
           review_scores_rating, review_scores_accuracy, review_scores_cleanliness, review_scores_checkin,
           review_scores_communication, review_scores_location, review_scores_value, calculated_host_listings_count,
           calculated_host_listings_count_entire_homes, calculated_host_listings_count_private_rooms,
           calculated_host_listings_count_shared_rooms, reviews_per_month))
str(scoringDataNumeric)

### Convert factor IVs to dummy variables
scoringDataFactor = cbind(model.matrix(~host_since_year-1, scoringData), 
                   model.matrix(~host_response_time1-1, scoringData),
                   model.matrix(~host_is_superhost-1, scoringData),
                   model.matrix(~host_has_profile_pic-1, scoringData),
                   model.matrix(~host_identity_verified-1, scoringData),
                   model.matrix(~neighbourhood_cleansed-1, scoringData),
                   model.matrix(~neighbourhood_group_cleansed-1, scoringData),
                   model.matrix(~is_location_exact-1, scoringData),
                   model.matrix(~property_type-1, scoringData),
                   model.matrix(~room_type-1, scoringData),
                   model.matrix(~bed_type-1, scoringData),
                   model.matrix(~first_review_year-1, scoringData),
                   model.matrix(~last_review_year-1, scoringData),
                   model.matrix(~instant_bookable-1, scoringData),
                   model.matrix(~cancellation_policy-1, scoringData),
                   model.matrix(~require_guest_profile_picture-1, scoringData),
                   model.matrix(~require_guest_phone_verification-1, scoringData),
                   model.matrix(~calendar_updated-1, scoringData), 
                   model.matrix(~amenities_AC-1, scoringData),
                   model.matrix(~amenities_wifi-1, scoringData),
                   model.matrix(~amenities_kitchen-1, scoringData),
                   model.matrix(~amenities_elevator-1, scoringData),
                   model.matrix(~amenities_tv-1, scoringData),
                   model.matrix(~transit_subway-1, scoringData),
                   model.matrix(~transit_train-1, scoringData))
### Combine dataNumeric and dataFactor and transform dataframe into matrix
scoringData_input = cbind(scoringDataNumeric, scoringDataFactor)
scoringData_input_matrix = data.matrix(scoringData_input)


# Split and model training ---------------------------------
### Split analysis data into train and test
library(caret)
set.seed(67)
split = createDataPartition(y = data$price, p = 0.75, list = F, groups = 100)
train = data_input_matrix[split,]
test = data_input_matrix[-split,]
train_labels = data$price[split]
test_labels = data$price[-split]
mean(train_labels)
mean(test_labels)

### Convert the cleaned dataframe to a dmatrix, helps model train move more quickly
library(xgboost)
dtrain = xgb.DMatrix(data = train, label = train_labels)
dtest = xgb.DMatrix(data = test, label = test_labels)

### Train and tune model
model_train = xgboost(data = dtrain,
                nround = 800,
                early_stopping_rounds = 400,
                eta = 0.1,
                colsample_bylevel = 0.7)
model_train$best_iteration
plot(model_train$evaluation_log)
pred_train = predict(model_train, dtrain)
rmse_train = sqrt(mean((pred_train - train_labels)^2)); rmse_train
pred_test = predict(model_train, dtest)
rmse_test = sqrt(mean((pred_test - test_labels)^2)); rmse_test


# Model run with scoring data and submission ------------------------------
ddata = xgb.DMatrix(data = data_input_matrix, label = data$price)
model = xgboost(data = ddata,
                nround = 800,
                early_stopping_rounds = 400,
                eta = 0.1,
                colsample_bylevel = 0.7)
dScoringData = xgb.DMatrix(data = scoringData_input_matrix)
pred_scoringData = predict(model, scoringData_input_matrix)
plot(model$evaluation_log)
model$best_iteration

### Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred_scoringData)
write.csv(submissionFile, 'submission.csv',row.names = F)
