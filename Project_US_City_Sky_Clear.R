## ----Load useful libraries,echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")


## ----Default not showing type in output,echo=FALSE----------------------------------------------------------------------------------------
#Not showing Code, Alert and message in the document output
opts_chunk$set(eval = TRUE, echo = FALSE, warning=FALSE, message=FALSE)


## ----Choose the City to study, eval=TRUE, echo=TRUE---------------------------------------------------------------------------------------
#Define City_Studied as New.York
City_Studied <- "New.York"


## ----Getting Data from the .CSV in the folder historical-hourly-weather-data saved on your workspace--------------------------------------
City<- read.csv("~/city_attributes.csv")
#save(City, file = "~/City.Rda")

Humidity<- read.csv("~/humidity.csv")
#save(Humidity, file = "~/Humidity.Rda")

Pressure<- read.csv("~/pressure.csv")
#save(Pressure, file = "~/Pressure.Rda")

Temperature<- read.csv("~/temperature.csv")
#save(Temperature, file = "~/Temperature.Rda")

Weather_Description<- read.csv("~/weather_description.csv")
#save(Weather_Description, file = "~/Weather.Rda")

Wind_Direction<- read.csv("~/wind_direction.csv")
#save(Wind_Direction, file = "~/Wind_Direction.Rda")

Wind_Speed<- read.csv("~/wind_speed.csv")
#save(Wind_Speed, file = "~/Wind_Speed.Rda")


## ----looking into the city_attibutes------------------------------------------------------------------------------------------------------
kable(City, caption= " Head of city_attributes file")


## ----looking into the humidity------------------------------------------------------------------------------------------------------------
kable(head(Humidity[1:3,]) %>% select (1:7), caption= "Head of humidity file")


## ----looking into the pressure------------------------------------------------------------------------------------------------------------
kable(head(Pressure[1:3,]) %>% select (1:7), caption= "Head of pressure file")


## ----looking into the temperature---------------------------------------------------------------------------------------------------------
kable(head(Temperature[1:3,]) %>% select (1:7), caption= "Head of temperature file")


## ----looking into the weather_description-------------------------------------------------------------------------------------------------
kable(head(Weather_Description[1:3,]) %>% select (1:6), caption= "Head of weather_description file")


## ----looking into the wind_direction------------------------------------------------------------------------------------------------------
kable(head(Wind_Direction[1:3,]) %>% select (1:7), caption= "Head of wind_direction file")


## ----looking into the wind_speed----------------------------------------------------------------------------------------------------------
kable(head(Wind_Speed[1:3,]) %>% select (1:7), caption= "Head of wind_speed file")


## ----Only Keeps daytime and City_Studied in each 6 file-----------------------------------------------------------------------------------
Humidity <- Humidity %>% select(datetime, City_Studied)
Pressure <- Pressure %>% select(datetime, City_Studied)
Temperature <- Temperature %>% select(datetime, City_Studied)
Weather_Description <- Weather_Description %>% select(datetime, City_Studied)
Wind_Direction <- Wind_Direction %>% select(datetime, City_Studied)
Wind_Speed <- Wind_Speed %>% select(datetime, City_Studied)


## ----Set the colnames to datetime and the variable----------------------------------------------------------------------------------------
colnames(Humidity) <- c("datetime", "humidity")
colnames(Pressure) <- c("datetime", "pressure")
colnames(Temperature) <- c("datetime", "temperature")
colnames(Weather_Description) <- c("datetime", "weather_des")
colnames(Wind_Direction) <- c("datetime", "wind_dir")
colnames(Wind_Speed) <- c("datetime", "wind_speed")


## ----Create our workable set of data: dataset---------------------------------------------------------------------------------------------
Dataset <- Weather_Description %>% left_join(Pressure, by="datetime")%>% left_join(Temperature, by="datetime")%>% left_join(Humidity, by="datetime")%>% left_join(Wind_Direction, by="datetime")%>% left_join(Wind_Speed, by="datetime")
kable(head(Dataset), caption= "Dataset Temp")


## ----Check class of each columns----------------------------------------------------------------------------------------------------------
kable(sapply(Dataset, class), col.names= "Class", caption = "Columns")


## ----Replace NA by previous value---------------------------------------------------------------------------------------------------------
Dataset$humidity <- ave(Dataset$humidity, cumsum(!is.na(Dataset$humidity)), FUN=function(x) x[1])
Dataset$pressure <- ave(Dataset$pressure, cumsum(!is.na(Dataset$pressure)), FUN=function(x) x[1])
Dataset$temperature <- ave(Dataset$temperature, cumsum(!is.na(Dataset$temperature)), FUN=function(x) x[1])
Dataset$wind_dir <- ave(Dataset$wind_dir, cumsum(!is.na(Dataset$wind_dir)), FUN=function(x) x[1])
Dataset$wind_speed <- ave(Dataset$wind_speed, cumsum(!is.na(Dataset$wind_speed)), FUN=function(x) x[1])


## ----Change format of datetime------------------------------------------------------------------------------------------------------------
Dataset$datetime <- ymd_hms(Dataset$datetime)


## ----Create Month and Hour----------------------------------------------------------------------------------------------------------------
Dataset <-Dataset %>% mutate(Month= month(datetime),Hour=hour(datetime))


## ----Remove 1st line and change factor weather_des with 0 & 1-----------------------------------------------------------------------------
Dataset <- Dataset%>% mutate(sky_clear=as.factor(ifelse(weather_des == "sky is clear", 1, 0)))
Dataset <- Dataset[-1,-2]


## ----Add the real observed sky_clear in 24H  and the datetime in 24Hto each entries-------------------------------------------------------
Dim_Dataset <- dim(Dataset)[1]
to_add_datetime <- Dataset[25:Dim_Dataset,] %>% select(sky_clear)
colnames(to_add_datetime) <- c("Pred_24H")
Dataset <-Dataset[1:(Dim_Dataset-24),]
Dataset <- bind_cols(Dataset, to_add_datetime)


## ----rounding temparature to the nearest--------------------------------------------------------------------------------------------------
Dataset$temperature <- round(Dataset$temperature,0)


## ----Show Head of our Final Dataset-------------------------------------------------------------------------------------------------------
kable(head(Dataset[-1]), caption= "Dataset (Not showing 1st Columns: datetime)")


## ----Remove useless and save our final Dataset, echo= FALSE-------------------------------------------------------------------------------
#Let's make some cleanning and delete data and file not usefull
rm(City, Humidity, Pressure, Temperature, Weather_Description, Wind_Direction, Wind_Speed, Pred_24H, to_add_datetime)
#if you had save each file in .Rda, now we will delete them
#file.remove("~/City.Rda","~/Humidity.Rda","~/Temperature.Rda","~/Weather.Rda","~/Wind_Direction.Rda","~/Pressure.Rda", "~/Wind_Speed.Rda")
save(Dataset, file = "~/Dataset.Rda")


## ----Some important figures---------------------------------------------------------------------------------------------------------------
print(paste0("We have: ",Dim_Dataset-24," records (datetime), for the city: ",City_Studied))


## ----Probability to get Pred_24H 1--------------------------------------------------------------------------------------------------------
kable(mean(Dataset$Pred_24H == 1), col.names= "Probability", caption = "Probability to get a clear sky in 24H")


## ----Probability to get Pred_24H 1 and sky_clear 1----------------------------------------------------------------------------------------
Now_clear <- Dataset %>% filter(sky_clear == 1)
kable(mean(Now_clear$Pred_24H ==1), col.names= "Probability",caption = "Probability to get a clear sky in 24H as it is now")
rm(Now_clear)


## ----Graph by Month and Hour, fig.height=3,fig.width=3.3----------------------------------------------------------------------------------
# By Month graph, with Months_Year define for labels
Month_Year<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
Dataset%>% group_by(Month) %>% filter(Pred_24H == 1) %>% ggplot(aes(Month)) + geom_histogram(bins=90,fill="darkgoldenrod3") + ggtitle("Monthly Seasonality") + theme_bw() + scale_x_continuous(breaks=seq(1,12,1), labels=Month_Year)+theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10))

# By Hour graph
Dataset%>% group_by(Hour) %>% filter(Pred_24H == 1) %>% ggplot(aes(Hour)) + geom_histogram(bins=90,fill="darkgoldenrod3") + ggtitle("Hourly Seasonality") + theme_bw()


## ----Graph of probability by Month and Hour, fig.height=3,fig.width=3.3-------------------------------------------------------------------
# By Monthly Probability graph, with Months_Year define for labels
Dataset%>% group_by(Month) %>%  summarize(Proba = mean(Pred_24H==1)) %>%ggplot(aes(Month, Proba)) + geom_point(color="darkgoldenrod3") + ggtitle("Monthly Seasonality Probability") + theme_bw() + scale_x_continuous(breaks=seq(1,12,1), labels=Month_Year)+theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10))

# By Hourly Probability graph
Dataset%>% group_by(Hour) %>%  summarize(Proba = mean(Pred_24H==1)) %>% ggplot(aes(Hour, Proba)) + geom_point(color="darkgoldenrod3") + ggtitle("Hourly Seasonality Probability") + theme_bw()


## ----Graph for temperature,fig.height=3,fig.width=3.3-------------------------------------------------------------------------------------
# Distribution by temperature
Dataset%>% group_by(temperature) %>% filter(Pred_24H == 1) %>% ggplot(aes(temperature)) + geom_histogram(bins=90,fill="darkgoldenrod3") + ggtitle("Clear sky by temperature") + theme_bw()

# By temperature Probability graph
Dataset%>% group_by(temperature) %>%  summarize(Proba = mean(Pred_24H==1)) %>% ggplot(aes(temperature, Proba)) + geom_point(color="darkgoldenrod3") + ggtitle("Probability by temperature") + theme_bw()


## ----Graph distribution and probability for humidity ,fig.height=3,fig.width=3.3----------------------------------------------------------
# Distribution by humidity
Dataset%>% group_by(humidity) %>% filter(Pred_24H == 1) %>% ggplot(aes(humidity)) + geom_histogram(bins=90,fill="darkgoldenrod3") + ggtitle("Clear sky by humidity") + theme_bw()

# By humidity Probability graph
Dataset%>% group_by(humidity) %>%  summarize(Proba = mean(Pred_24H==1)) %>% ggplot(aes(humidity, Proba)) + geom_point(color="darkgoldenrod3") + ggtitle("Probability by humidity") + theme_bw()


## ----Graph distribution and probability for pressure, fig.height=3,fig.width=3.3----------------------------------------------------------
# Distribution by presssure
Dataset%>% group_by(pressure) %>% filter(Pred_24H == 1) %>% ggplot(aes(pressure)) + geom_histogram(bins=90,fill="darkgoldenrod3") + ggtitle("Clear sky by pressure") + theme_bw()

# By pressure Probability graph
Dataset%>% group_by(pressure) %>%  summarize(Proba = mean(Pred_24H==1)) %>% ggplot(aes(pressure, Proba)) + geom_point(color="darkgoldenrod3") + ggtitle("Probability by pressure") + theme_bw()


## ----Graph of distibution and probability for wind direction,fig.height=3,fig.width=3.3---------------------------------------------------
# Distribution by wind direction
Dataset%>% group_by(wind_dir) %>% filter(Pred_24H == 1) %>% ggplot(aes(wind_dir)) + geom_histogram(bins=90,fill="darkgoldenrod3") + ggtitle("Clear sky by wind direction") + theme_bw()

# By wind direction Probability graph
Dataset%>% group_by(wind_dir) %>%  summarize(Proba = mean(Pred_24H==1)) %>% ggplot(aes(wind_dir, Proba)) + geom_point(color="darkgoldenrod3") + ggtitle("Probability by wind direction") + theme_bw()


## ----Graph of distibution and probability for wind speed, fig.height=3,fig.width=3.3------------------------------------------------------
# Distribution by wind speed
Dataset%>% group_by(wind_speed) %>% filter(Pred_24H == 1) %>% ggplot(aes(wind_speed)) + geom_histogram(bins=90,fill="darkgoldenrod3") + ggtitle("Clear sky by wind speed") + theme_bw()

# By pressure Probability graph
Dataset%>% group_by(wind_speed) %>%  summarize(Proba = mean(Pred_24H==1)) %>% ggplot(aes(wind_speed, Proba)) + geom_point(color="darkgoldenrod3") + ggtitle("Probability by wind speed") + theme_bw()


## ----Create our train and test sets-------------------------------------------------------------------------------------------------------
#Using function that split Dataset, here in 80% for Train set  and 20% for Test set, we use set.seed in order to get always the same partition.
set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(y = Dataset$temperature, times = 1,p = 0.2, list = FALSE)
train_temp <- Dataset[-test_index,]
test_temp <- Dataset[test_index,]
  
#Remove in test_set set the entries where variable are not present in train_set.
test_and_addTrain <- test_temp 
test_set  <- test_temp  %>% semi_join(train_temp, by = "pressure") %>% semi_join(train_temp, by = "temperature")%>% semi_join(train_temp, by = "humidity")%>% semi_join(train_temp, by = "wind_dir")%>% semi_join(train_temp, by = "wind_speed") %>% semi_join(train_temp, by = "Month") %>% semi_join(train_temp, by = "Hour")

#To keep Dataset set globally unchanged, we need to add the rating we just removed from the Test set.
addTrain<-anti_join(test_and_addTrain, test_set)
train_set<-rbind(train_temp ,addTrain)

#Delete useless Dataset
rm(test_index, train_temp, test_temp, test_and_addTrain, addTrain)

#Numbers of entries of the test_set and train_set
nb_train <- dim(train_set)[1]
nb_test <- dim(test_set)[1]


## ----Model: One day looks like the next---------------------------------------------------------------------------------------------------
model_results<- NULL
Acc1=round(mean(Dataset$Pred_24H == Dataset$sky_clear),3)
model_results<-bind_rows(model_results,tibble(Model = "One day looks like the next", Accuracy = Acc1))
kable(model_results, caption = "Models Accuracy")


## ----Model: Head and Tail-----------------------------------------------------------------------------------------------------------------
set.seed(1,sample.kind = "Rounding")
Pred_HT <- sample(c(1,0),nb_test, replace = TRUE, prob= c(0.5, 0.5))
Acc2 <- mean(Pred_HT == test_set$Pred_24H)
model_results<-bind_rows(model_results,tibble(Model = "Head and Tail", Accuracy = Acc2))
kable(model_results, caption = "Models Accuracy")


## ----Model: Biased Head and Tail----------------------------------------------------------------------------------------------------------
p <- mean(test_set$Pred_24H==1)
set.seed(1,sample.kind = "Rounding")
Pred_HT <- sample(c(1,0),nb_test, replace = TRUE, prob= c(p, 1-p))
Acc3 <- mean(Pred_HT == train_set$Pred_24H)
model_results<-bind_rows(model_results,tibble(Model = "Biased: Head and Tail", Accuracy = Acc3))
kable(model_results, caption = "Models Accuracy")


## ----Model LDA----------------------------------------------------------------------------------------------------------------------------
train_mod1 <- train(Pred_24H ~ temperature + pressure + humidity + wind_dir + wind_speed, method = "lda", data = train_set)
lda_preds <- predict(train_mod1, test_set)
Acc4 <- mean(lda_preds == test_set$Pred_24H)
model_results<-bind_rows(model_results,tibble(Model = "Linear Discriminant Analysis", Accuracy = Acc4))
kable(model_results, caption = "Models Accuracy")


## ----Model QDA----------------------------------------------------------------------------------------------------------------------------
train_mod2 <- train(Pred_24H ~ temperature + pressure + humidity + wind_dir + wind_speed, method = "qda", data = train_set)
qda_preds <- predict(train_mod2, test_set)
Acc5 <- mean(qda_preds == test_set$Pred_24H)
model_results<-bind_rows(model_results,tibble(Model = "Quadratic Discriminant Analysis", Accuracy = Acc5))
kable(model_results, caption = "Models Accuracy")


## ----Model GLM----------------------------------------------------------------------------------------------------------------------------
train_mod3 <- train(Pred_24H ~ temperature + Hour + Month + sky_clear, method = "glm", data = train_set)
glm_preds <- predict(train_mod3, test_set)
Acc6 <- mean(glm_preds == test_set$Pred_24H)
model_results<-bind_rows(model_results,tibble(Model = "General Linear Model with 4 observations", Accuracy = Acc6))
kable(model_results, caption = "Models Accuracy")


## ----Model KNN 4 params-------------------------------------------------------------------------------------------------------------------
train_mod4 <- train(Pred_24H ~ temperature + sky_clear +Hour + Month, method = "knn",data = train_set)
knn_preds <- predict(train_mod4, test_set)
Acc7 <- mean(knn_preds == test_set$Pred_24H)
model_results<-bind_rows(model_results,tibble(Model = "k-Nearest Neighbours Model ", Accuracy = Acc7))
kable(model_results, caption = "Models Accuracy")


## ----Model Decision Tree 4 params---------------------------------------------------------------------------------------------------------
train_mod5 <- train(Pred_24H ~ temperature + Month + Hour + sky_clear, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),data = train_set)
dt4_preds <- predict(train_mod5, test_set)
Acc8 <- mean(dt4_preds == test_set$Pred_24H)
model_results<-bind_rows(model_results,tibble(Model = "Easy to observe, Decision Tree Model ", Accuracy = Acc8))
kable(model_results, caption = "Models Accuracy")


## ----Simple model of Decision Tree, fig.height=7,fig.width=7------------------------------------------------------------------------------
plot(train_mod5$finalModel)
text(train_mod5$finalModel)


## ----Model Decision Tree full-------------------------------------------------------------------------------------------------------------
train_mod6 <- train(Pred_24H ~ temperature + pressure + humidity + wind_speed + wind_dir + sky_clear +Hour + Month, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),data = train_set)
dt_preds <- predict(train_mod6, test_set)
Acc9 <- mean(dt_preds == test_set$Pred_24H)
model_results<-bind_rows(model_results,tibble(Model = "Decision Tree Model all Variables", Accuracy = Acc9))
kable(model_results, caption = "Models Accuracy")


## ----Most important variables-------------------------------------------------------------------------------------------------------------
Var_imp <- as.data.frame(varImp(train_mod6$finalModel))
Var_imp <- cbind(rownames(Var_imp), Var_imp)
names(Var_imp) <- c("Variable", "Importance")
Var_imp <- Var_imp[order(-Var_imp$Importance),]
row.names(Var_imp) <- NULL
kable(Var_imp, caption = "Most important Variables")


## ----Decision Tree, fig.height=8,fig.width=9----------------------------------------------------------------------------------------------
plot(train_mod6$finalModel)
text(train_mod6$finalModel)


## ----Show results-------------------------------------------------------------------------------------------------------------------------
kable(model_results, caption = "Models Accuracy")

