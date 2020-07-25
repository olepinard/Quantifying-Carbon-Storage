install.packages('randomForest')
library(randomForest)
install.packages('Metrics')
library(Metrics)
install.packages('caret')
library(caret)

#Ask user to select the file that needs to be run
carbon_values <- read.csv("/Users/olepinard/Desktop/Thesis/plot_compiled_data", header=T)
carbon_values <- read.csv(file.choose())
carbon_values_LANDSAT <-carbon_values

carbon_values <- carbon_values_LANDSAT %>% 
  filter(CARBON > 2500) %>% 
  dplyr::select(Canopy.Height, CARBON)

drop <- c( "X", "FID")
carbon_values <- carbon_values[,!(names(carbon_values) %in% drop)]

carbon_values %>% 
  filter(NDSM < 1)

train <- sample(nrow(carbon_values), 0.5*nrow(carbon_values), replace = FALSE)
train_set <- carbon_values[train,]
valid_set <- carbon_values[-train,]

#running the random forest algorythm with the default perameters
#using carbon as the delineating variable
carbon_values.rf = randomForest(NDSM ~ ., data = train_set , ntree = 200, importance = TRUE)
carbon_values.rf

plot(carbon_values.rf)


#predicting 
# Predicting on train set
pred_train <- predict(carbon_values.rf, train_set, type = "class")
# Checking classification accuracy
train_set <- train_set %>% 
  mutate(error = rmse(CARBON, pred_train)) %>% 
  mutate(predicted = pred_train)

train_set %>% 
  ggplot(aes(x = CARBON, y = predicted)) +
  geom_point()+
  xlab("Measured Carbon")+
  ylab("Predicted Carbon")





# Predicting on Validation data
pred_valid <- predict(carbon_values.rf, valid_set, type ="class")
#check percent accuracy
valid_set <- valid_set %>% 
  mutate(error = rmse(NDSM, pred_valid)) %>% 
  mutate(predicted = pred_valid)

lm_line <- valid_set %>% 
  lm(predicted ~ NDSM, data = .)+
  summary()

lm_line

valid_set %>% 
  ggplot(aes(x = NDSM, y = predicted)) +
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color = "forestgreen", size = 1.5)+
  geom_abline(intercept = 2.95 , slope = .19, color = "red3", size = 1.4)+
  xlab("Measured Canopy Height")+
  ylab("Predicted Canopy Height")+
  ylim(0,30)


soccer2 %>% 
  geom

midd_fall_data <- readRDS(file.choose())
summer_model <- readRDS(file.choose())

summer_raster <- rasterFromXYZ(carbon_values_raster)
plot(summer_raster)  
plotRGB(summer_raster, r = 3, g = 2, b = 1, scale = 14000, stretch='lin')


writeRaster(fall_midd_raster_predicted, "fall_summer_predicted.tif", "GTiff", bylayer = F)

#check the importance of all variables
importance(carbon_values.rf)
varImpPlot(summer_model)

midd_summer_predicted %>% 
  lm(predicted ~ B6, data = .) %>% 
  summary()



pred_total <- predict(carbon_values.rf, carbon_values, type ="class")
#check percent accuracy
carbon_values <- carbon_values %>% 
  mutate(error = rmse(CARBON, pred_total)) %>% 
  mutate(predicted = pred_total)
carbon_values %>% 
  ggplot(aes(x = CARBON, y = predicted)) +
  geom_point()+
  xlab("Measured Carbon")+
  ylab("Predicted Carbon")




#ALL OF this is a for loop to test the different perameters of the program in order 
#type1
a=c()

for (i in 3:12) {
  model3 <- randomForest(CARBON ~ ., data = train_set, ntree = 10, mtry = 12, importance = TRUE)
  pred_valid <- predict(model3, valid_set, type = "class")
  a[i-2] = mean(rmse(valid_set$CARBON, pred_valid))
}


plot(1:12,a)








planet_band_data %>% 
  ggplot(aes(x= NDSM, y = NDVI)) +
  geom_point()










carbon_values <- read.csv(file.choose())

carbon_values_original <- carbon_values_planet 


carbon_values_planet <- carbon_values_original %>% 
  filter(FID < 2000) %>% 
  select(-FID) %>% 
  select(-X)


train <- sample(nrow(carbon_values_planet), 0.7*nrow(carbon_values_planet), replace = FALSE)
train_set_planet <- carbon_values_planet[train,]
valid_set_planet <- carbon_values_planet[-train,]

#running the random forest algorythm with the default perameters
#using carbon as the delineating variable
carbon_values_planet.rf = randomForest(NDSM ~ ., data = train_set_planet , ntree = 1000, mtry = 3,importance = TRUE)
carbon_values_planet.rf

plot(carbon_values_planet.rf)


#predicting 
# Predicting on train set
pred_train_planet <- predict(carbon_values_planet.rf, train_set_planet, type = "response")
# Checking classification accuracy
train_set_planet <- train_set_planet %>% 
  mutate(error = rmse(NDSM, pred_train_planet)) %>% 
  mutate(predicted = pred_train_planet)

train_set_planet %>% 
  ggplot(aes(x = NDSM, y = predicted)) +
  geom_point()+
  xlab("Measured Carbon")+
  ylab("Predicted Carbon")






median(train_set$error)

median(valid_set$error)

plot(error)
max(error)

# Predicting on Validation data
pred_valid_planet <- predict(carbon_values_planet.rf, valid_set_planet, type ="class")
#check percent accuracy
valid_set_planet <- valid_set_planet %>% 
  mutate(error = rmse(NDSM, pred_valid_planet)) %>% 
  mutate(predicted = pred_valid_planet)

valid_set_planet %>% 
  ggplot(aes(x = NDSM, y = predicted)) +
  geom_point()+
  xlab("Measured Carbon")+
  ylab("Predicted Carbon")



#check the importance of all variables
importance(carbon_values_planet.rf)

varImpPlot(carbon_values_planet.rf)




pred_total_planet <- predict(carbon_values_planet.rf, carbon_values, type ="class")
#check percent accuracy
carbon_values_planet <- carbon_values_planet %>% 
  mutate(error = rmse(NDSM, pred_total_planet)) %>% 
  mutate(predicted = pred_total)

carbon_values_planet %>% 
  ggplot(aes(x = NDSM, y = predicted)) +
  geom_point()+
  xlab("Measured Carbon")+
  ylab("Predicted Carbon")


rf2 <- caret::train(carbon_values_planet[,-3],
                    carbon_values_planet[,3],
                    method ="svmPoly")

#support vector machine method
#svmPoly
#bagging or booting method
#try neural network
#nnet





 




carbon_values_raster <- midd_trim_trial



train <- sample(nrow(carbon_values_raster), 0.5*nrow(carbon_values_raster), replace = FALSE)
train_set_raster <- carbon_values_raster[train,]
valid_set_raster <- carbon_values_raster[-train,]

#running the random forest algorythm with the default perameters
#using carbon as the delineating variable

system.time(carbon_values_raster.rf <-randomForest(B6 ~ ., data = train_set_raster ,importance = TRUE))
pred_total_raster <- predict(carbon_values_raster.rf, carbon_values_raster, type ="class")
carbon_values_raster_result <- carbon_values_raster %>% 
  mutate(predicted = pred_total_raster) %>% 
  mutate(dif = predicted-B6) %>% 
  select(x,y,B6, predicted, dif)
fall_midd_raster_predicted <- rasterFromXYZ(carbon_values_raster_result)
plot(fall_midd_raster_predicted)

saveRDS(carbon_values_raster.rf, "midd_fall_model")
small_fall_model <- readRDS("small_fall_model")
small_dataframe <- readRDS('small_fall_dataframe')
winter_dataframe <- readRDS("winter_midd_dataframe")



winter_raster <- rasterFromXYZ(winter_dataframe)
plot(winter_raster)

small_raster <- rasterFromXYZ(small_dataframe)
plot(small_raster[[6]])
plotRGB(small_raster, r = 3, g = 2, b = 1, scale = 14000, stretch='lin')



pred_total_raster <- predict(small_fall_model, small_dataframe, type ="class")
small_fall_result <- small_dataframe %>% 
  mutate(predicted = pred_total_raster) %>% 
  mutate(dif = predicted-B6) %>% 
  dplyr::select(x,y,B6, predicted, dif)
small_raster_predicted <- rasterFromXYZ(small_fall_result)
plot(small_raster_predicted[[1]])
plot(small_raster_predicted[[2]])




small_fall_result %>% 
   lm(predicted ~ B6, data = .) %>% 
   summary()
 
 

median(valid_set_raster$error)


# Predicting on Validation data
pred_valid_raster <- predict(carbon_values_raster.rf, valid_set_raster, type ="class")
#check percent accuracy
valid_set_raster <- valid_set_raster %>% 
  mutate(error = rmse(B2, pred_valid_raster)) %>% 
  mutate(predicted = pred_valid_raster)

valid_set_raster %>% 
  ggplot(aes(x = B2, y = predicted)) +
  geom_point()+
  xlab("Measured Carbon")+
  ylab("Predicted Carbon")

valid_set_raster %>% 
  lm(predicted ~ B2, data = .) %>% 
  summary()

#check the importance of all variables
importance(carbon_values_raster.rf)

varImpPlot(carbon_values_raster.rf)




pred_total_raster <- predict(carbon_values_raster.rf, carbon_values_raster, type ="class")
#check percent accuracy
carbon_values_raster <- carbon_values_raster %>% 
  mutate(error = rmse(B2, pred_total_raster)) %>% 
  mutate(predicted = pred_total_raster)

carbon_values_raster %>% 
  ggplot(aes(x = B2, y = predicted)) +
  geom_point()+
  xlab("Measured Carbon")+
  ylab("Predicted Carbon")

carbon_values_raster %>% 
  lm(predicted ~ B2, data = .) %>% 
  summary()

write_csv(carbon_values_raster, "raster_total_output")
