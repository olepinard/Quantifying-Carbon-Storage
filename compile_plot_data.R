#We are taking in the plot data from and excel sheet and 
#aggregating the carbon values together based on their plot ID



#choose file that will be aggregated
install.packages('bit64')
library(bit64)
library(tidyverse)
trees <- fread(file.choose(), header=T)
plots <- fread(file.choose(), header=T)


trees_concise <- trees %>%
  mutate(CARBON = CARBON_AG*TPA_UNADJ)
  select(CN, PLT_CN, DIA, CARBON, CARBON_AG)  #try rerunning this with TPA multiplication


plot_concise <- plots %>% 
  select(CN, LAT, LON)

trees_cumulative <- trees_concise %>% 
  group_by(PLT_CN) %>% 
  mutate(cumulative_values = sum(CARBON_AG)) %>% 
  mutate(cumulative_diamter = sum(DIA))


#outputting the plots with their cumulitive data
trees_cumulative2 <- trees_concise %>% 
  group_by(PLT_CN) %>% 
  summarize(cumulative_values = sum(CARBON_AG),
            cumulative_diamter = sum(DIA))

#graph comparing the carbon values with the diameter
trees_cumulative2 %>% 
  ggplot(aes(x = cumulative_diamter, 
             y = cumulative_values)) +
  geom_jitter()

#aggragating the coordinates
plots_datad <- merge(trees_cumulative2, plot_concise, by.x = "PLT_CN", by.y = "CN")

#group trees by plot ID to check the number of individual plots

plot_data_reduced <- plots_datad %>%
  na.omit(cumulative_diamter, cumulative_values) %>%  
  group_by(LAT, LON) %>% 
  summarize(PLT_CN = min(PLT_CN), 
            DIA = mean(cumulative_diamter), 
            CARBON = mean(cumulative_values))

write.csv(plots_or, "plot_data")

plots <- read_csv(file.choose())
plots_or <- plots

plots_or <- plots_or %>% 
  dplyr::select(-X1)

colnames(plots_or)[3] <- "Canopy Height"
colnames(plots_or)[5] <- "Aspect"
colnames(plots_or)[6] <- "Blue"
colnames(plots_or)[7] <- "Elevation"
colnames(plots_or)[8] <- "MWIR"
colnames(plots_or)[9] <- "Green"
colnames(plots_or)[10] <- "SWIR"
colnames(plots_or)[11] <- "NDVI"
colnames(plots_or)[12] <- "NIR"
colnames(plots_or)[13] <- "Red"
colnames(plots_or)[14] <- "SAVI"
colnames(plots_or)[15] <- "Slope"



plot_data_reduced %>% 
  ggplot(aes(x = DIA, 
             y = CARBON)) +
  geom_jitter()





#CREATE THE DATASET
#group all of plots band data
#collect file names and read all csv's into a table list
my_files <- list.files("/Users/olepinard/Desktop/Thesis/Band_data")
setwd("/Users/olepinard/Desktop/Thesis/Band_data")
tree_peramiter_list = lapply(my_files, read.csv)

#get plot ID and readjust trim the dataset
plots_band_data <- read.csv("/Users/olepinard/Desktop/Thesis/plot_ID.csv") 
colnames(plots_band_data)[2] <- "FIELD1" 
plots_band_data <- plots_band_data %>% 
  select(FIELD1, LAT, LON, DIA, CARBON)

#run through the list of files and append all of the means to the Plot data
for(i in 1:11){
  temp_data <- tree_peramiter_list[[i]] %>% 
    select(FIELD1, MEAN)
  plots_band_data <- left_join(plots_band_data,
                               temp_data,
                               by = "FIELD1")
  
  colnames(plots_band_data)[5 + i] <- str_trunc(my_files[i], 3, side = c("right"), "")
  
}

#clear all NA's
plots_band_data <- plots_band_data %>% 
  na.omit() 

plots_band_data$FIELD1 <- NULL

#save CSV
write.csv(plots_band_data,"plot_compiled_data")

plots_band_data <- read.csv("/Users/matiawhiting/Desktop/Desktop_old/Thesis/plot_compiled_data")








plots_band_norm %>% 
  ggplot(aes(x = DIA_norm)) +
  geom_bar()




#CREATE DATASET PLANET
my_files <- list.files("/Users/matiawhiting/Desktop/Desktop_old/Thesis/planet_band_data")
setwd("/Users/matiawhiting/Desktop/Desktop_old/Thesis/planet_band_data")
tree_peramiter_list <- lapply(my_files, read.csv)

planet_band_data <- tree_peramiter_list[[1]] %>% 
  select(FID, MEAN)

colnames(planet_band_data)[2] <- "BLUE"

for(i in 2:6){
  temp_data <- tree_peramiter_list[[i]] %>% 
    select(FID, MEAN)
  planet_band_data <- left_join(planet_band_data,
                                temp_data,
                                by = "FID")
  
  colnames(planet_band_data)[1 + i] <- str_trunc(my_files[i], 4, side = c("right"), "")
  
}

planet_band_data <- planet_band_data %>% 
  na.omit() 


write.csv(planet_band_data,"planet_compiled_data")

#------attempting to brig in a raster

library(raster)
library(caret)
library(rgdal)
library(tidyverse)

midd_forest <- brick(file.choose())
names(midd_forest) <- paste0("B", c(1:6))
names(midd_forest)

plotIMG <- plotRGB(midd_forest, r = 3, g = 4, b = 5, scale = 14000, stretch='lin')
plotCH <- plot(midd_forest)

midd_forest_table <- as.data.frame(midd_forest, xy = TRUE, na.rm = TRUE)

#doParallel
registerDoParallel(cores = 8)



