library(dplyr)
library(ggplot2)
library(car)
library(knitr)
library(tidyverse)
library(broom)
library(reshape2)
library(pheatmap)
library(readr)
library(psych)
install.packages('psych')
food_data = read.csv('food_data.csv')

### Data cleaning ###
# transform value which is -9999 and -8888 into NA
food_data[food_data == -9999 | food_data == -8888 ] <- NA


### Multi-linear Regression ###
# standardization
var_scale = c('RECFACPTH20','FFRPTH16','FFRPTH20','FSRPTH16','FSRPTH20','PCT_CACFP17','PCT_CACFP21','GROCPTH16','GROCPTH20',
              'SUPERCPTH16','SUPERCPTH20','SPECSPTH16','SPECSPTH20','DSPTH20','SNAPSPTH17','SNAPSPTH23','FMRKTPTH18')
for (i in var_scale){
  food_data[i] <- scale(food_data[i])
} 
food_data$MEDHHINC21 = log(food_data$MEDHHINC21)  # log

## Diabetes2015 ##
#FOODSTORES15
dia_15_foodstores <- lm(PCT_DIABETES_ADULTS15 ~ GROCPTH16 + SUPERCPTH16 + SPECSPTH16 + SNAPSPTH17, data = food_data, na.action = na.omit )
summary(dia_15_foodstores)
vif(dia_15_foodstores)
tidy_dia_15_foodstores <- tidy(dia_15_foodstores) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value)
tidy_dia_15_foodstores

#RESTAURANTS15
dia_15_restaurant <- lm(PCT_DIABETES_ADULTS15 ~ FFRPTH16 + FSRPTH16, data = food_data, na.action = na.omit )
summary(dia_15_restaurant)
vif(dia_15_restaurant)
tidy_dia_15_restaurant <- tidy(dia_15_restaurant) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value) 
tidy_dia_15_restaurant

#TAXES15
dia_15_taxes <- lm(PCT_DIABETES_ADULTS15 ~ SODATAX_STORES14 + SODATAX_VENDM14 + CHIPSTAX_STORES14 + CHIPSTAX_VENDM14, data = food_data, na.action = na.omit )
summary(dia_15_taxes)
tidy_dia_15_taxes <- tidy(dia_15_taxes) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value) 
tidy_dia_15_taxes

#All 2015
dia_15 <- lm( PCT_DIABETES_ADULTS15 ~ GROCPTH16 + SUPERCPTH16 + SPECSPTH16 + SNAPSPTH17 + FFRPTH16 + FSRPTH16, data = food_data, na.action = na.omit )
summary(dia_15)
vif(dia_15)
tidy_dia_15 <- tidy(dia_15) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value) 
tidy_dia_15

## Diabetes2019 ##
#FOODSTORES19
dia_19_foodstores <- lm(PCT_DIABETES_ADULTS19 ~ GROCPTH20 + SUPERCPTH20 + SPECSPTH20 + SNAPSPTH17 + DSPTH20, data = food_data, na.action = na.omit )
summary(dia_19_foodstores)
vif(dia_19_foodstores)
tidy_dia_19_foodstores <- tidy(dia_19_foodstores) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value) 
tidy_dia_19_foodstores

#RESTAURANTS19
dia_19_restaurant <- lm(PCT_DIABETES_ADULTS19 ~ FFRPTH20 + FSRPTH20, data = food_data, na.action = na.omit )
summary(dia_19_restaurant)
vif(dia_19_restaurant)
tidy_dia_19_restaurant <- tidy(dia_19_restaurant) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value) 
tidy_dia_19_restaurant

#ASSISTANCE19
dia_19_assistance <- lm(PCT_DIABETES_ADULTS19 ~ PCT_SNAP17 + PCT_CACFP17 + FOOD_BANKS18, data = food_data, na.action = na.omit )
summary(dia_19_assistance)
vif(dia_19_assistance)
tidy_dia_19_assistance <- tidy(dia_19_assistance) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value) 
tidy_dia_19_assistance

#INSECURITY19
dia_19_insecurity <- lm(PCT_DIABETES_ADULTS19 ~ FOODINSEC_18_20 + VLFOODSEC_18_20, data = food_data, na.action = na.omit )
summary(dia_19_insecurity)
vif(dia_19_insecurity)
tidy_dia_19_insecurity <- tidy(dia_19_insecurity) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value) 
tidy_dia_19_insecurity

#TAXES19
dia_19_taxes <- lm(PCT_DIABETES_ADULTS19 ~ SODATAX_STORES14 + SODATAX_VENDM14 + CHIPSTAX_STORES14 + CHIPSTAX_VENDM14, data = food_data, na.action = na.omit )
summary(dia_19_taxes)
vif(dia_19_taxes)
tidy_dia_19_taxes <- tidy(dia_19_taxes) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value) 
tidy_dia_19_taxes

#LOCAL19
dia_19_local <- lm(PCT_DIABETES_ADULTS19 ~ PC_DIRSALES17 + FMRKTPTH18 + FARM_TO_SCHOOL19, data = food_data, na.action = na.omit )
summary(dia_19_local)
vif(dia_19_local)
tidy_dia_19_local <- tidy(dia_19_local) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value) 
tidy_dia_19_local

#HEALTH19
dia_19_health <- lm(PCT_DIABETES_ADULTS19 ~ RECFACPTH20, data = food_data, na.action = na.omit )
summary(dia_19_health)
tidy_dia_19_health <- tidy(dia_19_health) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value) 
tidy_dia_19_health

#All 2019
dia_19 <- lm(PCT_DIABETES_ADULTS19 ~ GROCPTH20 + SUPERCPTH20 + 
    SPECSPTH20 + SNAPSPTH17 + DSPTH20 + FFRPTH20 + FSRPTH20 +
    PCT_SNAP17 + PCT_CACFP17 + FOOD_BANKS18 + PC_DIRSALES17 + 
    FMRKTPTH18 + FARM_TO_SCHOOL19 + RECFACPTH20,
    data = food_data, na.action = na.omit )
summary(dia_19)
vif(dia_19 )
tidy_dia_19 <- tidy(dia_19) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value
  ) 
tidy_dia_19


## Obesity 2017 ##
#FOODSTORES17
obese_17_foodstores = lm(PCT_OBESE_ADULTS17 ~ GROCPTH16 + SUPERCPTH16 + SPECSPTH16 + SNAPSPTH17  , data = food_data, na.action = na.omit)
summary(obese_17_foodstores)
vif(obese_17_foodstores)  # check multicollinearity
tidy_obese_17_foodstores <- tidy(obese_17_foodstores) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value) 
tidy_obese_17_foodstores

#RESTAURANTS17
obese_17_restaurant = lm(PCT_OBESE_ADULTS17~ FFRPTH16 + FSRPTH16, data = food_data, na.action = na.omit)
summary(obese_17_restaurant)
vif(obese_17_restaurant)  # check multicollinearity
tidy_obese_17_restaurant <- tidy(obese_17_restaurant) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value)
tidy_obese_17_restaurant

#ASSISTANCE 2017
obese_17_assistance = lm(PCT_OBESE_ADULTS17 ~ PCT_CACFP17 + PCT_SNAP17 + FOOD_BANKS18, data = food_data, na.action = na.omit)
summary(obese_17_assistance)
vif(obese_17_assistance)  # check multicollinearity
tidy_obese_17_assistance <- tidy(obese_17_assistance) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value)
tidy_obese_17_assistance

#TAXES17
obese_17_taxes = lm(PCT_OBESE_ADULTS17 ~ SODATAX_STORES14 + SODATAX_VENDM14 + CHIPSTAX_STORES14 + CHIPSTAX_VENDM14, data = food_data, na.action = na.omit)
summary(obese_17_taxes)
vif(obese_17_taxes)  # check multicollinearity
tidy_obese_17_taxes <- tidy(obese_17_taxes) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value)
tidy_obese_17_taxes

## All 2017
obese_17 = lm(PCT_OBESE_ADULTS17 ~ GROCPTH16 + SUPERCPTH16 + SPECSPTH16 + SNAPSPTH17 + PCT_CACFP17 + FOOD_BANKS18 ,data = food_data, na.action = na.omit)
summary(obese_17)
vif(obese_17) # check multicollinearity
tidy_obese_17 <- tidy(obese_17) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value)
tidy_obese_17

## Obesity 2022 ##
#FOODSTORES22
obese_22_foodstores = lm(PCT_OBESE_ADULTS22 ~ GROCPTH20 + SUPERCPTH20 + SPECSPTH20 + SNAPSPTH23 + DSPTH20  , data = food_data, na.action = na.omit)
summary(obese_22_foodstores)
vif(obese_22_foodstores) # check multicollinearity
tidy_obese_22_foodstores <- tidy(
  obese_22_foodstores,
  conf.int = TRUE,
  conf.level = 0.95) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value,
    'Conf.Low' = conf.low,
    'Conf.High' = conf.high)
tidy_obese_22_foodstores 

#RESTAURANTS22
obese_22_restaurant = lm(PCT_OBESE_ADULTS22 ~ FFRPTH20 + FSRPTH20, data = food_data, na.action = na.omit)
summary(obese_22_restaurant)
vif(obese_22_restaurant)  # check multicollinearity
tidy_obese_22_restaurant <- tidy(obese_22_restaurant) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value)
tidy_obese_22_restaurant

#ASSISTANCE 2022
obese_22_assistance = lm(PCT_OBESE_ADULTS22 ~ PCT_CACFP21 + PCT_SNAP22 + FOOD_BANKS21, data = food_data, na.action = na.omit)
summary(obese_22_assistance)
vif(obese_22_assistance)  # check multicollinearity
tidy_obese_22_assistance <- tidy(obese_22_assistance) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value)
tidy_obese_22_assistance

#INSECURITY22
obese_22_insecurity = lm(PCT_OBESE_ADULTS22 ~ FOODINSEC_18_20 + VLFOODSEC_18_20, data = food_data, na.action = na.omit)
summary(obese_22_insecurity)
vif(obese_22_insecurity)  # check multicollinearity
tidy_obese_22_insecurity<- tidy(obese_22_insecurity) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value)
tidy_obese_22_insecurity

#TAXES22
obese_22_taxes = lm(PCT_OBESE_ADULTS22 ~ SODATAX_STORES14 + SODATAX_VENDM14 + CHIPSTAX_STORES14 + CHIPSTAX_VENDM14, data = food_data, na.action = na.omit)
summary(obese_22_taxes)
vif(obese_22_taxes)  # check multicollinearity
tidy_obese_22_taxes <- tidy(obese_22_taxes) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value)
tidy_obese_22_taxes

#LOCAL22
obese_22_local = lm(PCT_OBESE_ADULTS22 ~ PC_DIRSALES17 + FMRKTPTH18 + FARM_TO_SCHOOL19, data = food_data, na.action = na.omit)
summary(obese_22_local)
vif(obese_22_local )  # check multicollinearity
tidy_obese_22_local <- tidy(obese_22_local) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value)
tidy_obese_22_local

#HEALTH22
obese_22_health = lm(PCT_OBESE_ADULTS22 ~ PCT_HSPA21 + RECFACPTH20, data = food_data, na.action = na.omit)
summary(obese_22_health)
vif(obese_22_health)  # check multicollinearity
tidy_obese_22_health <- tidy(obese_22_health) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value)
tidy_obese_22_health

#SOCIOECONOMIC22
obese_22_socioeconomic = lm(PCT_OBESE_ADULTS22 ~ MEDHHINC21 + POVRATE21, data = food_data, na.action = na.omit)
summary(obese_22_socioeconomic)
vif(obese_22_socioeconomic) # check multicollinearity
tidy_obese_22_socioeconomic <- tidy(obese_22_socioeconomic) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value)
tidy_obese_22_socioeconomic

#All 2022
obese_22 = lm(PCT_OBESE_ADULTS22 ~ GROCPTH20 + SUPERCPTH20 + SPECSPTH20 + SNAPSPTH23 + DSPTH20 + FFRPTH20 + FSRPTH20 + 
                VLFOODSEC_18_20 + SODATAX_STORES14 + CHIPSTAX_STORES14 + CHIPSTAX_VENDM14 + MEDHHINC21 + POVRATE21,
              data = food_data, na.action = na.omit)
summary(obese_22)
vif(obese_22) # check multicollinearity
tidy_obese_22 <- tidy(obese_22) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value)
tidy_obese_22


### PCA ###
# all standardization
num_cols <- sapply(food_data, is.numeric)
food_data[num_cols] <- scale(food_data[num_cols])

## Diabetes 2015 ##
#PCA
pca_variable2015 <- c(
  "GROCPTH16", "SNAPSPTH17","FFRPTH16", "FSRPTH16", "SODATAX_STORES14", 
  "SODATAX_VENDM14", "CHIPSTAX_STORES14", "CHIPSTAX_VENDM14")
pca_data2015 <- food_data |>
  select(PCT_DIABETES_ADULTS15, all_of(pca_variable2015)) |>
  drop_na()
pca_result2015 <- prcomp(
  pca_data2015 |> select(all_of(pca_variable2015)), center = TRUE, scale. = TRUE)
summary(pca_result2015)

#PCA Regression 
pca_scores2015 <- as.data.frame(pca_result2015$x[, 1:4])
colnames(pca_scores2015) <- paste0("PC", 1:4)
reg15_df <- cbind(
  pca_data2015 |> select(PCT_DIABETES_ADULTS15),
  pca_scores2015
)
pcr_2015 <- lm(PCT_DIABETES_ADULTS15 ~ ., data = reg15_df)
summary(pcr_2015 )
tidy_pcr_2015  <- tidy(pcr_2015 ) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value
  ) 
tidy_pcr_2015
sort((pca_result2015$rotation[, "PC3"]), decreasing = TRUE)

#Visualization PC3 in 2015
pca_result2015_rotation <- data.frame(
  Variable = rownames(pca_result2015$rotation),
  Loading = pca_result2015$rotation[, "PC3"]) |>
  arrange(desc(Loading))

ggplot(pca_result2015_rotation, aes(x = reorder(Variable,Loading),
                                    y = Loading)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() +
  theme_minimal() +
  labs(x = 'Food Variables',
       y = 'Factor Loadings',
       title = 'PCA Loadings for PC3 (Diabetes,2015)')


## Diabetes 2019 ##
#PCA
pca_variable2019 <- c("GROCPTH20", "SNAPSPTH17", "DSPTH20", "FFRPTH20", "FSRPTH20",
  "PCT_SNAP17", "PCT_CACFP17", "FOOD_BANKS18", "FOODINSEC_18_20", "VLFOODSEC_18_20",
  "SODATAX_STORES14", "SODATAX_VENDM14", "CHIPSTAX_STORES14", "CHIPSTAX_VENDM14",
  "PC_DIRSALES17", "FMRKTPTH18", "FARM_TO_SCHOOL19")
pca_data2019 <- food_data |>
  select(PCT_DIABETES_ADULTS19, all_of(pca_variable2019)) |>
  drop_na()
pca_result2019 <- prcomp(
  pca_data2019 |> select(all_of(pca_variable2019)), center = TRUE, scale. = TRUE)
summary(pca_result2019)

#PCA Regression 
pca_scores2019 <- as.data.frame(pca_result2019$x[, 1:8])
colnames(pca_scores2019) <- paste0("PC", 1:8)
reg19_df <- cbind(
  pca_data2019 |> select(PCT_DIABETES_ADULTS19),
  pca_scores2019
)
pcr_2019 <- lm(PCT_DIABETES_ADULTS19 ~ ., data = reg19_df)
summary(pcr_2019 )
tidy_pcr_2019  <- tidy(pcr_2019 ) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value
  ) 
tidy_pcr_2019
sort((pca_result2019$rotation[, "PC1"]), decreasing = TRUE)

#Visualization PC1 in 2019
pca_result2019_rotation <- data.frame(
  Variable = rownames(pca_result2019$rotation),
  Loading = pca_result2019$rotation[, "PC1"]) |>
  arrange(desc(Loading))

ggplot(pca_result2019_rotation, aes(x = reorder(Variable,Loading),
                                    y = Loading)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() +
  theme_minimal() +
  labs(x = 'Food Variables',
       y = 'Factor Loadings',
       title = 'PCA Loadings for PC1 (Diabetes,2019)')


## Obesity 2017 ##
#PCA
pca_variable2017 <- c(
  'GROCPTH16','SNAPSPTH17','FFRPTH16','FSRPTH16','PCT_SNAP17','PCT_CACFP17','FOOD_BANKS18',
  'SODATAX_STORES14','SODATAX_VENDM14','CHIPSTAX_STORES14','CHIPSTAX_VENDM14')
pca_data2017 <- food_data |>
  select(PCT_OBESE_ADULTS17, all_of(pca_variable2017)) |>
  drop_na()
pca_result2017 <- prcomp(
  pca_data2017 |> select(all_of(pca_variable2017)), center = TRUE, scale. = TRUE)
summary(pca_result2017)

#PCA Regression 
pca_scores2017 <- as.data.frame(pca_result2017$x[, 1:6])
colnames(pca_scores2017) <- paste0("PC", 1:6)
reg17_df <- cbind(
  pca_data2017 |> select(PCT_OBESE_ADULTS17),
  pca_scores2017
)
pcr_2017 <- lm(PCT_OBESE_ADULTS17 ~ ., data = reg17_df)
summary(pcr_2017 )
tidy_pcr_2017  <- tidy(pcr_2017 ) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value
  ) 
tidy_pcr_2017
sort((pca_result2017$rotation[, "PC3"]), decreasing = TRUE)

#Visualization PC3 in 2017
pca_result2017_rotation <- data.frame(
  Variable = rownames(pca_result2017$rotation),
  Loading = pca_result2017$rotation[, "PC3"]) |>
  arrange(desc(Loading))

ggplot(pca_result2017_rotation, aes(x = reorder(Variable,Loading),
                                    y = Loading)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() +
  theme_minimal() +
  labs(x = 'Food Variables',
       y = 'Factor Loadings',
       title = 'PCA Loadings for PC3 (Obesity,2017)')

## Obesity 2022 ##
#PCA
pca_variable2022 <- c('GROCPTH20','SNAPSPTH23','DSPTH20','FFRPTH20','FSRPTH20','FOODINSEC_18_20','VLFOODSEC_18_20',
                      'SODATAX_STORES14','SODATAX_VENDM14','CHIPSTAX_STORES14','CHIPSTAX_VENDM14','MEDHHINC21','POVRATE21',
                      'PCT_CACFP21','PCT_SNAP22','FOOD_BANKS21','PC_DIRSALES17','FMRKTPTH18','FARM_TO_SCHOOL19','PCT_HSPA21',
                      'CHILDPOVRATE21')
pca_data2022 <- food_data |>
  select(PCT_OBESE_ADULTS22, all_of(pca_variable2022)) |>
  drop_na()
pca_result2022 <- prcomp(
  pca_data2022 |> select(all_of(pca_variable2022)), center = TRUE, scale. = TRUE)
summary(pca_result2022)

#PCA Regression 
pca_scores2022 <- as.data.frame(pca_result2022$x[, 1:10])
colnames(pca_scores2022) <- paste0("PC", 1:10)
reg22_df <- cbind(
  pca_data2022 |> select(PCT_OBESE_ADULTS22),
  pca_scores2022
)
pcr_2022 <- lm(PCT_OBESE_ADULTS22 ~ ., data = reg22_df)
summary(pcr_2022 )
tidy_pcr_2022  <- tidy(pcr_2022 ) |>
  rename(
    Variable = term,
    Estimate = estimate,
    'Std. Error' = std.error,
    't value' = statistic,
    'Pr(>|t|)' = p.value
  ) 
tidy_pcr_2022
sort((pca_result2022$rotation[, "PC1"]), decreasing = TRUE)

#Visualization PC1 in 2022
pca_result2022_rotation <- data.frame(
  Variable = rownames(pca_result2022$rotation),
  Loading = pca_result2022$rotation[, "PC1"]) |>
  arrange(desc(Loading))

ggplot(pca_result2022_rotation, aes(x = reorder(Variable,Loading),
                                    y = Loading)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() +
  theme_minimal() +
  labs(x = 'Food Variables',
       y = 'Factor Loadings',
       title = 'PCA Loadings for PC1 (Obesity,2022)')

