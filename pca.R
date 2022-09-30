library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
#======================================================================================
# Step 1 : read data
#======================================================================================
#------------------------------
# Original Data
#------------------------------

data_directory = "C:/Users/FX505/Desktop/multipleRA"
land_before =  read_excel( file.path(data_directory, "land.xls"),sheet = 1)

#======================================================================================
# Step 2 : clean data
#======================================================================================
#------------------------------
# remove col what we don't use
#------------------------------
land = land_before[,-c(3,5,6,7,9,10,11,14,24,25,26,27,28,33)]
land = land[-c(1),]
#------------------------------
# rename colname
#------------------------------
names(land) <- c("localArea","tradeSign","landShiftArea","tradeDate","type","mainUse","completeDate","totalKM",
                 "room_amount","living_room_amount","bathroom_amount","presentSituation","whetherManage",
                 "totalPrize","perPrize","mainBuildArea","secondBuildArea","balconyArea","elevator")
#------------------------------
# remove NA data
#------------------------------
land <- land %>% drop_na()

#------------------------------
# add house age
#------------------------------
land$age <- as.numeric(substr(land$tradeDate,1,3))-as.numeric(substr(land$completeDate,1,3))
land <- land[-grep("tradeDate",colnames(land))]
land <- land[-grep("completeDate",colnames(land))]


# 創建虛擬變數
library('fastDummies')
land_test <- dummy_cols(land, select_columns = c('localArea','tradeSign', 'mainUse','type','presentSituation','whetherManage','elevator'),
                      remove_selected_columns = TRUE)
#------------------------------
# remove outlier
#------------------------------
land <- land[!grepl("五層，六層，七層", land$shiftFloor),]
land <- land[!grepl("三層，四層", land$shiftFloor),]
land <- land[!grepl("屋頂突出物", land$shiftFloor),]
land <- land[!grepl("見其他登記事項", land$shiftFloor),]

#------------------------------
# convert to number
#------------------------------
land$localArea <- case_when(
  land$localArea == "士林區"  ~ 0,
  land$localArea == "大同區" ~ 1,
  land$localArea == "大安區" ~ 2,
  land$localArea == "中山區" ~ 3,
  land$localArea == "中正區" ~ 4,
  land$localArea == "內湖區" ~ 5,
  land$localArea == "文山區" ~ 6,
  land$localArea == "北投區" ~ 7,
  land$localArea == "松山區" ~ 8,
  land$localArea == "信義區" ~ 9,
  TRUE ~ 10
)

land$tradeSign <- case_when(
  land$tradeSign == "房地(土地+建物)"  ~ 0,
  land$tradeSign == "房地(土地+建物)+車位" ~ 1,
  TRUE ~ 2
)

land$presentSituation <- case_when(
  land$presentSituation == "有"  ~ 1,
  land$presentSituation == "無" ~ 2,
  TRUE ~ 0
)

land$whetherManage <- case_when(
  land$whetherManage == "有"  ~ 1,
  land$whetherManage == "無" ~ 2,
  TRUE ~ 0
)

land$elevator <- case_when(
  land$elevator == "有"  ~ 1,
  land$elevator == "無" ~ 2,
  TRUE ~ 0
)

land$mainUse <- case_when(
  land$mainUse == "住家用"  ~ 0,
  land$mainUse == "商業用" ~ 1,
  land$mainUse == "辦公用" ~ 2,
  TRUE ~ 3
)

land$type <- case_when(
  land$type == "住宅大樓(11層含以上有電梯)"  ~ 4,
  land$type == "公寓(5樓含以下無電梯)" ~ 3,
  land$type == "華廈(10層含以下有電梯)" ~ 2,
  land$type == "透天厝" ~ 1,
  TRUE ~ 0
)

land$shiftFloor <- case_when(
  land$shiftFloor == "一層"  ~ 1,
  land$shiftFloor == "一層，平台" ~ 2,
  land$shiftFloor == "一層，夾層" ~ 3,
  land$shiftFloor == "一層，騎樓" ~ 4,
  land$shiftFloor == "二層" ~ 5,
  land$shiftFloor == "二層，陽臺" ~ 6,
  land$shiftFloor == "二層，電梯樓梯間" ~ 7,
  land$shiftFloor == "三層" ~ 8,
  land$shiftFloor == "三層，陽臺" ~ 9,
  land$shiftFloor == "三層，電梯樓梯間" ~ 10,
  land$shiftFloor == "四層" ~ 11,
  land$shiftFloor == "四層，陽臺" ~ 12,
  land$shiftFloor == "五層" ~ 13,
  land$shiftFloor == "五層，陽臺" ~ 14,
  land$shiftFloor == "六層" ~ 15,
  land$shiftFloor == "六層，陽臺" ~ 16,
  land$shiftFloor == "七層" ~ 17,
  land$shiftFloor == "七層，夾層" ~ 18,
  land$shiftFloor == "八層" ~ 19,
  land$shiftFloor == "九層" ~ 20,
  land$shiftFloor == "十層" ~ 21,
  land$shiftFloor == "十一層" ~ 22,
  land$shiftFloor == "十二層" ~ 23,
  land$shiftFloor == "十二層，陽臺" ~ 24,
  land$shiftFloor == "十三層" ~ 25,
  land$shiftFloor == "十四層" ~ 26,
  land$shiftFloor == "十五層" ~ 27,
  land$shiftFloor == "十六層" ~ 28,
  land$shiftFloor == "十七層" ~ 29,
  land$shiftFloor == "十九層" ~ 30,
  land$shiftFloor == "二十一層" ~ 31,
  land$shiftFloor == "全" ~ 32,
  land$shiftFloor == "地下一層" ~ 33,
  land$shiftFloor == "地下層" ~ 34,
  TRUE ~ 0
)

land$totalFloor <- case_when(
  land$totalFloor == "二層"  ~ 1,
  land$totalFloor == "三層" ~ 2,
  land$totalFloor == "四層" ~ 3,
  land$totalFloor == "五層" ~ 4,
  land$totalFloor == "六層" ~ 5,
  land$totalFloor == "七層" ~ 6,
  land$totalFloor == "八層" ~ 7,
  land$totalFloor == "九層" ~ 8,
  land$totalFloor == "十層" ~ 9,
  land$totalFloor == "十一層" ~ 10,
  land$totalFloor == "十二層" ~ 11,
  land$totalFloor == "十三層" ~ 12,
  land$totalFloor == "十四層" ~ 13,
  land$totalFloor == "十五層" ~ 14,
  land$totalFloor == "十六層" ~ 15,
  land$totalFloor == "十七層" ~ 16,
  land$totalFloor == "十八層" ~ 17,
  land$totalFloor == "十九層" ~ 18,
  land$totalFloor == "二十一層" ~ 19,
  land$totalFloor == "二十四層" ~ 20,
  land$totalFloor == "二十六層" ~ 21,
  land$totalFloor == "二十七層" ~ 22,
  land$totalFloor == "三十層" ~ 23,
  TRUE ~ 0
)

land$landShiftArea <- as.numeric(land$landShiftArea)
land$totalKM <- as.numeric(land$totalKM)
land$room_amount <- as.numeric(land$room_amount)
land$living_room_amount <- as.numeric(land$living_room_amount)
land$bathroom_amount <- as.numeric(land$bathroom_amount)
land$totalPrize <- as.numeric(land$totalPrize)
land$perPrize <- as.numeric(land$perPrize)
str(land)

#------------------------------
# Correlation
#------------------------------

cor(land)[14,]

#------------------------------
# remove outliers
#------------------------------

outliers <-boxplot(land$totalKM, plot=FALSE)$out
land_after <- land
land_after<- land[-which(land$totalKM %in% outliers),]

#======================================================================================
# Step 3 : Exploratory Data Analysis
#======================================================================================
#------------------------------
# EDA
#------------------------------
ggplot(land_after, aes(x = age, y = totalPrize)) + geom_point()
ggplot(land_after, aes(x = mainBuildArea, y = totalPrize)) + geom_boxplot()

#======================================================================================
# Step 4 : linear model
#======================================================================================
#-----------------------------------------
# multiple linear model(all variance)
#-----------------------------------------

mlm <- lm(totalPrize ~totalKM + room_amount +localArea +tradeSign+landShiftArea +shiftFloor+totalFloor
          +type+mainUse+living_room_amount+bathroom_amount+presentSituation+whetherManage+perPrize
          +mainBuildArea+secondBuildArea+balconyArea+elevator+age, data = land_after)
summary(mlm)

#-----------------------------------------
# multiple linear model(select variance)
#-----------------------------------------

mlm_select <- lm(totalPrize ~totalKM +mainUse+landShiftArea+age, data = land_after)
summary(mlm_select)

#-----------------------------------------
# use model
#-----------------------------------------
df <- data.frame(totalKM = 38.9,mainUse=0,landShiftArea=9.35,age=21)

predict(mlm_select, newdata = df)

#======================================================================================
# Step 4 : linear model check
#======================================================================================

#-----------------------------------------
# Model diagnosis
#-----------------------------------------
library(ggfortify)
autoplot(mlm_select)

#-----------------------------------------
# test & assumption
#-----------------------------------------
# normality test
shapiro.test(mlm_select$residual)
# independence assumption
require(car)
durbinWatsonTest(mlm_select) 
# homogeneity of variance assumption
require(car)
ncvTest(mlm_select)