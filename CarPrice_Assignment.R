##Linear Regression Assignment
library(stringr)
library(dplyr)
library(car)
library(MASS)
options(max.print=100000)

##Read data
cars <- read.csv("CarPrice_Assignment.csv")
str(cars)

# lets check summary to get more idea
summary(cars)

##Data preparation
#Data cleaning

# check missing values
sum(is.na(cars)) # No missing values

# check duplicate values
duplicate_values <- cars[duplicated(cars),] # no duplicate values found

# car ID is variable is not of any use, so remove it
cars<- cars[,-1]

#R is case sensitive so for safety lets convert strings to lower format
cars$CarName <- tolower(cars$CarName)

# sperate car company name and assign it to the same
cars$CarName <- gsub("([A-Za-z]+).*", "\\1", cars$CarName)
View(cars)

#lets correct spelling of name
cars$CarName[which(cars$CarName == "maxda")] <- "mazda"
cars$CarName[which(cars$CarName == "porcshce")] <- "porsche"
cars$CarName[which(cars$CarName == "toyouta")] <- "toyota"
cars$CarName[which(cars$CarName == "vokswagen")] <- "volkswagen"
cars$CarName[which(cars$CarName == "vw")] <- "volkswagen"

#Change carname to factor type
cars$CarName <- as.factor(cars$CarName)
str(cars)

## lets treat the outliers

## check if there are outliers in the wheelbase variable
boxplot(cars$wheelbase)
quantile(cars$wheelbase, seq(0,1,0.01))

#look at values > 98 percentile, note that there is a jump from 98%. So, cap all values above 114.200 (98%) to 114.200
cars$wheelbase[which(cars$wheelbase > 114.200)] <- 114.200
## check if there are outliers in the carlength variable
boxplot(cars$carlength)
quantile(cars$carlength, seq(0,1,0.01))
#look at values < 4 percentile, note that there is a jump between 0-4%. So, floor all values below 155.900 (3%) to 155.900
cars$carlength[which(cars$carlength < 155.900)] <- 155.900

## check if there are outliers in the carwidth variable
boxplot(cars$carwidth)
quantile(cars$carwidth, seq(0,1,0.01))
#look at values > 96 percentile, note that there is a jump from 96%. So, cap all values above 70.852 (96%) to 70.852
cars$carwidth[which(cars$carwidth > 70.852)] <- 70.852

##check if there are outliers in the carheight variable
boxplot(cars$carheight)
quantile(cars$carheight, seq(0,1,0.01)) # car height dont have outliers


##check if there are outliers in the curbweight variable
boxplot(cars$curbweight)
quantile(cars$curbweight, seq(0,1,0.01)) # car height dont have outliers

##check if there are outliers in the engine size variable
boxplot(cars$enginesize)
quantile(cars$enginesize, seq(0,1,0.01))
#look at values > 95 percentile, note that there is a jump from 95%. So, cap all values above 201.20 (95%) to 201.20
cars$enginesize[which(cars$enginesize > 201.20)] <- 201.20

##check if there are outliers in the boreratio variable
boxplot(cars$boreratio)
quantile(cars$boreratio, seq(0,1,0.01)) # car height dont have outliers

##check if there are outliers in the stroke variable
boxplot(cars$stroke)
quantile(cars$stroke, seq(0,1,0.01))
#look at values < 2 percentile, note that there is a jump from 0-8%, floor all values below 2.705600(%) to 2.705600
cars$stroke[which(cars$stroke < 2.705600)] <- 2.705600
#look at value > 96, note that there is a jump from 96%, so cap all values above 3.8248 (96%) to 3.8248
cars$stroke[which(cars$stroke > 3.8248)] <- 3.8248

##check if there are outliers in the compression ratio variable
boxplot(cars$compressionratio)
quantile(cars$compressionratio, seq(0,1,0.01))
#look at value > 89, note that there is a jump from 89%, so cap all values above 10.0000 (89%) to 10.0000
cars$compressionratio[which(cars$compressionratio > 10.0000)] <- 10.0000
#look at values < 4 percentile, note that there is a jump from 0-4%, floor all values below 7.5000(4%) to 7.5000
cars$compressionratio[which(cars$compressionratio < 7.5000)] <- 7.5000

##check if there are outliers in the horsepower variable
boxplot(cars$horsepower)
quantile(cars$horsepower, seq(0,1,0.01))
#look at value > 93, note that there is a jump from 93%, so cap all values above 162.00 (93%) to 162.00
cars$horsepower[which(cars$horsepower > 162.00)] <- 162.00

##check if there are outliers in the peakrpm variable
boxplot(cars$peakrpm)
quantile(cars$peakrpm, seq(0,1,0.01))
#look at value > 99, note that there is a jump from 99%, so cap all values above 6000  (99%) to 6000
cars$peakrpm[which(cars$peakrpm > 6000)] <- 6000

##check if there are outliers in the citympg variable
boxplot(cars$citympg)
quantile(cars$citympg, seq(0,1,0.01))
#look at value > 98, note that there is a jump from 98%, so cap all values above 38.00  (98%) to 38.00
cars$citympg[which(cars$citympg > 38.00)] <- 38.00

##check if there are outliers in the highwaympg variable
boxplot(cars$highwaympg)
quantile(cars$highwaympg, seq(0,1,0.01))
#look at value > 98, note that there is a jump from 98%, so cap all values above 38.00  (98%) to 38.00
cars$highwaympg[which(cars$highwaympg > 46.92)] <- 46.92

#Lets check the display structure for variables
str(cars)

## Here varaables with levels
# CarName: 22 levels, fueltype: 2 levels, aspiration: 2 levels, doornumber: 2 levels, carbody: 5 levels
# drivewheel: 3 levels, enginelocation: 2 levels, enginetype: 7 levels, cylindernumber: 7 levels, fuelsystem: 8 levels

## create dummy variables to convert the categorical variable carname to numerical
dummy_carname <- data.frame(model.matrix(~ CarName, data=cars))

# a factor with n levels, n-1 dummy variables are required, So, in this case, remove the first column from dummy
dummy_carname <- dummy_carname[,-1]

#Now cbind the dummy variable with the cars dataset. Remember to remove the variable carname
cars_1 <- cbind(cars[,-2], dummy_carname)

# view how the data set is
View(cars_1)

## create dummy variables to convert the categorical variable fuel type to numerical
dummy_fueltype <- data.frame(model.matrix(~ cars_1$fueltype, data=cars_1))

# a factor with n levels, n-1 dummy variables are required, So, in this case, remove the first column from dummy
dummy_fueltype <- dummy_fueltype[,-1]

#Now cbind the dummy variable with the cars dataset. Remember to remove the variable fuel type
cars_2 <- cbind(cars_1[,-2], dummy_fueltype)

# view how the data set is
View(cars_2)

## create dummy variables to convert the categorical variable aspiration to numerical
dummy_aspiration <- data.frame(model.matrix(~ cars_2$aspiration, data=cars_2))

# a factor with n levels, n-1 dummy variables are required, So, in this case, remove the first column from dummy
dummy_aspiration <- dummy_aspiration[,-1]

#Now cbind the dummy variable with the cars dataset. Remember to remove the variable aspiration
cars_3 <- cbind(cars_2[,-2], dummy_aspiration)

# view how the data set is
View(cars_3)

## create dummy variables to convert the categorical variable door number to numerical
dummy_doornumber <- data.frame(model.matrix(~ cars_3$doornumber, data=cars_3))

# a factor with n levels, n-1 dummy variables are required, So, in this case, remove the first column from dummy
dummy_doornumber <- dummy_doornumber[,-1]

#Now cbind the dummy variable with the cars dataset. Remember to remove the variable doornumber
cars_4 <- cbind(cars_3[,-2], dummy_doornumber)

# view how the data set is
View(cars_4)

## create dummy variables to convert the categorical variable carbody to numerical
dummy_carbody <- data.frame(model.matrix(~ cars_4$carbody, data=cars_4))

# a factor with n levels, n-1 dummy variables are required, So, in this case, remove the first column from dummy
dummy_carbody <- dummy_carbody[,-1]

#Now cbind the dummy variable with the cars dataset. Remember to remove the variable carbody
cars_5 <- cbind(cars_4[,-2], dummy_carbody)

# view how the data set is
View(cars_5)

## create dummy variables to convert the categorical variable drive wheel to numerical
dummy_drivewheel <- data.frame(model.matrix(~ cars_5$drivewheel, data=cars_5))

# a factor with n levels, n-1 dummy variables are required, So, in this case, remove the first column from dummy
dummy_drivewheel <- dummy_drivewheel[,-1]

#Now cbind the dummy variable with the cars dataset. Remember to remove the variable drive wheel
cars_6 <- cbind(cars_5[,-2], dummy_drivewheel)

# view how the data set is
View(cars_6)

## create dummy variables to convert the categorical variable engine location to numerical
dummy_enginelocation <- data.frame(model.matrix(~ cars_6$enginelocation, data=cars_6))

# a factor with n levels, n-1 dummy variables are required, So, in this case, remove the first column from dummy
dummy_enginelocation <- dummy_enginelocation[,-1]

#Now cbind the dummy variable with the cars dataset. Remember to remove the variable engine location
cars_7 <- cbind(cars_6[,-2], dummy_enginelocation)

# view how the data set is
View(cars_7)

## create dummy variables to convert the categorical variable engine type to numerical
dummy_enginetype <- data.frame(model.matrix(~ cars_7$enginetype, data=cars_7))

# a factor with n levels, n-1 dummy variables are required, So, in this case, remove the first column from dummy
dummy_enginetype <- dummy_enginetype[,-1]

#Now cbind the dummy variable with the cars dataset. Remember to remove the variable engine type
cars_8 <- cbind(cars_7[,-7], dummy_enginetype)

# view how the data set is
View(cars_8)

## create dummy variables to convert the categorical variable cylinder number to numerical
dummy_cylindernumber <- data.frame(model.matrix(~ cars_8$cylindernumber, data=cars_8))

# a factor with n levels, n-1 dummy variables are required, So, in this case, remove the first column from dummy
dummy_cylindernumber <- dummy_cylindernumber[,-1]

#Now cbind the dummy variable with the cars dataset. Remember to remove the variable cylinder number
cars_9 <- cbind(cars_8[,-7], dummy_cylindernumber)

# view how the data set is
View(cars_9)

## create dummy variables to convert the categorical variable fuel system to numerical
dummy_fuelsystem <- data.frame(model.matrix(~ cars_9$fuelsystem, data=cars_9))

# a factor with n levels, n-1 dummy variables are required, So, in this case, remove the first column from dummy
dummy_fuelsystem <- dummy_fuelsystem[,-1]

#Now cbind the dummy variable with the cars dataset. Remember to remove the variable fuel system
cars_10 <- cbind(cars_9[,-8], dummy_fuelsystem)

# view how the data set is
View(cars_10)
str(cars_10)

---------------###############--------------

## linear regression model

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices = sample(1:nrow(cars_10), 0.7*nrow(cars_10))

# generate the train data set
train = cars_10[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = cars_10[-trainindices,]

#####
#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)

# Check the summary of model. 
summary(model_1)

#Notice that there are many insignificant variables in the model
#we will use step-wise function to remove the extremely insignificant variables in the beginning itself

step <- stepAIC(model_1, direction = "both")

# now we need to know our model equation so lets write the Step command here
step

# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2

model_2 <- lm(formula = price ~ carlength + carwidth + curbweight + enginesize + 
                compressionratio + horsepower + peakrpm + CarNamebmw + CarNamebuick + 
                CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + dummy_aspiration + 
                cars_4.carbodyhatchback + cars_4.carbodysedan + cars_4.carbodywagon + 
                cars_5.drivewheelfwd + dummy_enginelocation + cars_7.enginetypeohc + 
                cars_7.enginetyperotor + cars_8.cylindernumberfive + cars_8.cylindernumberfour + 
                cars_8.cylindernumbersix + cars_9.fuelsystem2bbl + cars_9.fuelsystemmpfi, 
              data = train)

#check the summary of model_2
summary(model_2)

#Let us check for multicollinearity 
# If the VIF is above 2 as the business goal says, we would remove 
# the variables if they are statistically insignificant
vif(model_2)

#Also check the summary of model_2 to compare VIF and significance of the variables simultaneously
summary(model_2)

#Note that the variables curbweights and engine size have the highest VIFs, 
#but engine size cannot be removed since these are highly significant

#So, the variable with next highest VIF is curbweight. It is insignificant as well. 
#Hence, curbweight can be removed from the model.

#Now, build a new model by removing curbweight variable
model_3 <- lm(formula = price ~ carlength + carwidth + enginesize + 
                compressionratio + horsepower + peakrpm + CarNamebmw + CarNamebuick + 
                CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + dummy_aspiration + 
                cars_4.carbodyhatchback + cars_4.carbodysedan + cars_4.carbodywagon + 
                cars_5.drivewheelfwd + dummy_enginelocation + cars_7.enginetypeohc + 
                cars_7.enginetyperotor + cars_8.cylindernumberfive + cars_8.cylindernumberfour + 
                cars_8.cylindernumbersix + cars_9.fuelsystem2bbl + cars_9.fuelsystemmpfi, 
              data = train)

#Again, apply VIF on model_3 to check if there are variables with VIF>2
vif(model_3)

#Also check the summary of model_3 to compare VIF and significance of the variables simultaneously
summary(model_3)

#Note that the variables egine size, cylinder number four, horse power, car width, car length and
#cylinder number six, engine type ohc have the highest VIFs, 
#but cannot be removed since these are highly significant

#So, the variable with next highest VIF is fuelsystemmpfi. It is insignificant as well. 
#Hence, fuelsystemmpfi can be removed from the model

#Now, build a new model by removing fuelsystemmpfi variable
model_4 <- lm(formula = price ~ carlength + carwidth + enginesize + 
                compressionratio + horsepower + peakrpm + CarNamebmw + CarNamebuick + 
                CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + dummy_aspiration + 
                cars_4.carbodyhatchback + cars_4.carbodysedan + cars_4.carbodywagon + 
                cars_5.drivewheelfwd + dummy_enginelocation + cars_7.enginetypeohc + 
                cars_7.enginetyperotor + cars_8.cylindernumberfive + cars_8.cylindernumberfour + 
                cars_8.cylindernumbersix + cars_9.fuelsystem2bbl, data = train)

#Again, apply VIF on model_3 to check if there are variables with VIF>2
vif(model_4)

#Also check the summary of model_4 to compare VIF and significance of the variables simultaneously
summary(model_4)

#Also, notice that the variables engine size and cylinder number four still have the highest VIFs with very highsignificance since the beginning. 
#So, it will be a good idea to check their correlation as they might be highly correlated.

#Check the correlation of the variables engine size and cylinder number four
cor(train$enginesize, train$cars_8.cylindernumberfour)

#Note that the correlation is ~ 64%, indicating that the variables are highly correlated. 
#So, remove the variable with lower significance level out of the two which is engine size

#build new model
model_5 <- lm(formula = price ~ carlength + carwidth + compressionratio + horsepower + 
                peakrpm + CarNamebmw + CarNamebuick + 
                CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + dummy_aspiration + 
                cars_4.carbodyhatchback + cars_4.carbodysedan + cars_4.carbodywagon + 
                cars_5.drivewheelfwd + dummy_enginelocation + cars_7.enginetypeohc + 
                cars_7.enginetyperotor + cars_8.cylindernumberfive + cars_8.cylindernumberfour + 
                cars_8.cylindernumbersix + cars_9.fuelsystem2bbl, data = train)

#check the summary of model_5 to see how much removal of engine size affect R2
summary(model_5)

#Now check the vif of model_5
vif(model_5)

#Note that the variable cylinder number four has VIF very high, but also has very high significance.  
#So, we will have to check thebimpact of the removal of this variable on the model. So, let's remove it

model_6 <- lm(formula = price ~ carlength + carwidth + compressionratio + horsepower + 
                peakrpm + CarNamebmw + CarNamebuick + 
                CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + 
                CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + dummy_aspiration + 
                cars_4.carbodyhatchback + cars_4.carbodysedan + cars_4.carbodywagon + 
                cars_5.drivewheelfwd + dummy_enginelocation + cars_7.enginetypeohc + 
                cars_7.enginetyperotor + cars_8.cylindernumberfive + cars_8.cylindernumbersix + 
                cars_9.fuelsystem2bbl, data = train)

#check the summary of model_6
summary(model_6)

#Notice that the Adjusted R-squared value has nor affect too much so it is good t remove.

#Again check the vif of model_6
vif(model_6)

# the variable with next highest VIF is horsepower. It is insignificant as well. 
#Hence, horsepower can be removed from the model

model_7 <- lm(formula = price ~ carlength + carwidth + compressionratio + peakrpm + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + dummy_aspiration + 
                cars_4.carbodyhatchback + cars_4.carbodysedan + cars_4.carbodywagon + 
                cars_5.drivewheelfwd + dummy_enginelocation + cars_7.enginetypeohc + 
                cars_7.enginetyperotor + cars_8.cylindernumberfive + cars_8.cylindernumbersix + 
                cars_9.fuelsystem2bbl, data = train)

#Again check the vif of model_7
vif(model_7)

#check the summary of model_7
summary(model_7)

#the variable with next highest VIF is car length. It is insignificant as well. 
#Hence, car length can be removed from the model

model_8 <- lm(formula = price ~ carwidth + compressionratio + peakrpm + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + CarNamesaab + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + dummy_aspiration + 
                cars_4.carbodyhatchback + cars_4.carbodysedan + cars_4.carbodywagon + 
                cars_5.drivewheelfwd + dummy_enginelocation + cars_7.enginetypeohc + 
                cars_7.enginetyperotor + cars_8.cylindernumberfive + cars_8.cylindernumbersix + 
                cars_9.fuelsystem2bbl, data = train)

#Again check the vif of model_8
vif(model_8)

#check the summary of model_8
summary(model_8)

#Note that the variables engine type ohc has highest VIF but it is highli significant, we cant roomove it

#the variable with next highest VIF is carbodyhatchback. It is insignificant as well. 
#Hence, carbodyhatchback can be removed from the model

model_9 <- lm(formula = price ~ carwidth + compressionratio + peakrpm + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + dummy_aspiration + cars_4.carbodysedan + 
                cars_4.carbodywagon + cars_5.drivewheelfwd + dummy_enginelocation + 
                cars_7.enginetypeohc + cars_7.enginetyperotor + cars_8.cylindernumberfive + 
                cars_8.cylindernumbersix + cars_9.fuelsystem2bbl, data = train)

#Again check the vif of model_9
vif(model_9)

#check the summary of model_9
summary(model_9)

#Note that the variables engine type ohc and carwidth has highest VIF but it is highli significant, we cant roomove it

#the variable with next highest VIF is cylinder number six. It is insignificant as well. 
#Hence, cylinder number six can be removed from the model

model_10 <- lm(formula = price ~ carwidth + compressionratio + peakrpm + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                 CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNamepeugeot + CarNameplymouth + CarNamerenault + CarNamesaab + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + dummy_aspiration + cars_4.carbodysedan + 
                 cars_4.carbodywagon + cars_5.drivewheelfwd + dummy_enginelocation + 
                 cars_7.enginetypeohc + cars_7.enginetyperotor + cars_8.cylindernumberfive + 
                 cars_9.fuelsystem2bbl, data = train)

#Again check the vif of model_10
vif(model_10)

#check the summary of model_10
summary(model_10)

#Note that the variables engine type ohc and carwidth has highest VIF but it is highli significant, we cant roomove it

#the variable with next highest VIF is drivewheelfwd. It is insignificant as well. 
#Hence, drivewheelfwd can be removed from the model

model_11 <- lm(formula = price ~ carwidth + compressionratio + peakrpm + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                 CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNamepeugeot + CarNameplymouth + CarNamerenault + CarNamesaab + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + dummy_aspiration + cars_4.carbodysedan + 
                 cars_4.carbodywagon + dummy_enginelocation + cars_7.enginetypeohc + 
                 cars_7.enginetyperotor + cars_8.cylindernumberfive + cars_9.fuelsystem2bbl, data = train)

#Again check the vif of model_11
vif(model_11)

#check the summary of model_11
summary(model_11)

##the variable with next highest VIF is peakrpm. It is insignificant as well. 
#Hence, peakrpm can be removed from the model

model_12 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNamerenault + CarNamesaab + CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                 dummy_aspiration + cars_4.carbodysedan + cars_4.carbodywagon + dummy_enginelocation + 
                 cars_7.enginetypeohc + cars_7.enginetyperotor + cars_8.cylindernumberfive + 
                 cars_9.fuelsystem2bbl, data = train)

#Again check the vif of model_12
vif(model_12)

#check the summary of model_12
summary(model_12)

##the variable with next highest VIF is fuel syste2bbl. It is insignificant as well. 
#Hence, fuelsystem2bbl can be removed from the model

model_13 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNamerenault + CarNamesaab + CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                 dummy_aspiration + cars_4.carbodysedan + cars_4.carbodywagon + dummy_enginelocation + 
                 cars_7.enginetypeohc + cars_7.enginetyperotor + cars_8.cylindernumberfive, data = train)

#Again check the vif of model_13
vif(model_13)

#check the summary of model_13
summary(model_13)

#Also, notice that the variables carwidth and enginetypeohc still have the highest VIFs with very highsignificance since the beginning. 
#So, it will be a good idea to check their correlation as they might be highly correlated.

#Check the correlation of the variables engine size and cylinder number four
cor(train$carwidth, train$cars_7.enginetypeohc)
# it is not higly correlated

# lest check correlation for carwidth and carnametoyota
cor(train$carwidth, train$CarNametoyota)
# it is not higly correlated

#Note that the variable carwidth and carnametoyota has VIF very high, but also has very high significance.  
#lets remove carnametoyota to check thebimpact of the removal of this variable on the model. So, let's remove it

model_14 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNamerenault + CarNamesaab + CarNamesubaru + CarNamevolkswagen + 
                 dummy_aspiration + cars_4.carbodysedan + cars_4.carbodywagon + dummy_enginelocation + 
                 cars_7.enginetypeohc + cars_7.enginetyperotor + cars_8.cylindernumberfive, data = train)

#Again check the vif of model_14
vif(model_14)

#check the summary of model_14
summary(model_14)

#Notice that the Adjusted R-squared value has not affect too much so it is good to remove.

#Note that the variable carwidth and enginetypeohc has VIF very high, but also has very high significance.  
#lets remove carwidth to check the impact of the removal of this variable on the model. So, let's remove it

model_15 <- lm(formula = price ~ compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNamerenault + CarNamesaab + CarNamesubaru + CarNamevolkswagen + 
                 dummy_aspiration + cars_4.carbodysedan + cars_4.carbodywagon + dummy_enginelocation + 
                 cars_7.enginetypeohc + cars_7.enginetyperotor + cars_8.cylindernumberfive, data = train)

#Again check the vif of model_15
vif(model_15)

#check the summary of model_15
summary(model_15)

#Notice that the Adjusted R-squared value has decrease abruptly
#This is because carwidth is highly significant.So, we cannot remove this variable

#Again check the vif of model_14
vif(model_14)

#Note that the variable enginetypeohc has VIF very high, but also has very high significance.  
#lets remove enginetypeohc to check the impact of the removal of this variable on the model. So, let's remove it

model_16 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNamerenault + CarNamesaab + CarNamesubaru + CarNamevolkswagen + 
                 dummy_aspiration + cars_4.carbodysedan + cars_4.carbodywagon + dummy_enginelocation + 
                 cars_7.enginetyperotor + cars_8.cylindernumberfive, data = train)

#check the summary of model_16
summary(model_16)

#Again check the vif of model_16
vif(model_16)

#Notice that the Adjusted R-squared value has not affect too much so it is good to remove.

#Here VIF for carwidth is 2.56591 which is above 2 but stll good as not exceeding 4
#And all othere variable are have VIF < 2

#So, now we will remove variables on the basis of p-values (or significance)

#check the summary of model_16
summary(model_16)

# The variable aspiration has highest p-value or lowest significance, so remove it

model_17 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNamerenault + CarNamesaab + CarNamesubaru + CarNamevolkswagen + cars_4.carbodysedan + 
                 cars_4.carbodywagon + dummy_enginelocation + cars_7.enginetyperotor + 
                 cars_8.cylindernumberfive, data = train)

#check the summary of model_17
summary(model_17)

# The variable carbodysedan has highest p-value or lowest significance, so remove it

model_18 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNamerenault + CarNamesaab + CarNamesubaru + CarNamevolkswagen + 
                 cars_4.carbodywagon + dummy_enginelocation + cars_7.enginetyperotor + 
                 cars_8.cylindernumberfive, data = train)

#check the summary of model_18
summary(model_18)

# The variable carbodywagon has highest p-value or lowest significance, so remove it

model_19 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                 CarNamemazda + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNamerenault + CarNamesaab + CarNamesubaru + CarNamevolkswagen + 
                 dummy_enginelocation + cars_7.enginetyperotor + 
                 cars_8.cylindernumberfive, data = train)

#check the summary of model_19
summary(model_19)

# The variable carnamenisan has highest p-value or lowest significance, so remove it

model_20 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamedodge + CarNamehonda + CarNameisuzu + CarNamejaguar + 
                 CarNamemazda + CarNamemitsubishi + CarNamepeugeot + CarNameplymouth + 
                 CarNamerenault + CarNamesaab + CarNamesubaru + CarNamevolkswagen + 
                 dummy_enginelocation + cars_7.enginetyperotor + 
                 cars_8.cylindernumberfive, data = train)

#check the summary of model_20
summary(model_20)

# The variable carnamehonda has highest p-value or lowest significance, so remove it

model_21 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamedodge + CarNameisuzu + CarNamejaguar + 
                 CarNamemazda + CarNamemitsubishi + CarNamepeugeot + CarNameplymouth + 
                 CarNamerenault + CarNamesaab + CarNamesubaru + CarNamevolkswagen + 
                 dummy_enginelocation + cars_7.enginetyperotor + 
                 cars_8.cylindernumberfive, data = train)

#check the summary of model_21
summary(model_21)

# The variable carnameisuzu has highest p-value or lowest significance, so remove it

model_22 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
                 CarNamepeugeot + CarNameplymouth + CarNamerenault + CarNamesaab + CarNamesubaru + 
                 CarNamevolkswagen + dummy_enginelocation + cars_7.enginetyperotor + 
                 cars_8.cylindernumberfive, data = train)

#check the summary of model_22
summary(model_22)

# The variable CarNamevolkswagen has highest p-value or lowest significance, so remove it

model_23 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
                 CarNamepeugeot + CarNameplymouth + CarNamerenault + CarNamesaab + CarNamesubaru + 
                 dummy_enginelocation + cars_7.enginetyperotor + 
                 cars_8.cylindernumberfive, data = train)

#check the summary of model_23
summary(model_23)

# The variable CarNameplymouth has highest p-value or lowest significance, so remove it

model_24 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamedodge + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
                 CarNamepeugeot + CarNamerenault + CarNamesaab + CarNamesubaru + 
                 dummy_enginelocation + cars_7.enginetyperotor + 
                 cars_8.cylindernumberfive, data = train)

#check the summary of model_24
summary(model_24)

# The variable CarNamedodge has highest p-value or lowest significance, so remove it

model_25 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamepeugeot + 
                 CarNamerenault + CarNamesaab + CarNamesubaru + dummy_enginelocation + 
                 cars_7.enginetyperotor + cars_8.cylindernumberfive, data = train)

#check the summary of model_25
summary(model_25)

# The variable CarNamedodge has highest p-value or lowest significance, so remove it

model_26 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamepeugeot + 
                 CarNamerenault + CarNamesubaru + dummy_enginelocation + 
                 cars_7.enginetyperotor + cars_8.cylindernumberfive, data = train)

#check the summary of model_26
summary(model_26)

# Although, all variables have a p value below 0.05, the number of variables is still too large.
# You could continue removing the variables till the significance level < 0.001

# remove CarNamesubaru

model_27 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamepeugeot + 
                 CarNamerenault + dummy_enginelocation + 
                 cars_7.enginetyperotor + cars_8.cylindernumberfive, data = train)

#check the summary of model_27
summary(model_27)

# remove enginetype rotor

model_28 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamejaguar + CarNamemazda + CarNamemitsubishi + CarNamepeugeot + 
                 CarNamerenault + dummy_enginelocation + cars_8.cylindernumberfive, data = train)

#check the summary of model_28
summary(model_28)

# remove CarNamemazda

model_29 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamejaguar + CarNamemitsubishi + CarNamepeugeot + 
                 CarNamerenault + dummy_enginelocation + cars_8.cylindernumberfive, data = train)

#check the summary of model_29
summary(model_29)

# remove CarNamerenault

model_30 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamejaguar + CarNamemitsubishi + CarNamepeugeot + 
                 dummy_enginelocation + cars_8.cylindernumberfive, data = train)

#check the summary of model_30
summary(model_30)

# remove CarNamemitsubishi

model_31 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamejaguar + CarNamepeugeot + 
                 dummy_enginelocation + cars_8.cylindernumberfive, data = train)

#check the summary of model_31
summary(model_31)

#  remove CarNamepeugeot

model_32 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamejaguar + dummy_enginelocation + cars_8.cylindernumberfive, data = train)

#check the summary of model_32
summary(model_32)

# remove cars_8.cylindernumberfive

model_33 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamechevrolet + CarNamejaguar + dummy_enginelocation, data = train)

#check the summary of model_33
summary(model_33)

# remove CarNamechevrolet

model_34 <- lm(formula = price ~ carwidth + compressionratio + CarNamebmw + CarNamebuick + 
                 CarNamejaguar + dummy_enginelocation, data = train)

#check the summary of model_34
summary(model_34)

# remove compressionratio

model_35 <- lm(formula = price ~ carwidth + CarNamebmw + CarNamebuick + 
                 CarNamejaguar + dummy_enginelocation, data = train)

#check the summary of model_35
summary(model_35)

##Now the model has 5 variables all of which are significant. So, model_35 is the final model
#1. carwidth 2. CarNamebmw 3. CarNamebuick 4. CarNamejaguar 5. dummy_enginelocation

#Note that the value of Adjusted R-squared is ~ 87%
#Now, let us move forward to test the model on test data

# predicting the results in test dataset
Predict_1 <- predict(model_35,test[,-15])
test$test_price <- Predict_1

#In order to check how accurate are the predictions of the model,
#we need to find the r-squared value between the predicted and actual values of price

# Now, we need to test the r square between actual and predicted price
rsquared <- cor(test$price,test$test_price)^2
rsquared

----------------#################---------------------

##Let us stop here and analyze the model we have built

#the r-squared value for the model is 87% 
#while the r-squared value for the test dataset is 82%

----------------################----------------------


