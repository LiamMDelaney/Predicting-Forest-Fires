library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
set.seed(0)


#loading and exploring data
ff = read.delim('forestfires.tsv', header = TRUE, sep = '\t')
forest_fires_unprocessed = read.delim('forestfires.tsv', header = TRUE, sep = '\t')
head(ff)
tail(ff)
str(ff)
summary(ff)

#changing month and day to factors
ff$month = as.factor(ff$month)
ff$day = as.factor(ff$day)
str(ff)

#viewing the Burn Area as is
ggplot(ff, aes(x = area)) + geom_histogram()

#as you can see the burn area is heavily right skewed. 

#transforming burn area to log to fix the right skew in area. I use area + 1
#to get rid of the large number of 0s in the data
ggplot(ff, aes(x = area + 1)) +geom_histogram() + scale_x_log10()

#adding labels and break points
ggplot(ff, aes(x = area + 1)) +
  geom_histogram() + 
  scale_x_log10('Burn Area (log10)',
                breaks = c(1, 10, 100, 1000))

ff$log_area = log10(ff$area + 1)

#EDA (Exploratory Data Analysis)
#Creating a new variable for the X and Y coordinates to explore
burn_coord = 
  ff %>%
    group_by(X, Y) %>%
    summarise(area_mean = mean(area),
              area_sd = sd(area),
              n = n())

burn_coord

#Examining burn_coord using log_area
burn_coord_log_area = 
  ff %>%
  group_by(X, Y) %>%
  summarise(log_area_mean = mean(log_area),
            log_area_sd = sd(log_area),
            n = n())

#We create the burn_coord table so that we can see the average burn area
#for each pair of coordinates in the park. But there is a problem.
#we include the n=n() function to get the number of observations at each
#coordinate pairing and we can see that out of 36 variables, there
#are several that only have a few, less than 10, observations. We will
#plot to try and get a better picture of if the data is skewed and if this will
#make for a good way to predict forest fires.

#plotting burn_coord
ggplot(burn_coord, aes(x = factor(X), y = factor(Y), 
                       fill = area_mean)) +
  geom_tile() + 
  scale_fill_gradient2()


ggplot(burn_coord_log_area, aes(x = factor(X), y = factor(Y), 
                       fill = log_area_mean)) +
  geom_tile() + 
  scale_fill_gradient2()

#We can see from the tile chart that the upper right corner seems to be 
#particularly susceptible to forest fires, especially the tile at 8,8.
#However, looking at the chart and examining the number of observations
#located at X = 6, 7, 8, and 9, we see that while the burn areas are large
#the averages are based almost entirely on numbers of observations that are
#less than 10. Areas X = 1, 2, 3, 4, and 5 have much more observations
#meaning that even though those fires have smaller areas the averages
#are based on many more observations and can therefore be considered more 
#accurate. 
#Comparing our first tile plot (using only the area) to a second
#tile plot (using log area) gives us perhaps a clearer picture of this dilemma.
#The colors in the plot are more vibrant and the areas are clearer, but the
#upper right corner is still tracking larger fires based on fewer observations.

#Because our data seems to have more data when predicting smaller fires
#we may want to examine other ways of predicting forest fires, such as 
#date, day, and weather conditions. We will begin to do that now

#facets by month
ggplot(ff, aes(x = RH, y = log_area)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~month)

#changing order of month and day
ff$month = factor(ff$month, 
                  level = c('jan', 'feb', 'mar', 'apr', 'may',
                            'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))

#By using a facet wrap we can examine the seasonality of forest fires.
#We have also mapped trend lines onto the charts so that we can get a basic
#idea of if there is a negative correlation or a positive correlation
#in the data and to see any kind of linearity to the data. 
#We can also see the amount of observations, or at least an approximate amount,
#of observations fr each month, and it is clear that the majority
#of our observations come from August and September.

#grouping burn coords and month
burn_coord_season = 
  ff %>%
  group_by(month) %>%
  summarise(log_area_mean = mean(log_area),
            log_area_sd = sd(log_area),
            n = n())
burn_coord_season

#creating another variable that only tracks burn area by month shows us that
#indeed seasonality plays a larger role than the X and Y coordinates, although
#August and September have the greatest number of observations in the data set

#exploring the weather features with ggpairs
ggpairs(select(ff, temp, RH, wind, log_area))

#Rather than run several different charts to examine the relationship between
#the weather features and the burn area, I have run a ggpairs plot to quickly
#examine the correlations between these features and the burn area.
#As we can see, none have a particularly strong correlation with log_area.
#I have left rain out of my ggpairs plot because in 517 observations
#total in this dataset, there are 509 0 values in rain.
table(ff$rain)

#changing coords to factors, because they represent spacial locations
#and are therefore discreet numerical type not continuous and therefore
#in order to make ML modeling and generating plots easier X and Y have been
#turned into factors
ff$X = factor(ff$X)
ff$Y = factor(ff$Y)
str(ff)

#We can assume that park attendance goes up during the weekend, and because
#the majority of forest fires are started by people we can assume that
#more fires occur during the weekend than during the week. 
#creating new feature is_weekend
ff$is_weekend = ifelse(ff$day %in% c('sat', 'sun'), 1, 0)
head(ff)
ff$is_weekend_fri = ifelse(ff$day %in% c('fri', 'sat', 'sun'), 1, 0)
head(ff)
#I am creating an additional is_weekend column that includes Friday 
#because many people leave to go camping on Fridays 
#and I want to see if that affects the burn area if friday is included in
#my analysis

ff$is_weekend = factor(ff$is_weekend)
ff$is_weekend_fri = factor(ff$is_weekend_fri)
str(ff)

#using ggpairs to analyze is_weekend
is_weekend_plot = select(ff, temp, RH, wind, log_area, is_weekend) %>% 
  filter(log_area >0)

is_weekend_fri_plot = select(ff, temp, RH, wind, log_area, is_weekend_fri) %>%
  filter(log_area > 0)

ggpairs(is_weekend_plot, mapping = aes(color = is_weekend, alpha = 0.50))

ggpairs(is_weekend_fri_plot, mapping = aes(color = is_weekend_fri, 
                                           alpha = 0.50))
#While the correlation does change when we add Friday into our definition
#of weekend, it does not change with enough significance to put be a 
#statistically significant feature

#create binary features
month = model.matrix(~ month - 1, data = ff)
day = model.matrix(~ day - 1, data = ff)
#Binarizing, or one-hot encoding is different from integer encoding(where I would turn the months
#into integers i.e. Jan = 1, Feb = 2, Mar = 3 etc) and I have chosen one-hot for this project
#because it removes any ordered relationship that the variables may have, which could affect
#the ML algorithms. I have also chosen to use one-hot because there is no such ordinal relationship
#to months and days of the week, and the same goes for coordinates, as they are fixed locations
#in the park, namely they are discrete numerical values, not continuous.

X = model.matrix(~ X - 1, data = ff)
Y = model.matrix(~ Y - 1, data = ff)

ff = cbind(ff, month, day, X, Y)

ff = select(ff, -X, -Y, -month, -day, -area)
#I remove the X, Y, month, day, and area variables from the dataset as I am
#no longer using them and have converted all of these variables to other 
#forms


#splitting data into train and test
in_train = createDataPartition(y = ff$log_area, 
                               p = 0.8, 
                               list = FALSE)

ff_train = ff[in_train, ]
ff_test = ff[-in_train, ]
set.seed(0)
#Here I am splitting my data into a testing set and a training set.

#preprocessing
#centering and scaling dataset
#I center and scale the data because the data started out as somewhat skewed.
#Despite the feature engineering that I did above and changing area(which was
#heavily skewed) to log area, I want to make sure that my data is as close to
#a normal standard distribution as possible. In addition, because I have
#features that use different units (temperature, wind speed,
#relative humidity etc), centering and scaling the data will insure that these
#features are using essentially the same units.
preProcess_steps = preProcess(select(ff, FFMC, DMC, DC, ISI, temp, RH, 
                                     wind, rain),
                              method = c('center', 'scale'))

#applying preprocessing steps to the train and test sets
ff_train_proc = predict(preProcess_steps, newdata = ff_train)

ff_test_proc = predict(preProcess_steps, newdata = ff_test)

#looking at the results to confirm that it worked the way I wanted it to
head(ff_train_proc)
head(ff_test_proc)

#removing near 0 variance
#I examine the dataset to see how many of my features have no variance(all 
#values are identical) and how many have near zero variance. We remove these
#features, or at least control for them, so that they don't skew our data or
#cause errors in our ML testing, as features that have almost no variance
#are features that have very few observations. This can be because of several
#different reasons such as lack of surveillance equipment in the park, but
#because we are working to predict forest fires, it can also mean(in the case
#of coordinates) that there haven't historically been forest fires in those
#parts of the park, and so we can ignore those elements in our modeling.
nearZeroVar(ff_train_proc, saveMetrics = TRUE)

table(ff_train_proc$Y9)

#redoing the preprocessing step with the addition of the nzv function to 
#remove the features with near zero variance
preProcess_steps = preProcess(ff, method = c('center', 'scale', 'nzv'))

preProcess_steps

#regression. I start with a linear regression as my data is very linear and 
#this is a good place to start in order to get a basic idea of how my data
#will behave when I start to apply ML algorithms to it. I am filtering only
#for July for a similar reason, to test my linear model on a smaller subset
#of the data to see if there are any major anomalies that need to be dealt with
#before moving onto the full model. I also set a seed at this point so that
#my output remains the same if I have to run multiple tests
set.seed(0)
july_ff = filter(ff_train_proc, monthjul == 1)

model = lm(formula = log_area ~ DMC, data = july_ff)
attributes(model)

#get model output
summary(model)

#plotting the linear model to get a clearer view of the fit
ggplot(july_ff, aes(x = DMC, y= log_area)) +
  geom_point() +
  geom_abline(intercept = 0.4913, slope = 0.1824, color = 'red')
#The model at this stage fits the data reasonably well, as we can see from the
#residuals. The quartiles of the residuals range from -.7142 on the low
#end to 2.0145 at the high end and the median is -0.2068.
#The spread of the data also indicates some variability. The intercept's p-value being 
#.00171 indicates that it is statistically significant, as it is essentially 0, 
#however the p-value for the DMC is much closer to 1, indicating that DMC in this model does 
#not have a statistically significant impact on predicting wildfires. We see
#this as well in our plot of our model. The majority of the points are
#too far away from the line and indicate poor predictions.

#adding one additional variable to see how that affects the model
multi_model = lm(log_area ~ DMC + wind, data = ff_train_proc)
summary(multi_model)

#train function
model_fit = train(log_area ~ DMC + is_weekend,
                  data = ff_train_proc,
                  method = 'lm',
                  metric = 'RMSE')

model_fit

pred = predict(model_fit, newdata = ff_test_proc)

#view RMSE
postResample(pred = pred, obs = ff_test_proc$log_area)
#after fitting the model I used postResample to examine the RMSE.
#At this point the model has an overall error rate of around .6
errors = data.frame(predicted = pred,
                   observed = ff_test_proc$log_area,
                   error = pred - ff_test_proc$log_area)
head(errors)
tail(errors)


ggplot(data = errors, aes(x = predicted, y = observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = 'red')
#looking at a more detailed examination of the errors in this model by 
#creating an errors dataframe and then plotting the predicted points against
#the observed data shows even more clearly the limits of our model, or at 
#least the limits of our model when only taking into account DMC and 
#is_weekend. Now that we have an idea of the shortcomings of our model
#we can attempt to create a more accurate full model using all features
#which may also tell us if there is a better feature to use to predict 
#wildfires

#the full model
#Here I look at a general picture of training a linear model using all variables and
#features found in the dataset at once. 
full_model = train(log_area ~ ., 
                   data = ff_train_proc,
                   method = 'lm',
                   trControl = trainControl(method = 'cv', number = 10))

full_model
pred_full_model = predict(full_model, newdata = ff_test_proc)

errors_full_model = data.frame(predicted = pred_full_model,
                    observed = ff_test_proc$log_area,
                    error = pred_full_model - ff_test_proc$log_area)

ggplot(data = errors_full_model, aes(x = predicted, y = observed)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = 'red')
#Using the full model, the linear regression used 416 samples and 45 predictors.
#The RMSE indicates that the predictions are off by about .65 so at this point it's 
#not the most accurate model. Rerunning the errors code above and plotting the
#the predicted values against the observed values in the full model shows that
#while we are doing a little better, we are still not doing a great job predicting
#wildfires using this model


#forward selection model
set.seed(0)
forward_model = train(log_area ~ ., 
                      data = ff_train_proc,
                      method = 'leapForward',
                      tuneGrid = expand.grid(nvmax = 1:20),
                      trControl = trainControl(method = 'cv', number = 10))
forward_model
#in order to try and get a much better idea of what we should be using to predict
#forest fires we use forward selection which tells us that the best value to use
#for the model is 2. We can then run a few diagnostic plots to determine what 
#value 2 is

#diagnostic plots
plot(forward_model)
#generating a very simple plot shows us that 6 is the number of predictors that performed
#best in the model, but in this linear regression even this still has an error of 
#about .612. 

plot(varImp(forward_model))
#Running the variable importance plot shows us that temp, X3, and monthdec are the
#most important variables in predicting wildfires, at least in this model. Running
#the variable importance plot prevents the linear model from becoming a black box and
#makes our ML analysis more transparent. By knowing what the model considers to be
#important in making predictions we can focus on those areas, or try to understand,
#why those features and variables are important

#growing a single tree. I will start with a single tree for simplicity's sake,
#only to compare and see if I get any kind of improvement over the linear
#model. I am starting with a single tree to try and keep the load on my 
#computer low. As a random forest is a more complex algorithm to run, if I
#don't see an improvement in the accuracy of the model then there may not 
#be a reason to run a random forest.

#Before running a decision tree or a random forest we must add a column that
#indicates whether there was a forest fire or not. In order to do this I will
#use the same method as when I created the is_weekend column.


#when running decision tree, select everything except is_fire and log_area
ff$is_fire = ifelse(ff$log_area > 0, "Yes", "No")
#converting is_fire to factor so that I can run a confusion matrix in my 
#predict test
ff$is_fire = as.factor(ff$is_fire)

ff_tree_proc = preProcess(ff, method = c('center', 'scale', 'nzv'))
#I created a separate preprocessed data frame to use for the random forest
#testing to to separate out the work I did with linear regressions. I will
#also create a separate test and train set specific to the tree model.
set.seed(0)

tree_intrain = createDataPartition(y = ff$is_fire,
                                    p = 0.8,
                                    list = FALSE)

tree_train = ff[tree_intrain, ]
tree_test = ff[-tree_intrain, ]

ff_tree_model = train(y = tree_train$is_fire,
                      x = select(tree_train, -log_area, -is_fire),
                      method = 'rpart')

ff_tree_model

ff_tree_model$finalModel

library(rpart.plot)

rpart.plot(ff_tree_model$finalModel)

plot(varImp(ff_tree_model))

#The result of running a single tree gives us an accuracy of .554, Slightly 
#more accurate than a coin flip. Given this dataset the model is not much more accurate than
#randomly guessing. You can see how the algorithm is making its
#decisions in the decision tree plot, with X3 = 1 and RH>=72 being the most
#important factors in the tree. Next we will run a random forest test to see
#if that improves accuracy. Temp is the most important factor with DC and monthdec, and x3
#being high on  the list as well. It is very very important to set seed
#prior to running any testing algorithm, as I ran this algorithm previously
#but made a mistake that I needed to correct and reran without setting a seed
#and I got different results to my previous test.

tree_predict = predict(ff_tree_model, newdata = tree_test)
table(tree_predict)
confusionMatrix(tree_predict, tree_test$is_fire)
#running a confusion matrix shows that we have an accuracy of .534 with this
#model. The accuracy is very low with this model with the sensitivity being
#.12245 and the specificity being .90741. So it is a highly sensitive model
#Sensitivity indicates that the proportion of true positives is high and the 
#proportion of false negatives is low. A highly specific test indicates that 
#the proportion of true negatives is high while the proportion of false
#positives is low. So our test is very very good at indicating the absence
#of fire, but not very good at indicating if there is a fire. I have run this model
#and all of this code several times now and the accuracy, sensitivity and specificity
#are the same every time. Is this a sign of a good model/good data, or just what happens
#when you remember to set the seed?

#Because seasonality and coordinate seem to have a large bearing on where and when
#forest fires will occur, tuning this model may involve generating a model that
#focuses on these elements, rather than using area as the determining factor, as this
#only denotes the area of the burn. 
#bagged model

ff_tree_model_bagged = train(y = tree_train$is_fire,
                      x = select(tree_train, -log_area, -is_fire),
                      method = 'treebag')
ff_tree_model_bagged

#running a bagged, or random forest, only gets us a little bit more accuracy
#over running a single tree. Lastly I will run a neural network and see
#if I can get a little more accuracy out of my model.
#In order to run a neural network I must install the neural network package
#into R. 

library(neuralnet)
set.seed(0)

#creating a train and test set for neural network
nn_intrain = createDataPartition(y = ff$is_fire,
                                   p = 0.8,
                                   list = FALSE)

nn_train = ff[nn_intrain, ]
nn_test = ff[-nn_intrain, ]

#Using str to check my classes as I am getting errors using the neuralnet
#package. The neuralnet package doesn't work with columns that are factors
#so I change the is_weekend, is_weekend_fri, and is_fire columns to numeric
#so that I can run the neural network using the neural net package. 
str(nn_train)

nn_train$is_weekend = as.numeric(nn_train$is_weekend)
nn_train$is_weekend_fri = as.numeric(nn_train$is_weekend_fri)
nn_train$is_fire = as.numeric(nn_train$is_fire)

nn_test$is_weekend = as.numeric(nn_test$is_weekend)
nn_test$is_weekend_fri = as.numeric(nn_test$is_weekend_fri)
nn_test$is_fire = as.numeric(nn_test$is_fire)

ff_nn_formula = is_fire ~ .


ff_nn = neuralnet(ff_nn_formula, 
                  data = nn_train, 
                  hidden = c(5, 3),
                  linear.output = FALSE)

plot(ff_nn)

predictions = predict(ff_nn, nn_test[, !(names(nn_test) %in% c('is_fire'))])

predictions_vec = ifelse(predictions[,1] > 0.5, 1, 0)

confusionMatrix(as.factor(predictions_vec), as.factor(nn_test$is_fire))

#running a confusion matrix on the neural network gets us an accuracy of .4757, and a 
#sensitivity of 1 with a specificity of 0. Clearly there is something wrong with my weights,
#I suspect it has something to do with the is_fire column being judged only on the log_area
#being greater than 1. The neural network correctly identified the absence of fire 49 times
#and there were 54 false positives. There were 0 false negatives and 0 true positives. Bear in
#mind that in this confusion matrix 1 = No fire and 2 = Yes fire. So the neural net is doing
#a perfect job of identifying the absence of fire, just like in our tree model, and a terrible
#job of telling us where there is a fire. The sensitivity of 1 and specificity of 0 could 
#be a sign that the model is overfit and could be having trouble generalizing. I have some
#suspicions about why this is but the 2 biggest suspicions are that one-hot encoding led 
#to some of the overfitting problems where integer encoding or cyclic encoding 
#would have prevented some  of the overfitting. Also relying too much on the 
#burn area(the area and log_area columns) as the predictive value may have hurt the model's 
#ability to generalize about other factors causing wildfires, namely location and seasonality. 
#Were I to tune these models, I would not use one-hot encoding and, especially with my neural net, 
#I would try to not use burn area as the predictive value, perhaps doing away with the is_fire column entirely.

