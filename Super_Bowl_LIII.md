Super Bowl Prediction
================

Multivariate Multiple Regression to predict the Super Bowl LIII
---------------------------------------------------------------

### 1. Looking at the data

First we import the data and take an exploratory look at the summary statistics for the 2018 Season. We need to make sure there are no missing values.

The variables are:

Team: Team name
Opponent: opponent team name
location: home/away
TS: team score (to predict)
OS: opponent score (to predict)
TP: team passing (in yards) TR: team rushing (in yards) TY: total yards TT: Team turnover
OP: opponent passing (in yards) OR: opponent rushing (in yards) OY: total yards OT: opponent turnover

We want to predict the TS and OS for LA Rams and New England Patriots by using the team statistics as predictor variables.

``` r
data <- read.csv("NFL+2018+Season+Data.csv")
names(data) <- c("Team","Opponent", "Number", "Location", "TS", "OS", "TP", "TR", "TY", "TT", "OP", "OR", "OY", "OT")
summary(data)
```

    ##                    Team                     Opponent       Number      
    ##  Dallas Cowboys      : 18   Dallas Cowboys      : 18   Min.   : 1.000  
    ##  Indianapolis Colts  : 18   Indianapolis Colts  : 18   1st Qu.: 5.000  
    ##  Kansas City Chiefs  : 18   Kansas City Chiefs  : 18   Median : 9.000  
    ##  Los Angeles Chargers: 18   Los Angeles Chargers: 18   Mean   : 8.835  
    ##  Los Angeles Rams    : 18   Los Angeles Rams    : 18   3rd Qu.:13.000  
    ##  New England Patriots: 18   New England Patriots: 18   Max.   :18.000  
    ##  (Other)             :424   (Other)             :424                   
    ##  Location         TS              OS              TP       
    ##  AWAY:266   Min.   : 0.00   Min.   : 0.00   Min.   : 20.0  
    ##  HOME:266   1st Qu.:17.00   1st Qu.:17.00   1st Qu.:174.8  
    ##             Median :23.00   Median :23.00   Median :229.0  
    ##             Mean   :23.33   Mean   :23.33   Mean   :238.0  
    ##             3rd Qu.:30.00   3rd Qu.:30.00   3rd Qu.:297.0  
    ##             Max.   :54.00   Max.   :54.00   Max.   :462.0  
    ##                                                            
    ##        TR              TY              TT              OP       
    ##  Min.   : 14.0   Min.   : 89.0   Min.   :0.000   Min.   : 20.0  
    ##  1st Qu.: 76.0   1st Qu.:292.0   1st Qu.:0.000   1st Qu.:174.8  
    ##  Median :106.0   Median :352.5   Median :1.000   Median :229.0  
    ##  Mean   :114.1   Mean   :352.1   Mean   :1.346   Mean   :238.0  
    ##  3rd Qu.:146.0   3rd Qu.:415.0   3rd Qu.:2.000   3rd Qu.:297.0  
    ##  Max.   :323.0   Max.   :576.0   Max.   :6.000   Max.   :462.0  
    ##                                                                 
    ##        OR              OY              OT       
    ##  Min.   : 14.0   Min.   : 89.0   Min.   :0.000  
    ##  1st Qu.: 76.0   1st Qu.:292.0   1st Qu.:0.000  
    ##  Median :106.0   Median :352.5   Median :1.000  
    ##  Mean   :114.1   Mean   :352.1   Mean   :1.346  
    ##  3rd Qu.:146.0   3rd Qu.:415.0   3rd Qu.:2.000  
    ##  Max.   :323.0   Max.   :576.0   Max.   :6.000  
    ## 

``` r
pairs(data)
```
![](https://github.com/shruhi/Super-Bowl-LIII-Score-Prediction/blob/master/ReadData-1.png)

### 2. Assumptions for a regression

In order to do a linear regression, the following assumptions are made: &gt;Linear Relatioship

The dependent variables and independent variables have a linear relatioship.

> Homoscedasticity

It means that the variance of the dependent variable(Y) is the same for all values of predictor variables (X) To check both of these, we use scatterplots. A quick look gives us an idea into if a linear regression is suitable here.

### For Team Score

![](https://github.com/shruhi/Super-Bowl-LIII-Score-Prediction/blob/master/TStest-1.png)

#### For Opponent Score

![](https://github.com/shruhi/Super-Bowl-LIII-Score-Prediction/blob/master/OSTest-1.png)

> Multicolinearity

If any of the variables are correlated, it gives causes a problem in our regression model. To verfiy that they are not correlate, we use a simple correlation matrix.

![](https://github.com/shruhi/Super-Bowl-LIII-Score-Prediction/blob/master/corrplot-1.png)

We don't want correlated variables and total yards for both team and opponent are highly correlated. We won't be using them in our model.
Our requirements to do a linear regression are satisfied!

### 3. Multivariate Regression

#### 3.1 Creating training, validation and testing sets

Since this is a relatively smaller dataset, we split it by 80%-10%-10% into training, validation and testing sets. The regression is trained on training set, predicted values for a combination of scores is run on the validation set and finally, a final result is obtained using the test set.

``` r
train   <- 0.8
validate <- 0.1
test <- 0.1

#Sample Sizes
trainsize   <- floor(train * nrow(data))
validatesize <- floor(validate * nrow(data))
testsize <- floor(test * nrow(data))

#Randomly samples indices

trainindex <- sort(sample(seq_len(nrow(data)), size = trainsize))
ntrainindex <- setdiff(seq_len(nrow(data)), trainindex) #to avoid overlap
validateindex <- sort(sample(ntrainindex, size = validatesize))
testindex <- setdiff(ntrainindex, validateindex)

#Now we can have three dataframes

training <- data[trainindex,]
validation <- data[validateindex,]
testing <- data[testindex,]
```

#### 3.2 Training the model

We want to model the variables Team and Opponent Score using the other variables as predictors and view the results. Using anova() we test the covariance between the coefficients in the two models and remove the non-useful predictors. This is run on the training set.

``` r
m2 <- lm(cbind(TS, OS) ~ Team + Opponent + TP + TR + TT + OP + OR + OT, data = training)
summary(m2)
```

    ## Response TS :
    ## 
    ## Call:
    ## lm(formula = TS ~ Team + Opponent + TP + TR + TT + OP + OR + 
    ##     OT, data = training)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -20.7004  -4.2471   0.0581   4.1934  19.8557 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -3.642095   3.209859  -1.135   0.2573    
    ## TeamAtlanta Falcons          -0.491926   2.656885  -0.185   0.8532    
    ## TeamBaltimore Ravens          0.103308   2.812896   0.037   0.9707    
    ## TeamBuffalo Bills            -1.755237   2.712204  -0.647   0.5179    
    ## TeamCarolina Panthers        -1.710062   2.783262  -0.614   0.5393    
    ## TeamChicago Bears            -0.853387   2.565259  -0.333   0.7396    
    ## TeamCincinnati Bengals        2.500372   2.600625   0.961   0.3370    
    ## TeamCleveland Browns         -3.565423   2.605380  -1.368   0.1720    
    ## TeamDallas Cowboys           -1.417216   2.943386  -0.481   0.6305    
    ## TeamDenver Broncos           -4.237091   2.555727  -1.658   0.0982 .  
    ## TeamDetroit Lions            -1.139594   2.499416  -0.456   0.6487    
    ## TeamGreen Bay Packers        -0.425550   2.626890  -0.162   0.8714    
    ## TeamHouston Texans           -0.126750   2.837275  -0.045   0.9644    
    ## TeamIndianapolis Colts       -0.159460   2.721561  -0.059   0.9533    
    ## TeamJacksonville Jaguars     -1.775511   2.674229  -0.664   0.5072    
    ## TeamKansas City Chiefs        6.159205   2.649695   2.324   0.0207 *  
    ## TeamLos Angeles Chargers      2.676854   2.590628   1.033   0.3022    
    ## TeamLos Angeles Rams          3.513886   2.573238   1.366   0.1729    
    ## TeamMiami Dolphins           -0.495290   2.680166  -0.185   0.8535    
    ## TeamMinnesota Vikings         1.534319   2.609765   0.588   0.5570    
    ## TeamNew England Patriots     -0.255999   2.793999  -0.092   0.9270    
    ## TeamNew Orleans Saints        3.807836   2.588705   1.471   0.1422    
    ## TeamNew York Giants           0.477009   2.633085   0.181   0.8563    
    ## TeamNew York Jets             1.791492   2.815094   0.636   0.5249    
    ## TeamOakland Raiders          -3.077156   2.595049  -1.186   0.2365    
    ## TeamPhiladelphia Eagles      -0.058240   2.660541  -0.022   0.9825    
    ## TeamPittsburgh Steelers       0.893154   2.747604   0.325   0.7453    
    ## TeamSan Francisco 49ers      -0.493981   2.619168  -0.189   0.8505    
    ## TeamSeattle Seahawks          0.292518   2.579753   0.113   0.9098    
    ## TeamTampa Bay Buccaneers     -0.423486   2.907729  -0.146   0.8843    
    ## TeamTennessee Titans         -0.697609   2.686471  -0.260   0.7953    
    ## TeamWashington Redskins      -2.186643   2.610904  -0.838   0.4029    
    ## OpponentAtlanta Falcons      -0.324805   2.826970  -0.115   0.9086    
    ## OpponentBaltimore Ravens     -1.612370   2.664716  -0.605   0.5455    
    ## OpponentBuffalo Bills         1.947913   3.036177   0.642   0.5216    
    ## OpponentCarolina Panthers    -0.690375   2.745683  -0.251   0.8016    
    ## OpponentChicago Bears        -1.607893   2.675213  -0.601   0.5482    
    ## OpponentCincinnati Bengals    0.100152   2.674302   0.037   0.9701    
    ## OpponentCleveland Browns     -1.336937   2.838048  -0.471   0.6379    
    ## OpponentDallas Cowboys       -1.011304   2.803753  -0.361   0.7185    
    ## OpponentDenver Broncos       -2.874148   2.742839  -1.048   0.2954    
    ## OpponentDetroit Lions        -2.301606   2.857711  -0.805   0.4211    
    ## OpponentGreen Bay Packers     0.223204   2.596249   0.086   0.9315    
    ## OpponentHouston Texans       -1.616160   2.757501  -0.586   0.5582    
    ## OpponentIndianapolis Colts   -4.192238   2.766028  -1.516   0.1305    
    ## OpponentJacksonville Jaguars -3.585703   2.744927  -1.306   0.1923    
    ## OpponentKansas City Chiefs   -1.656012   2.646274  -0.626   0.5319    
    ## OpponentLos Angeles Chargers -1.102970   2.527287  -0.436   0.6628    
    ## OpponentLos Angeles Rams     -0.518891   2.644679  -0.196   0.8446    
    ## OpponentMiami Dolphins        1.576030   2.632338   0.599   0.5497    
    ## OpponentMinnesota Vikings    -0.718708   2.571556  -0.279   0.7800    
    ## OpponentNew England Patriots -3.891071   2.709835  -1.436   0.1519    
    ## OpponentNew Orleans Saints   -1.483239   2.662685  -0.557   0.5778    
    ## OpponentNew York Giants       0.612798   2.718556   0.225   0.8218    
    ## OpponentNew York Jets         1.233646   2.544750   0.485   0.6281    
    ## OpponentOakland Raiders       1.322370   2.641553   0.501   0.6170    
    ## OpponentPhiladelphia Eagles  -4.898147   2.711130  -1.807   0.0717 .  
    ## OpponentPittsburgh Steelers  -3.285937   2.784292  -1.180   0.2387    
    ## OpponentSan Francisco 49ers  -0.345881   2.513306  -0.138   0.8906    
    ## OpponentSeattle Seahawks      0.720284   2.629153   0.274   0.7843    
    ## OpponentTampa Bay Buccaneers -0.910134   2.761898  -0.330   0.7419    
    ## OpponentTennessee Titans     -4.551440   3.082138  -1.477   0.1406    
    ## OpponentWashington Redskins  -1.608088   2.779582  -0.579   0.5633    
    ## TP                            0.066394   0.004849  13.692  < 2e-16 ***
    ## TR                            0.083967   0.008040  10.443  < 2e-16 ***
    ## TT                           -1.768734   0.296728  -5.961 6.04e-09 ***
    ## OP                            0.011917   0.005139   2.319   0.0210 *  
    ## OR                           -0.005607   0.007888  -0.711   0.4777    
    ## OT                            2.105212   0.306305   6.873 2.83e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.567 on 356 degrees of freedom
    ## Multiple R-squared:  0.6633, Adjusted R-squared:  0.599 
    ## F-statistic: 10.31 on 68 and 356 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Response OS :
    ## 
    ## Call:
    ## lm(formula = OS ~ Team + Opponent + TP + TR + TT + OP + OR + 
    ##     OT, data = training)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -21.9787  -3.9214  -0.0202   3.7186  16.3295 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -2.269908   3.142292  -0.722   0.4705    
    ## TeamAtlanta Falcons          -2.064092   2.600959  -0.794   0.4280    
    ## TeamBaltimore Ravens         -3.422021   2.753685  -1.243   0.2148    
    ## TeamBuffalo Bills             3.776384   2.655113   1.422   0.1558    
    ## TeamCarolina Panthers        -3.123950   2.724675  -1.147   0.2523    
    ## TeamChicago Bears            -2.952124   2.511261  -1.176   0.2406    
    ## TeamCincinnati Bengals       -1.294462   2.545883  -0.508   0.6115    
    ## TeamCleveland Browns         -3.951138   2.550537  -1.549   0.1222    
    ## TeamDallas Cowboys           -2.919103   2.881428  -1.013   0.3117    
    ## TeamDenver Broncos           -4.451177   2.501929  -1.779   0.0761 .  
    ## TeamDetroit Lions            -1.653731   2.446804  -0.676   0.4996    
    ## TeamGreen Bay Packers         0.252156   2.571595   0.098   0.9219    
    ## TeamHouston Texans           -1.379232   2.777551  -0.497   0.6198    
    ## TeamIndianapolis Colts       -0.390920   2.664273  -0.147   0.8834    
    ## TeamJacksonville Jaguars     -4.077684   2.617938  -1.558   0.1202    
    ## TeamKansas City Chiefs       -1.088820   2.593920  -0.420   0.6749    
    ## TeamLos Angeles Chargers     -2.423901   2.536096  -0.956   0.3398    
    ## TeamLos Angeles Rams         -0.492245   2.519072  -0.195   0.8452    
    ## TeamMiami Dolphins            0.626885   2.623749   0.239   0.8113    
    ## TeamMinnesota Vikings        -1.316561   2.554831  -0.515   0.6066    
    ## TeamNew England Patriots     -4.363294   2.735186  -1.595   0.1115    
    ## TeamNew Orleans Saints       -2.610481   2.534213  -1.030   0.3037    
    ## TeamNew York Giants          -0.570877   2.577660  -0.221   0.8249    
    ## TeamNew York Jets             4.007433   2.755837   1.454   0.1468    
    ## TeamOakland Raiders          -0.588308   2.540424  -0.232   0.8170    
    ## TeamPhiladelphia Eagles      -5.669155   2.604537  -2.177   0.0302 *  
    ## TeamPittsburgh Steelers      -4.352127   2.689768  -1.618   0.1065    
    ## TeamSan Francisco 49ers      -1.397164   2.564036  -0.545   0.5862    
    ## TeamSeattle Seahawks         -2.665637   2.525450  -1.056   0.2919    
    ## TeamTampa Bay Buccaneers     -3.439316   2.846523  -1.208   0.2278    
    ## TeamTennessee Titans         -2.842097   2.629921  -1.081   0.2806    
    ## TeamWashington Redskins      -0.911000   2.555946  -0.356   0.7217    
    ## OpponentAtlanta Falcons       0.262691   2.767463   0.095   0.9244    
    ## OpponentBaltimore Ravens      1.344268   2.608625   0.515   0.6067    
    ## OpponentBuffalo Bills        -3.458631   2.972267  -1.164   0.2454    
    ## OpponentCarolina Panthers     0.456323   2.687888   0.170   0.8653    
    ## OpponentChicago Bears         1.135532   2.618901   0.434   0.6648    
    ## OpponentCincinnati Bengals    3.931115   2.618009   1.502   0.1341    
    ## OpponentCleveland Browns     -1.161453   2.778308  -0.418   0.6762    
    ## OpponentDallas Cowboys        0.788171   2.744735   0.287   0.7742    
    ## OpponentDenver Broncos       -1.369404   2.685103  -0.510   0.6104    
    ## OpponentDetroit Lions        -1.627018   2.797557  -0.582   0.5612    
    ## OpponentGreen Bay Packers    -0.818736   2.541599  -0.322   0.7475    
    ## OpponentHouston Texans       -1.521211   2.699457  -0.564   0.5734    
    ## OpponentIndianapolis Colts   -0.081075   2.707804  -0.030   0.9761    
    ## OpponentJacksonville Jaguars -3.599427   2.687147  -1.339   0.1813    
    ## OpponentKansas City Chiefs    6.036845   2.590571   2.330   0.0203 *  
    ## OpponentLos Angeles Chargers  3.587653   2.474088   1.450   0.1479    
    ## OpponentLos Angeles Rams      3.240153   2.589009   1.252   0.2116    
    ## OpponentMiami Dolphins        0.006224   2.576928   0.002   0.9981    
    ## OpponentMinnesota Vikings     0.722332   2.517426   0.287   0.7743    
    ## OpponentNew England Patriots -2.169996   2.652794  -0.818   0.4139    
    ## OpponentNew Orleans Saints    4.100821   2.606636   1.573   0.1166    
    ## OpponentNew York Giants      -0.333664   2.661331  -0.125   0.9003    
    ## OpponentNew York Jets         1.992043   2.491184   0.800   0.4245    
    ## OpponentOakland Raiders      -1.091910   2.585949  -0.422   0.6731    
    ## OpponentPhiladelphia Eagles  -0.078094   2.654062  -0.029   0.9765    
    ## OpponentPittsburgh Steelers   1.993533   2.725684   0.731   0.4650    
    ## OpponentSan Francisco 49ers   0.771482   2.460402   0.314   0.7540    
    ## OpponentSeattle Seahawks      0.888518   2.573811   0.345   0.7301    
    ## OpponentTampa Bay Buccaneers -0.271083   2.703761  -0.100   0.9202    
    ## OpponentTennessee Titans     -1.866305   3.017259  -0.619   0.5366    
    ## OpponentWashington Redskins  -1.786873   2.721073  -0.657   0.5118    
    ## TP                            0.010137   0.004747   2.136   0.0334 *  
    ## TR                           -0.003157   0.007871  -0.401   0.6886    
    ## TT                            2.321121   0.290482   7.991 1.89e-14 ***
    ## OP                            0.064585   0.005031  12.838  < 2e-16 ***
    ## OR                            0.075575   0.007722   9.787  < 2e-16 ***
    ## OT                           -1.603834   0.299857  -5.349 1.59e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.429 on 356 degrees of freedom
    ## Multiple R-squared:  0.6479, Adjusted R-squared:  0.5807 
    ## F-statistic: 9.634 on 68 and 356 DF,  p-value: < 2.2e-16

``` r
anova(m2)
```

    ## Analysis of Variance Table
    ## 
    ##              Df  Pillai approx F num Df den Df    Pr(>F)    
    ## (Intercept)   1 0.95997   4256.7      2    355 < 2.2e-16 ***
    ## Team         31 0.65800      5.6     62    712 < 2.2e-16 ***
    ## Opponent     31 0.63446      5.3     62    712 < 2.2e-16 ***
    ## TP            1 0.26048     62.5      2    355 < 2.2e-16 ***
    ## TR            1 0.37311    105.6      2    355 < 2.2e-16 ***
    ## TT            1 0.28420     70.5      2    355 < 2.2e-16 ***
    ## OP            1 0.20886     46.9      2    355 < 2.2e-16 ***
    ## OR            1 0.24269     56.9      2    355 < 2.2e-16 ***
    ## OT            1 0.22678     52.1      2    355 < 2.2e-16 ***
    ## Residuals   356                                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### 3.3 Validate it!

To test the accuracy of this model, we validate it on the validation set. To evaluate it, we need a parameter. We choose the root mean square error (RMSE). The errors are expected to be normally distributed.

``` r
val <- data.frame(predict(m2, validation, type = "response"))
names(val) = c("predTS", "predOS")
validation <- cbind.data.frame(validation, val)

library(ModelMetrics)
```

    ## 
    ## Attaching package: 'ModelMetrics'

    ## The following object is masked from 'package:base':
    ## 
    ##     kappa

``` r
error_plot <- function(df){
for (i in 1:nrow(df)){
  df$var[i] <- ((df$TS[i]- df$predTS[i]) +  (df$OS[i] - df$predOS[i]))/2
  df$rmse[i] <- rmse(c(df$TS[i],df$OS[i]),  c(df$predTS[i], df$predOS[i]))
}
  a <- ggplot(df, aes(x = var)) + geom_density()
  plot(a)
  print("Average RMSE is")
  avg_rmse <- mean(df$rmse)
  avg_rmse
}

error_plot(validation)
```

![](https://github.com/shruhi/Super-Bowl-LIII-Score-Prediction/blob/master/valerr-1.png)

    ## [1] "Average RMSE is"

    ## [1] 5.628645

#### 3.4 Testing!

The next step to this supervised learning model is to run this model on our testing set and check the accuracy for it.

``` r
valt <- data.frame(predict(m2, testing, type = "response"))
names(valt) = c("predTS", "predOS")
testing <- cbind.data.frame(testing, valt)

error_plot(testing)
```

![](https://github.com/shruhi/Super-Bowl-LIII-Score-Prediction/blob/master/testerr-1.png)

    ## [1] "Average RMSE is"

    ## [1] 6.265477

Lower error on the testing set!

### 4. Prediction

> What-if Scenarios

I will run a bunch of what-if scenarios for the prediciton. First let's start with the obvious. Take the average statistics for both teams and try to predict the outcome.
Second, based on the experience, take the 1st quartile performance of Rams and an average performace by Patriots.
Next we have a best case scenario where both teams give their best performance.

To really see an avergae performance, consensus scores are formulated. E.g. the average Team passing yards for Rams would be an average of TP(Rams) and OP(Patriots) and so forth. These data manipulations are done in excel and the csv can be imported for predictions to be made.

``` r
scenarios <- read.csv("scenarios.csv")
```

    ## Warning in read.table(file = file, header = header, sep = sep, quote =
    ## quote, : incomplete final line found by readTableHeader on 'scenarios.csv'

``` r
scenarios
```

    ##               Team             Opponent      TP      TR    TT     OP
    ## 1 Los Angeles Rams New England Patriots 263.940 123.445 1.390 256.36
    ## 2 Los Angeles Rams New England Patriots 223.285  99.645 0.705 224.25
    ## 3 Los Angeles Rams New England Patriots 416.000 231.000 4.000 442.00
    ##        OR    OT    scenario
    ## 1 122.865 1.415 Average-Avg
    ## 2 102.980 0.970     Bad-Avg
    ## 3 273.000 4.000   Best-Best

``` r
predt <- data.frame(predict(m2, scenarios, type = "response"))
names(predt) <- c("PredictedTS","PredictedOS")
scenarios <- cbind(scenarios, predt)
scenarios
```

    ##               Team             Opponent      TP      TR    TT     OP
    ## 1 Los Angeles Rams New England Patriots 263.940 123.445 1.390 256.36
    ## 2 Los Angeles Rams New England Patriots 223.285  99.645 0.705 224.25
    ## 3 Los Angeles Rams New England Patriots 416.000 231.000 4.000 442.00
    ##        OR    OT    scenario PredictedTS PredictedOS
    ## 1 122.865 1.415 Average-Avg    26.75660    24.15315
    ## 2 102.980 0.970     Bad-Avg    22.06253    19.36327
    ## 3 273.000 4.000   Best-Best    48.07963    50.60322

``` r
  datafr <- data.frame(scenarios[1,10:11])
  fr <- data.frame()
  fr <- rbind(fr, datafr)
```

Final Prediction
----------------

We have a no. of poosibilities in this manner. For this prediction, I will consider average performance of both teams this season.

For more accuracy, the model is reiterated 5 more times to get five final predictions and then the average is taken for a final prediction:

    ##    X PredictedTS PredictedOS
    ## 1  1    28.18823    25.25942
    ## 2 11    27.12906    23.46251
    ## 3 12    25.70178    24.65486
    ## 4 13    27.15497    26.51249
    ## 5 14    27.05595    27.25775

    ##   X Rams.Score Patriots.Score
    ## 1 1     27.046       25.42941
