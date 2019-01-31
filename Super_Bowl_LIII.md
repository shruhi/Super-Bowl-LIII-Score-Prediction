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

![](Super_Bowl_LIII_files/figure-markdown_github/Read%20Data-1.png)

### 2. Assumptions for a regression

In order to do a linear regression, the following assumptions are made: &gt;Linear Relatioship

The dependent variables and independent variables have a linear relatioship.

> Homoscedasticity

It means that the variance of the dependent variable(Y) is the same for all values of predictor variables (X) To check both of these, we use scatterplots. A quick look gives us an idea into if a linear regression is suitable here.

### For Team Score

![](Super_Bowl_LIII_files/figure-markdown_github/unnamed-chunk-2-1.png)

#### For Opponent Score

![](Super_Bowl_LIII_files/figure-markdown_github/unnamed-chunk-3-1.png)

> Multicolinearity

If any of the variables are correlated, it gives causes a problem in our regression model. To verfiy that they are not correlate, we use a simple correlation matrix.

![](Super_Bowl_LIII_files/figure-markdown_github/unnamed-chunk-4-1.png)

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
    ## -21.3279  -3.5917   0.2754   3.7861  19.2760 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -4.836496   3.231076  -1.497   0.1353    
    ## TeamAtlanta Falcons          -1.330373   2.796430  -0.476   0.6346    
    ## TeamBaltimore Ravens         -0.591375   2.591944  -0.228   0.8197    
    ## TeamBuffalo Bills            -2.338904   2.620151  -0.893   0.3726    
    ## TeamCarolina Panthers        -2.331119   2.602159  -0.896   0.3709    
    ## TeamChicago Bears            -1.851075   2.657109  -0.697   0.4865    
    ## TeamCincinnati Bengals        1.354884   2.593653   0.522   0.6017    
    ## TeamCleveland Browns         -2.382220   2.642634  -0.901   0.3680    
    ## TeamDallas Cowboys           -1.491292   2.548956  -0.585   0.5589    
    ## TeamDenver Broncos           -6.391813   2.651189  -2.411   0.0164 *  
    ## TeamDetroit Lions            -2.517062   2.883913  -0.873   0.3834    
    ## TeamGreen Bay Packers        -0.372002   2.676141  -0.139   0.8895    
    ## TeamHouston Texans           -1.441238   2.765879  -0.521   0.6026    
    ## TeamIndianapolis Colts        0.242270   2.546800   0.095   0.9243    
    ## TeamJacksonville Jaguars     -2.837678   2.736115  -1.037   0.3004    
    ## TeamKansas City Chiefs        5.820100   2.653893   2.193   0.0290 *  
    ## TeamLos Angeles Chargers      3.087470   2.545140   1.213   0.2259    
    ## TeamLos Angeles Rams          3.347634   2.577962   1.299   0.1949    
    ## TeamMiami Dolphins            0.663384   2.649874   0.250   0.8025    
    ## TeamMinnesota Vikings        -0.823784   2.682785  -0.307   0.7590    
    ## TeamNew England Patriots     -0.990652   2.587302  -0.383   0.7020    
    ## TeamNew Orleans Saints        4.023591   2.579337   1.560   0.1197    
    ## TeamNew York Giants          -0.826405   2.757642  -0.300   0.7646    
    ## TeamNew York Jets             3.985236   2.688919   1.482   0.1392    
    ## TeamOakland Raiders          -1.875911   2.710056  -0.692   0.4893    
    ## TeamPhiladelphia Eagles      -0.975677   2.544042  -0.384   0.7016    
    ## TeamPittsburgh Steelers       2.955855   2.690306   1.099   0.2726    
    ## TeamSan Francisco 49ers       1.047685   2.655078   0.395   0.6934    
    ## TeamSeattle Seahawks          0.392265   2.626685   0.149   0.8814    
    ## TeamTampa Bay Buccaneers     -0.926585   2.811188  -0.330   0.7419    
    ## TeamTennessee Titans         -1.554790   2.650671  -0.587   0.5579    
    ## TeamWashington Redskins      -0.533590   2.632182  -0.203   0.8395    
    ## OpponentAtlanta Falcons       1.884294   2.825429   0.667   0.5053    
    ## OpponentBaltimore Ravens      2.515539   3.135058   0.802   0.4229    
    ## OpponentBuffalo Bills         4.393936   2.831073   1.552   0.1215    
    ## OpponentCarolina Panthers     1.250782   2.876903   0.435   0.6640    
    ## OpponentChicago Bears         2.608435   2.811806   0.928   0.3542    
    ## OpponentCincinnati Bengals    2.343117   2.694644   0.870   0.3851    
    ## OpponentCleveland Browns     -0.341568   2.745278  -0.124   0.9011    
    ## OpponentDallas Cowboys        1.113357   2.669769   0.417   0.6769    
    ## OpponentDenver Broncos       -0.512979   2.861930  -0.179   0.8578    
    ## OpponentDetroit Lions         2.391958   2.796716   0.855   0.3930    
    ## OpponentGreen Bay Packers     3.633869   2.761634   1.316   0.1891    
    ## OpponentHouston Texans        1.450364   2.782579   0.521   0.6025    
    ## OpponentIndianapolis Colts    0.850901   2.854235   0.298   0.7658    
    ## OpponentJacksonville Jaguars -0.871257   2.718025  -0.321   0.7487    
    ## OpponentKansas City Chiefs    0.661608   2.742614   0.241   0.8095    
    ## OpponentLos Angeles Chargers  0.716129   2.670789   0.268   0.7888    
    ## OpponentLos Angeles Rams      3.146170   2.740552   1.148   0.2517    
    ## OpponentMiami Dolphins        6.826700   2.998705   2.277   0.0234 *  
    ## OpponentMinnesota Vikings     1.739921   2.809075   0.619   0.5361    
    ## OpponentNew England Patriots -0.633819   2.942289  -0.215   0.8296    
    ## OpponentNew Orleans Saints    2.310423   2.832569   0.816   0.4152    
    ## OpponentNew York Giants       4.572714   2.809040   1.628   0.1044    
    ## OpponentNew York Jets         5.542482   2.749269   2.016   0.0446 *  
    ## OpponentOakland Raiders       2.163090   2.840958   0.761   0.4469    
    ## OpponentPhiladelphia Eagles  -2.240022   2.703766  -0.828   0.4080    
    ## OpponentPittsburgh Steelers   0.794358   2.828029   0.281   0.7790    
    ## OpponentSan Francisco 49ers   4.752699   2.928209   1.623   0.1055    
    ## OpponentSeattle Seahawks      3.628301   2.803159   1.294   0.1964    
    ## OpponentTampa Bay Buccaneers  3.133410   2.864284   1.094   0.2747    
    ## OpponentTennessee Titans     -0.113387   2.857580  -0.040   0.9684    
    ## OpponentWashington Redskins   1.870148   2.795056   0.669   0.5039    
    ## TP                            0.068697   0.004805  14.297  < 2e-16 ***
    ## TR                            0.080109   0.007587  10.559  < 2e-16 ***
    ## TT                           -1.941170   0.289526  -6.705 7.93e-11 ***
    ## OP                            0.009231   0.004867   1.897   0.0587 .  
    ## OR                           -0.010140   0.007338  -1.382   0.1678    
    ## OT                            2.105152   0.289487   7.272 2.27e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.372 on 356 degrees of freedom
    ## Multiple R-squared:  0.6856, Adjusted R-squared:  0.6255 
    ## F-statistic: 11.41 on 68 and 356 DF,  p-value: < 2.2e-16
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
    ## -14.2770  -3.4825   0.0492   3.2401  19.5731 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -3.520273   3.119785  -1.128   0.2599    
    ## TeamAtlanta Falcons           0.933583   2.700109   0.346   0.7297    
    ## TeamBaltimore Ravens         -3.396578   2.502667  -1.357   0.1756    
    ## TeamBuffalo Bills             2.283921   2.529902   0.903   0.3673    
    ## TeamCarolina Panthers        -1.316667   2.512530  -0.524   0.6006    
    ## TeamChicago Bears            -1.548689   2.565587  -0.604   0.5465    
    ## TeamCincinnati Bengals       -0.187548   2.504317  -0.075   0.9403    
    ## TeamCleveland Browns         -1.697694   2.551611  -0.665   0.5063    
    ## TeamDallas Cowboys           -1.056099   2.461159  -0.429   0.6681    
    ## TeamDenver Broncos           -4.633021   2.559871  -1.810   0.0712 .  
    ## TeamDetroit Lions             0.074542   2.784579   0.027   0.9787    
    ## TeamGreen Bay Packers         0.254457   2.583964   0.098   0.9216    
    ## TeamHouston Texans           -2.237694   2.670611  -0.838   0.4027    
    ## TeamIndianapolis Colts       -3.504158   2.459077  -1.425   0.1550    
    ## TeamJacksonville Jaguars     -4.952873   2.641872  -1.875   0.0616 .  
    ## TeamKansas City Chiefs       -2.589482   2.562482  -1.011   0.3129    
    ## TeamLos Angeles Chargers     -1.496597   2.457475  -0.609   0.5429    
    ## TeamLos Angeles Rams          0.396617   2.489166   0.159   0.8735    
    ## TeamMiami Dolphins            0.069001   2.558601   0.027   0.9785    
    ## TeamMinnesota Vikings         0.230919   2.590379   0.089   0.9290    
    ## TeamNew England Patriots     -3.482756   2.498184  -1.394   0.1642    
    ## TeamNew Orleans Saints       -1.313050   2.490493  -0.527   0.5984    
    ## TeamNew York Giants          -0.453394   2.662657  -0.170   0.8649    
    ## TeamNew York Jets             3.207132   2.596301   1.235   0.2175    
    ## TeamOakland Raiders          -0.150007   2.616711  -0.057   0.9543    
    ## TeamPhiladelphia Eagles      -5.506758   2.456415  -2.242   0.0256 *  
    ## TeamPittsburgh Steelers      -2.966766   2.597640  -1.142   0.2542    
    ## TeamSan Francisco 49ers       0.251124   2.563626   0.098   0.9220    
    ## TeamSeattle Seahawks          0.850755   2.536210   0.335   0.7375    
    ## TeamTampa Bay Buccaneers     -1.871530   2.714359  -0.689   0.4910    
    ## TeamTennessee Titans         -4.214661   2.559371  -1.647   0.1005    
    ## TeamWashington Redskins       1.774686   2.541519   0.698   0.4855    
    ## OpponentAtlanta Falcons      -0.875226   2.728109  -0.321   0.7485    
    ## OpponentBaltimore Ravens      1.762371   3.027073   0.582   0.5608    
    ## OpponentBuffalo Bills        -0.923802   2.733559  -0.338   0.7356    
    ## OpponentCarolina Panthers    -3.775607   2.777811  -1.359   0.1749    
    ## OpponentChicago Bears         0.975845   2.714956   0.359   0.7195    
    ## OpponentCincinnati Bengals    3.227797   2.601829   1.241   0.2156    
    ## OpponentCleveland Browns     -2.679317   2.650719  -1.011   0.3128    
    ## OpponentDallas Cowboys       -1.206638   2.577811  -0.468   0.6400    
    ## OpponentDenver Broncos       -2.627022   2.763353  -0.951   0.3424    
    ## OpponentDetroit Lions        -0.717911   2.700386  -0.266   0.7905    
    ## OpponentGreen Bay Packers    -2.421282   2.666512  -0.908   0.3645    
    ## OpponentHouston Texans       -0.108203   2.686735  -0.040   0.9679    
    ## OpponentIndianapolis Colts    0.892208   2.755924   0.324   0.7463    
    ## OpponentJacksonville Jaguars -1.646490   2.624405  -0.627   0.5308    
    ## OpponentKansas City Chiefs    5.818405   2.648147   2.197   0.0287 *  
    ## OpponentLos Angeles Chargers  3.152819   2.578796   1.223   0.2223    
    ## OpponentLos Angeles Rams      3.362793   2.646156   1.271   0.2046    
    ## OpponentMiami Dolphins        1.951790   2.895417   0.674   0.5007    
    ## OpponentMinnesota Vikings    -2.517083   2.712319  -0.928   0.3540    
    ## OpponentNew England Patriots -2.175639   2.840945  -0.766   0.4443    
    ## OpponentNew Orleans Saints    3.725539   2.735003   1.362   0.1740    
    ## OpponentNew York Giants       1.398968   2.712285   0.516   0.6063    
    ## OpponentNew York Jets         3.910475   2.654573   1.473   0.1416    
    ## OpponentOakland Raiders      -2.298060   2.743104  -0.838   0.4027    
    ## OpponentPhiladelphia Eagles  -1.487968   2.610637  -0.570   0.5691    
    ## OpponentPittsburgh Steelers   1.671091   2.730620   0.612   0.5409    
    ## OpponentSan Francisco 49ers   1.990773   2.827349   0.704   0.4818    
    ## OpponentSeattle Seahawks      0.845048   2.706607   0.312   0.7551    
    ## OpponentTampa Bay Buccaneers  1.052230   2.765626   0.380   0.7038    
    ## OpponentTennessee Titans      0.095406   2.759153   0.035   0.9724    
    ## OpponentWashington Redskins  -1.011977   2.698782  -0.375   0.7079    
    ## TP                            0.012186   0.004639   2.627   0.0090 ** 
    ## TR                           -0.008358   0.007326  -1.141   0.2547    
    ## TT                            2.131073   0.279553   7.623 2.27e-13 ***
    ## OP                            0.070897   0.004699  15.087  < 2e-16 ***
    ## OR                            0.077860   0.007085  10.990  < 2e-16 ***
    ## OT                           -1.956775   0.279516  -7.001 1.28e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.152 on 356 degrees of freedom
    ## Multiple R-squared:  0.6981, Adjusted R-squared:  0.6404 
    ## F-statistic:  12.1 on 68 and 356 DF,  p-value: < 2.2e-16

``` r
anova(m2)
```

    ## Analysis of Variance Table
    ## 
    ##              Df  Pillai approx F num Df den Df    Pr(>F)    
    ## (Intercept)   1 0.96403   4756.7      2    355 < 2.2e-16 ***
    ## Team         31 0.74036      6.7     62    712 < 2.2e-16 ***
    ## Opponent     31 0.63946      5.4     62    712 < 2.2e-16 ***
    ## TP            1 0.28470     70.6      2    355 < 2.2e-16 ***
    ## TR            1 0.40738    122.0      2    355 < 2.2e-16 ***
    ## TT            1 0.28602     71.1      2    355 < 2.2e-16 ***
    ## OP            1 0.27656     67.9      2    355 < 2.2e-16 ***
    ## OR            1 0.30725     78.7      2    355 < 2.2e-16 ***
    ## OT            1 0.28196     69.7      2    355 < 2.2e-16 ***
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

![](Super_Bowl_LIII_files/figure-markdown_github/unnamed-chunk-7-1.png)

    ## [1] "Average RMSE is"

    ## [1] 6.80082

#### 3.4 Testing!

The next step to this supervised learning model is to run this model on our testing set and check the accuracy for it.

``` r
valt <- data.frame(predict(m2, testing, type = "response"))
names(valt) = c("predTS", "predOS")
testing <- cbind.data.frame(testing, valt)

error_plot(testing)
```

![](Super_Bowl_LIII_files/figure-markdown_github/unnamed-chunk-8-1.png)

    ## [1] "Average RMSE is"

    ## [1] 6.757044

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
    ## 1 122.865 1.415 Average-Avg    27.29937    24.82009
    ## 2 102.980 0.970     Bad-Avg    22.89805    20.10982
    ## 3 273.000 4.000   Best-Best    46.92815    51.12882

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
