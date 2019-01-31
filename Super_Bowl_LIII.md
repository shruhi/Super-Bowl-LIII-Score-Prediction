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

![](Super_Bowl_LIII_files/figure-markdown_github/ReadData-1.png)

### 2. Assumptions for a regression

In order to do a linear regression, the following assumptions are made: &gt;Linear Relatioship

The dependent variables and independent variables have a linear relatioship.

> Homoscedasticity

It means that the variance of the dependent variable(Y) is the same for all values of predictor variables (X) To check both of these, we use scatterplots. A quick look gives us an idea into if a linear regression is suitable here.

### For Team Score

![](Super_Bowl_LIII_files/figure-markdown_github/TStest-1.png)

#### For Opponent Score

![](Super_Bowl_LIII_files/figure-markdown_github/OSTest-1.png)

> Multicolinearity

If any of the variables are correlated, it gives causes a problem in our regression model. To verfiy that they are not correlate, we use a simple correlation matrix.

![](Super_Bowl_LIII_files/figure-markdown_github/corrplot-1.png)

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
    ## -21.7090  -4.1352   0.0272   3.9827  19.3041 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -1.024417   3.212057  -0.319   0.7500    
    ## TeamAtlanta Falcons          -0.794131   2.843189  -0.279   0.7802    
    ## TeamBaltimore Ravens         -1.117375   2.738754  -0.408   0.6835    
    ## TeamBuffalo Bills            -2.423346   2.830874  -0.856   0.3926    
    ## TeamCarolina Panthers        -1.982637   2.885034  -0.687   0.4924    
    ## TeamChicago Bears             0.496617   2.743296   0.181   0.8564    
    ## TeamCincinnati Bengals        1.760604   2.712414   0.649   0.5167    
    ## TeamCleveland Browns         -3.904938   2.732474  -1.429   0.1539    
    ## TeamDallas Cowboys           -1.665910   2.611908  -0.638   0.5240    
    ## TeamDenver Broncos           -3.461663   2.692785  -1.286   0.1994    
    ## TeamDetroit Lions            -0.802010   2.718097  -0.295   0.7681    
    ## TeamGreen Bay Packers        -3.132202   2.792967  -1.121   0.2628    
    ## TeamHouston Texans           -1.828206   2.643507  -0.692   0.4896    
    ## TeamIndianapolis Colts       -0.614972   2.783564  -0.221   0.8253    
    ## TeamJacksonville Jaguars     -4.002309   2.680509  -1.493   0.1363    
    ## TeamKansas City Chiefs        5.594677   2.671345   2.094   0.0369 *  
    ## TeamLos Angeles Chargers      2.555520   2.593070   0.986   0.3250    
    ## TeamLos Angeles Rams          1.380932   2.678257   0.516   0.6064    
    ## TeamMiami Dolphins           -0.792453   2.863201  -0.277   0.7821    
    ## TeamMinnesota Vikings        -0.934749   2.648983  -0.353   0.7244    
    ## TeamNew England Patriots      0.776112   2.766192   0.281   0.7792    
    ## TeamNew Orleans Saints        3.251446   2.652553   1.226   0.2211    
    ## TeamNew York Giants          -0.253080   2.831483  -0.089   0.9288    
    ## TeamNew York Jets             1.826427   2.759187   0.662   0.5084    
    ## TeamOakland Raiders          -3.005358   2.632474  -1.142   0.2544    
    ## TeamPhiladelphia Eagles      -1.419801   2.786393  -0.510   0.6107    
    ## TeamPittsburgh Steelers       1.644757   2.793399   0.589   0.5564    
    ## TeamSan Francisco 49ers      -1.468761   2.716068  -0.541   0.5890    
    ## TeamSeattle Seahawks         -0.514846   2.726341  -0.189   0.8503    
    ## TeamTampa Bay Buccaneers     -0.916612   2.866851  -0.320   0.7494    
    ## TeamTennessee Titans         -1.187542   2.860051  -0.415   0.6782    
    ## TeamWashington Redskins      -2.386655   2.737870  -0.872   0.3839    
    ## OpponentAtlanta Falcons      -2.565459   2.748419  -0.933   0.3512    
    ## OpponentBaltimore Ravens     -2.463488   2.691902  -0.915   0.3607    
    ## OpponentBuffalo Bills         6.032934   2.760589   2.185   0.0295 *  
    ## OpponentCarolina Panthers    -1.079033   2.684208  -0.402   0.6879    
    ## OpponentChicago Bears        -0.557215   2.716796  -0.205   0.8376    
    ## OpponentCincinnati Bengals   -0.347317   2.797040  -0.124   0.9012    
    ## OpponentCleveland Browns     -3.520084   2.598356  -1.355   0.1764    
    ## OpponentDallas Cowboys       -1.286681   2.599091  -0.495   0.6209    
    ## OpponentDenver Broncos       -3.908094   2.551142  -1.532   0.1264    
    ## OpponentDetroit Lions        -3.187306   2.650250  -1.203   0.2299    
    ## OpponentGreen Bay Packers    -0.327481   2.592779  -0.126   0.8996    
    ## OpponentHouston Texans       -1.549856   2.738598  -0.566   0.5718    
    ## OpponentIndianapolis Colts   -1.539529   2.683081  -0.574   0.5665    
    ## OpponentJacksonville Jaguars -4.258556   2.657769  -1.602   0.1100    
    ## OpponentKansas City Chiefs   -2.114657   2.613719  -0.809   0.4190    
    ## OpponentLos Angeles Chargers -2.852844   2.617643  -1.090   0.2765    
    ## OpponentLos Angeles Rams     -0.084863   2.624347  -0.032   0.9742    
    ## OpponentMiami Dolphins        2.081094   2.625308   0.793   0.4285    
    ## OpponentMinnesota Vikings    -1.216930   2.616320  -0.465   0.6421    
    ## OpponentNew England Patriots -3.400416   2.721819  -1.249   0.2124    
    ## OpponentNew Orleans Saints   -0.343275   2.734809  -0.126   0.9002    
    ## OpponentNew York Giants      -0.667398   2.767812  -0.241   0.8096    
    ## OpponentNew York Jets         0.910158   2.497950   0.364   0.7158    
    ## OpponentOakland Raiders      -0.594642   2.571180  -0.231   0.8172    
    ## OpponentPhiladelphia Eagles  -6.156892   2.627081  -2.344   0.0196 *  
    ## OpponentPittsburgh Steelers  -1.363715   2.758130  -0.494   0.6213    
    ## OpponentSan Francisco 49ers  -0.213069   2.729889  -0.078   0.9378    
    ## OpponentSeattle Seahawks     -0.521528   2.546057  -0.205   0.8378    
    ## OpponentTampa Bay Buccaneers -1.386593   2.691366  -0.515   0.6067    
    ## OpponentTennessee Titans     -3.092562   2.653282  -1.166   0.2446    
    ## OpponentWashington Redskins  -1.605166   2.667026  -0.602   0.5477    
    ## TP                            0.069229   0.005217  13.270  < 2e-16 ***
    ## TR                            0.078339   0.007725  10.140  < 2e-16 ***
    ## TT                           -1.962896   0.301249  -6.516 2.47e-10 ***
    ## OP                            0.008907   0.005117   1.741   0.0826 .  
    ## OR                           -0.008875   0.007915  -1.121   0.2629    
    ## OT                            1.760081   0.308388   5.707 2.42e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.607 on 356 degrees of freedom
    ## Multiple R-squared:  0.646,  Adjusted R-squared:  0.5784 
    ## F-statistic: 9.553 on 68 and 356 DF,  p-value: < 2.2e-16
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
    ## -21.1789  -3.8515   0.2122   3.9154  18.3315 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -1.701251   3.122073  -0.545   0.5862    
    ## TeamAtlanta Falcons          -2.030802   2.763538  -0.735   0.4629    
    ## TeamBaltimore Ravens         -4.140967   2.662029  -1.556   0.1207    
    ## TeamBuffalo Bills             1.276440   2.751568   0.464   0.6430    
    ## TeamCarolina Panthers         0.187314   2.804211   0.067   0.9468    
    ## TeamChicago Bears             0.243564   2.666444   0.091   0.9273    
    ## TeamCincinnati Bengals       -1.204032   2.636426  -0.457   0.6482    
    ## TeamCleveland Browns         -4.532025   2.655925  -1.706   0.0888 .  
    ## TeamDallas Cowboys           -1.938279   2.538736  -0.763   0.4457    
    ## TeamDenver Broncos           -3.031202   2.617347  -1.158   0.2476    
    ## TeamDetroit Lions            -1.519952   2.641950  -0.575   0.5654    
    ## TeamGreen Bay Packers        -0.412142   2.714723  -0.152   0.8794    
    ## TeamHouston Texans           -1.841770   2.569450  -0.717   0.4740    
    ## TeamIndianapolis Colts       -0.281590   2.705583  -0.104   0.9172    
    ## TeamJacksonville Jaguars     -3.718377   2.605415  -1.427   0.1544    
    ## TeamKansas City Chiefs       -3.034157   2.596508  -1.169   0.2434    
    ## TeamLos Angeles Chargers     -3.484001   2.520426  -1.382   0.1677    
    ## TeamLos Angeles Rams         -1.842710   2.603227  -0.708   0.4795    
    ## TeamMiami Dolphins            0.073535   2.782989   0.026   0.9789    
    ## TeamMinnesota Vikings        -1.478651   2.574773  -0.574   0.5661    
    ## TeamNew England Patriots     -2.836745   2.688698  -1.055   0.2921    
    ## TeamNew Orleans Saints       -1.389512   2.578243  -0.539   0.5903    
    ## TeamNew York Giants           0.544175   2.752160   0.198   0.8434    
    ## TeamNew York Jets             0.570135   2.681889   0.213   0.8318    
    ## TeamOakland Raiders          -0.197820   2.558726  -0.077   0.9384    
    ## TeamPhiladelphia Eagles      -5.683611   2.708333  -2.099   0.0366 *  
    ## TeamPittsburgh Steelers      -3.273905   2.715143  -1.206   0.2287    
    ## TeamSan Francisco 49ers      -2.968936   2.639979  -1.125   0.2615    
    ## TeamSeattle Seahawks         -0.266784   2.649963  -0.101   0.9199    
    ## TeamTampa Bay Buccaneers     -2.995082   2.786537  -1.075   0.2832    
    ## TeamTennessee Titans         -3.378624   2.779927  -1.215   0.2250    
    ## TeamWashington Redskins      -1.520679   2.661170  -0.571   0.5681    
    ## OpponentAtlanta Falcons       0.040974   2.671423   0.015   0.9878    
    ## OpponentBaltimore Ravens     -1.159996   2.616489  -0.443   0.6578    
    ## OpponentBuffalo Bills        -1.895084   2.683252  -0.706   0.4805    
    ## OpponentCarolina Panthers    -1.263671   2.609011  -0.484   0.6284    
    ## OpponentChicago Bears         1.539157   2.640686   0.583   0.5604    
    ## OpponentCincinnati Bengals    2.960160   2.718681   1.089   0.2770    
    ## OpponentCleveland Browns     -2.801913   2.525564  -1.109   0.2680    
    ## OpponentDallas Cowboys       -1.156491   2.526278  -0.458   0.6474    
    ## OpponentDenver Broncos       -3.448580   2.479673  -1.391   0.1652    
    ## OpponentDetroit Lions        -1.238302   2.576004  -0.481   0.6310    
    ## OpponentGreen Bay Packers    -2.272952   2.520144  -0.902   0.3677    
    ## OpponentHouston Texans       -2.536756   2.661877  -0.953   0.3412    
    ## OpponentIndianapolis Colts   -0.590852   2.607915  -0.227   0.8209    
    ## OpponentJacksonville Jaguars -3.570927   2.583312  -1.382   0.1677    
    ## OpponentKansas City Chiefs    5.052149   2.540496   1.989   0.0475 *  
    ## OpponentLos Angeles Chargers  3.716452   2.544311   1.461   0.1450    
    ## OpponentLos Angeles Rams      2.729172   2.550827   1.070   0.2854    
    ## OpponentMiami Dolphins       -0.797208   2.551761  -0.312   0.7549    
    ## OpponentMinnesota Vikings    -1.584478   2.543024  -0.623   0.5336    
    ## OpponentNew England Patriots -0.676099   2.645568  -0.256   0.7984    
    ## OpponentNew Orleans Saints    6.365532   2.658194   2.395   0.0172 *  
    ## OpponentNew York Giants      -1.996267   2.690272  -0.742   0.4586    
    ## OpponentNew York Jets         1.558914   2.427971   0.642   0.5212    
    ## OpponentOakland Raiders      -4.313787   2.499149  -1.726   0.0852 .  
    ## OpponentPhiladelphia Eagles  -1.415159   2.553484  -0.554   0.5798    
    ## OpponentPittsburgh Steelers   4.381643   2.680862   1.634   0.1031    
    ## OpponentSan Francisco 49ers   1.609397   2.653412   0.607   0.5445    
    ## OpponentSeattle Seahawks     -0.077386   2.474730  -0.031   0.9751    
    ## OpponentTampa Bay Buccaneers -0.339383   2.615968  -0.130   0.8968    
    ## OpponentTennessee Titans     -0.925922   2.578951  -0.359   0.7198    
    ## OpponentWashington Redskins  -2.061791   2.592310  -0.795   0.4269    
    ## TP                            0.012764   0.005071   2.517   0.0123 *  
    ## TR                           -0.003459   0.007509  -0.461   0.6453    
    ## TT                            2.177297   0.292810   7.436 7.82e-13 ***
    ## OP                            0.065040   0.004973  13.078  < 2e-16 ***
    ## OR                            0.074475   0.007693   9.680  < 2e-16 ***
    ## OT                           -1.896308   0.299748  -6.326 7.54e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.421 on 356 degrees of freedom
    ## Multiple R-squared:  0.6711, Adjusted R-squared:  0.6082 
    ## F-statistic: 10.68 on 68 and 356 DF,  p-value: < 2.2e-16

``` r
anova(m2)
```

    ## Analysis of Variance Table
    ## 
    ##              Df  Pillai approx F num Df den Df    Pr(>F)    
    ## (Intercept)   1 0.95945   4200.0      2    355 < 2.2e-16 ***
    ## Team         31 0.62815      5.3     62    712 < 2.2e-16 ***
    ## Opponent     31 0.68251      5.9     62    712 < 2.2e-16 ***
    ## TP            1 0.24127     56.4      2    355 < 2.2e-16 ***
    ## TR            1 0.35286     96.8      2    355 < 2.2e-16 ***
    ## TT            1 0.29828     75.4      2    355 < 2.2e-16 ***
    ## OP            1 0.23043     53.1      2    355 < 2.2e-16 ***
    ## OR            1 0.27138     66.1      2    355 < 2.2e-16 ***
    ## OT            1 0.22540     51.7      2    355 < 2.2e-16 ***
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

![](Super_Bowl_LIII_files/figure-markdown_github/valerr-1.png)

    ## [1] "Average RMSE is"

    ## [1] 5.928667

#### 3.4 Testing!

The next step to this supervised learning model is to run this model on our testing set and check the accuracy for it.

``` r
valt <- data.frame(predict(m2, testing, type = "response"))
names(valt) = c("predTS", "predOS")
testing <- cbind.data.frame(testing, valt)

error_plot(testing)
```

![](Super_Bowl_LIII_files/figure-markdown_github/testerr-1.png)

    ## [1] "Average RMSE is"

    ## [1] 5.940066

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
    ## 1 122.865 1.415 Average-Avg    25.85405    24.88905
    ## 2 102.980 0.970     Bad-Avg    21.62689    20.23550
    ## 3 273.000 4.000   Best-Best    44.55448    50.49399

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
