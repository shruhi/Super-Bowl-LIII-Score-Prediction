Super Bowl Prediction
================

Multivariate Multiple Regression to predict the Super Bowl LIII
---------------------------------------------------------------

### 1. Looking at the data

The variables are:

Team: Team name
Opponent: opponent team name
location: home/away
TS: team score (to predict)
OS: opponent score (to predict)
TP: team passing (in yards) TR: team rushing (in yards) TY: total yards TT: Team turnover
OP: opponent passing (in yards) OR: opponent rushing (in yards) OY: total yards OT: opponent turnover

Make sure no missing data.
An intital exploratory look using pairs()
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

In order to do a linear regression, the following assumptions are made: 
> Linear Relatioship

The dependent variables and independent variables have a linear relatioship.

> Homoscedasticity

It means that the variance of the dependent variable(Y) is the same for all values of predictor variables (X) To check both of these, we use scatterplots.

### For Team Score

![](https://github.com/shruhi/Super-Bowl-LIII-Score-Prediction/blob/master/TStest-1.png)

#### For Opponent Score

![](https://github.com/shruhi/Super-Bowl-LIII-Score-Prediction/blob/master/OSTest-1.png)

> Multicolinearity

If any of the variables are correlated, it gives causes a problem in regression models. To verfiy that they are not correlate, use a simple correlation matrix.

![](https://github.com/shruhi/Super-Bowl-LIII-Score-Prediction/blob/master/corrplot-1.png)

Total yards for both team and opponent are highly correlated with passing yards. We won't be using them in our model.

Requirements to do a linear regression are satisfied!

### 3. Multivariate Regression

#### 3.1 Creating training, validation and testing sets

This is a supervised machine-learning method. The data is split 80%-10%-10% into training, validation and testing sets. 

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

Dependent variables: Team and Opponent Score 
Predictors: Team Statistics 
Using anova() we test the covariance between the coefficients in the two models and remove the non-useful predictors.

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
    ##     Min      1Q  Median      3Q     Max 
    ## -20.315  -3.885  -0.053   3.617  22.199 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -4.608305   3.309679  -1.392  0.16468    
    ## TeamAtlanta Falcons           2.244594   2.733792   0.821  0.41216    
    ## TeamBaltimore Ravens          1.026821   2.888634   0.355  0.72245    
    ## TeamBuffalo Bills            -1.628296   2.719408  -0.599  0.54971    
    ## TeamCarolina Panthers        -0.651206   2.724031  -0.239  0.81120    
    ## TeamChicago Bears             0.552277   2.833281   0.195  0.84556    
    ## TeamCincinnati Bengals        4.018364   2.667000   1.507  0.13277    
    ## TeamCleveland Browns         -2.522134   3.048830  -0.827  0.40865    
    ## TeamDallas Cowboys           -0.692757   2.612973  -0.265  0.79107    
    ## TeamDenver Broncos           -0.736417   2.877974  -0.256  0.79819    
    ## TeamDetroit Lions             0.047727   2.533623   0.019  0.98498    
    ## TeamGreen Bay Packers        -0.679161   2.835064  -0.240  0.81081    
    ## TeamHouston Texans            0.332258   2.670664   0.124  0.90106    
    ## TeamIndianapolis Colts        1.801168   2.679921   0.672  0.50196    
    ## TeamJacksonville Jaguars     -0.114938   2.718677  -0.042  0.96630    
    ## TeamKansas City Chiefs        8.703770   2.636474   3.301  0.00106 ** 
    ## TeamLos Angeles Chargers      3.538465   2.669511   1.326  0.18585    
    ## TeamLos Angeles Rams          4.036764   2.684443   1.504  0.13353    
    ## TeamMiami Dolphins            2.090097   2.812617   0.743  0.45790    
    ## TeamMinnesota Vikings         1.400335   2.683858   0.522  0.60216    
    ## TeamNew England Patriots      0.482209   2.649660   0.182  0.85570    
    ## TeamNew Orleans Saints        5.022811   2.709901   1.854  0.06464 .  
    ## TeamNew York Giants           1.619635   2.841075   0.570  0.56898    
    ## TeamNew York Jets             2.851828   2.645090   1.078  0.28169    
    ## TeamOakland Raiders          -1.789929   2.735414  -0.654  0.51331    
    ## TeamPhiladelphia Eagles       0.287279   2.614193   0.110  0.91256    
    ## TeamPittsburgh Steelers       3.450294   2.827862   1.220  0.22323    
    ## TeamSan Francisco 49ers       1.086290   2.615144   0.415  0.67811    
    ## TeamSeattle Seahawks          1.537531   2.571029   0.598  0.55021    
    ## TeamTampa Bay Buccaneers     -0.232750   2.922167  -0.080  0.93656    
    ## TeamTennessee Titans          0.876512   2.590312   0.338  0.73528    
    ## TeamWashington Redskins      -1.825573   2.618729  -0.697  0.48618    
    ## OpponentAtlanta Falcons      -1.131960   2.829966  -0.400  0.68940    
    ## OpponentBaltimore Ravens     -3.281709   2.777307  -1.182  0.23815    
    ## OpponentBuffalo Bills         0.029294   2.723643   0.011  0.99142    
    ## OpponentCarolina Panthers    -1.405313   2.761576  -0.509  0.61115    
    ## OpponentChicago Bears        -2.199482   2.870731  -0.766  0.44408    
    ## OpponentCincinnati Bengals   -1.919870   2.759620  -0.696  0.48707    
    ## OpponentCleveland Browns     -5.075952   2.941865  -1.725  0.08532 .  
    ## OpponentDallas Cowboys       -2.135358   2.561418  -0.834  0.40503    
    ## OpponentDenver Broncos       -4.887553   2.757734  -1.772  0.07720 .  
    ## OpponentDetroit Lions        -2.145328   2.803975  -0.765  0.44472    
    ## OpponentGreen Bay Packers     0.226526   2.718672   0.083  0.93364    
    ## OpponentHouston Texans       -2.442520   2.803839  -0.871  0.38427    
    ## OpponentIndianapolis Colts   -3.278834   2.667247  -1.229  0.21977    
    ## OpponentJacksonville Jaguars -4.254335   2.821566  -1.508  0.13249    
    ## OpponentKansas City Chiefs   -1.686785   2.780269  -0.607  0.54444    
    ## OpponentLos Angeles Chargers -2.288793   2.892030  -0.791  0.42923    
    ## OpponentLos Angeles Rams     -0.745899   2.717335  -0.274  0.78386    
    ## OpponentMiami Dolphins        0.730902   2.608059   0.280  0.77945    
    ## OpponentMinnesota Vikings    -1.800416   2.708474  -0.665  0.50665    
    ## OpponentNew England Patriots -4.231789   2.735867  -1.547  0.12280    
    ## OpponentNew Orleans Saints   -1.822939   2.815405  -0.647  0.51773    
    ## OpponentNew York Giants       0.182842   2.877508   0.064  0.94937    
    ## OpponentNew York Jets        -0.035603   2.649592  -0.013  0.98929    
    ## OpponentOakland Raiders      -1.368023   2.747611  -0.498  0.61887    
    ## OpponentPhiladelphia Eagles  -6.024887   2.624560  -2.296  0.02228 *  
    ## OpponentPittsburgh Steelers  -4.431272   2.941148  -1.507  0.13279    
    ## OpponentSan Francisco 49ers   0.167585   2.711552   0.062  0.95075    
    ## OpponentSeattle Seahawks     -1.919929   2.618264  -0.733  0.46387    
    ## OpponentTampa Bay Buccaneers -3.442014   2.895243  -1.189  0.23529    
    ## OpponentTennessee Titans     -4.342178   2.740253  -1.585  0.11395    
    ## OpponentWashington Redskins  -1.950588   2.770506  -0.704  0.48186    
    ## TP                            0.066693   0.005088  13.109  < 2e-16 ***
    ## TR                            0.077784   0.007565  10.282  < 2e-16 ***
    ## TT                           -1.619917   0.306827  -5.280 2.26e-07 ***
    ## OP                            0.014541   0.004844   3.002  0.00287 ** 
    ## OR                           -0.003805   0.007601  -0.501  0.61697    
    ## OT                            2.256358   0.299440   7.535 4.07e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.563 on 356 degrees of freedom
    ## Multiple R-squared:  0.6648, Adjusted R-squared:  0.6007 
    ## F-statistic: 10.38 on 68 and 356 DF,  p-value: < 2.2e-16
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
    ## -21.0759  -3.9444   0.0145   3.9994  18.7514 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -5.056860   3.262595  -1.550  0.12204    
    ## TeamAtlanta Falcons          -1.299941   2.694900  -0.482  0.62984    
    ## TeamBaltimore Ravens         -2.078503   2.847539  -0.730  0.46591    
    ## TeamBuffalo Bills             3.337965   2.680721   1.245  0.21389    
    ## TeamCarolina Panthers        -2.490541   2.685278  -0.927  0.35431    
    ## TeamChicago Bears            -0.260833   2.792973  -0.093  0.92565    
    ## TeamCincinnati Bengals       -1.514741   2.629058  -0.576  0.56488    
    ## TeamCleveland Browns         -2.167310   3.005456  -0.721  0.47131    
    ## TeamDallas Cowboys           -1.747987   2.575800  -0.679  0.49782    
    ## TeamDenver Broncos           -3.349395   2.837031  -1.181  0.23855    
    ## TeamDetroit Lions            -1.257599   2.497579  -0.504  0.61491    
    ## TeamGreen Bay Packers        -0.645490   2.794732  -0.231  0.81747    
    ## TeamHouston Texans           -1.636757   2.632670  -0.622  0.53453    
    ## TeamIndianapolis Colts       -3.666194   2.641796  -1.388  0.16608    
    ## TeamJacksonville Jaguars     -2.332034   2.680000  -0.870  0.38480    
    ## TeamKansas City Chiefs       -1.377573   2.598966  -0.530  0.59641    
    ## TeamLos Angeles Chargers     -2.967465   2.631534  -1.128  0.26022    
    ## TeamLos Angeles Rams         -0.113351   2.646253  -0.043  0.96586    
    ## TeamMiami Dolphins           -0.358594   2.772604  -0.129  0.89717    
    ## TeamMinnesota Vikings        -0.275216   2.645676  -0.104  0.91721    
    ## TeamNew England Patriots     -4.116709   2.611965  -1.576  0.11589    
    ## TeamNew Orleans Saints       -1.639435   2.671349  -0.614  0.53980    
    ## TeamNew York Giants           0.544413   2.800657   0.194  0.84598    
    ## TeamNew York Jets             1.165545   2.607460   0.447  0.65514    
    ## TeamOakland Raiders           0.729216   2.696499   0.270  0.78699    
    ## TeamPhiladelphia Eagles      -6.360217   2.577003  -2.468  0.01405 *  
    ## TeamPittsburgh Steelers      -2.479971   2.787632  -0.890  0.37426    
    ## TeamSan Francisco 49ers      -1.268526   2.577940  -0.492  0.62297    
    ## TeamSeattle Seahawks         -1.438636   2.534453  -0.568  0.57064    
    ## TeamTampa Bay Buccaneers     -0.838143   2.880595  -0.291  0.77125    
    ## TeamTennessee Titans         -3.264333   2.553462  -1.278  0.20194    
    ## TeamWashington Redskins      -0.698420   2.581474  -0.271  0.78689    
    ## OpponentAtlanta Falcons       1.522780   2.789706   0.546  0.58551    
    ## OpponentBaltimore Ravens      2.126528   2.737796   0.777  0.43783    
    ## OpponentBuffalo Bills        -2.072948   2.684896  -0.772  0.44058    
    ## OpponentCarolina Panthers    -2.979112   2.722289  -1.094  0.27455    
    ## OpponentChicago Bears         1.257698   2.829891   0.444  0.65700    
    ## OpponentCincinnati Bengals    3.870665   2.720361   1.423  0.15566    
    ## OpponentCleveland Browns     -0.443478   2.900013  -0.153  0.87855    
    ## OpponentDallas Cowboys       -0.849269   2.524979  -0.336  0.73681    
    ## OpponentDenver Broncos       -4.308835   2.718502  -1.585  0.11385    
    ## OpponentDetroit Lions        -0.608108   2.764085  -0.220  0.82599    
    ## OpponentGreen Bay Packers    -1.962198   2.679996  -0.732  0.46455    
    ## OpponentHouston Texans       -1.893716   2.763951  -0.685  0.49370    
    ## OpponentIndianapolis Colts    0.562377   2.629302   0.214  0.83076    
    ## OpponentJacksonville Jaguars -0.668581   2.781425  -0.240  0.81018    
    ## OpponentKansas City Chiefs    6.573138   2.740716   2.398  0.01698 *  
    ## OpponentLos Angeles Chargers  2.646249   2.850887   0.928  0.35392    
    ## OpponentLos Angeles Rams      3.256803   2.678677   1.216  0.22486    
    ## OpponentMiami Dolphins       -0.081060   2.570956  -0.032  0.97487    
    ## OpponentMinnesota Vikings     0.949893   2.669942   0.356  0.72222    
    ## OpponentNew England Patriots -0.410948   2.696945  -0.152  0.87898    
    ## OpponentNew Orleans Saints    3.619122   2.775352   1.304  0.19307    
    ## OpponentNew York Giants       0.836261   2.836571   0.295  0.76831    
    ## OpponentNew York Jets         1.946643   2.611898   0.745  0.45658    
    ## OpponentOakland Raiders      -3.732411   2.708522  -1.378  0.16906    
    ## OpponentPhiladelphia Eagles  -0.506083   2.587222  -0.196  0.84503    
    ## OpponentPittsburgh Steelers   1.781637   2.899306   0.615  0.53927    
    ## OpponentSan Francisco 49ers   2.152780   2.672977   0.805  0.42113    
    ## OpponentSeattle Seahawks      0.221457   2.581016   0.086  0.93167    
    ## OpponentTampa Bay Buccaneers -1.077450   2.854054  -0.378  0.70602    
    ## OpponentTennessee Titans     -0.419893   2.701269  -0.155  0.87656    
    ## OpponentWashington Redskins  -3.113025   2.731092  -1.140  0.25512    
    ## TP                            0.014472   0.005015   2.885  0.00415 ** 
    ## TR                           -0.008597   0.007457  -1.153  0.24978    
    ## TT                            2.441886   0.302462   8.073 1.07e-14 ***
    ## OP                            0.069484   0.004775  14.552  < 2e-16 ***
    ## OR                            0.085068   0.007493  11.353  < 2e-16 ***
    ## OT                           -1.742776   0.295180  -5.904 8.27e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.47 on 356 degrees of freedom
    ## Multiple R-squared:  0.6858, Adjusted R-squared:  0.6258 
    ## F-statistic: 11.43 on 68 and 356 DF,  p-value: < 2.2e-16

``` r
anova(m2)
```

    ## Analysis of Variance Table
    ## 
    ##              Df  Pillai approx F num Df den Df    Pr(>F)    
    ## (Intercept)   1 0.96026   4289.2      2    355 < 2.2e-16 ***
    ## Team         31 0.66235      5.7     62    712 < 2.2e-16 ***
    ## Opponent     31 0.64214      5.4     62    712 < 2.2e-16 ***
    ## TP            1 0.25764     61.6      2    355 < 2.2e-16 ***
    ## TR            1 0.37647    107.2      2    355 < 2.2e-16 ***
    ## TT            1 0.25180     59.7      2    355 < 2.2e-16 ***
    ## OP            1 0.26510     64.0      2    355 < 2.2e-16 ***
    ## OR            1 0.31367     81.1      2    355 < 2.2e-16 ***
    ## OT            1 0.25246     59.9      2    355 < 2.2e-16 ***
    ## Residuals   356                                             
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### 3.3 Validate it!

To evaluate validation set, the average of root mean square error (RMSE) is used.
The errors, i.e. variance is expected to be normally distributed.

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

    ## [1] 6.034522

#### 3.4 Testing!

Final step - testing and finding accuracy using RMSE.

``` r
valt <- data.frame(predict(m2, testing, type = "response"))
names(valt) = c("predTS", "predOS")
testing <- cbind.data.frame(testing, valt)

error_plot(testing)
```

![](https://github.com/shruhi/Super-Bowl-LIII-Score-Prediction/blob/master/testerr-1.png)

    ## [1] "Average RMSE is"

    ## [1] 5.730586

Lower error on the testing set!

### 4. Prediction

> What-if Scenarios

First let's start with the obvious. Take the average statistics for both teams and try to predict the outcome.
Second, considering experience advantage, take an average performace by Patriots and the 1st quartile performance of Rams.
Next, a best case scenario where both teams give their best performance.

To really predict an avergae performance, consensus scores are formulated. E.g. the average Team passing yards for Rams would be an average of TP(Rams) and OP(Patriots) and so forth. These data manipulations are done in excel and the csv can be imported for predictions to be made.

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
    ## 1 122.865 1.415 Average-Avg    26.60321    26.37036
    ## 2 102.980 0.970     Bad-Avg    21.75482    21.16674
    ## 3 273.000 4.000   Best-Best    48.84361    55.18534

``` r
  datafr <- data.frame(scenarios[1,10:11])
  fr <- data.frame()
  fr <- rbind(fr, datafr)
```

Final Prediction
----------------
For this prediction, consider average performance of both teams this season.

For more accuracy, the model is iterated 5 more times to get five final predictions and the average is taken for a final prediction:

    ##    X PredictedTS PredictedOS
    ## 1  1    28.18823    25.25942
    ## 2 11    27.12906    23.46251
    ## 3 12    25.70178    24.65486
    ## 4 13    27.15497    26.51249
    ## 5 14    27.05595    27.25775

    ##   X Rams.Score Patriots.Score
    ## 1 1     27.046       25.42941
