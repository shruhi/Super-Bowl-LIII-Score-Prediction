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
    ## -19.8854  -3.6616   0.3273   3.6887  17.4710 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -5.126548   3.382498  -1.516   0.1305    
    ## TeamAtlanta Falcons           0.865903   2.827493   0.306   0.7596    
    ## TeamBaltimore Ravens         -0.049859   2.733342  -0.018   0.9855    
    ## TeamBuffalo Bills            -2.884724   2.706869  -1.066   0.2873    
    ## TeamCarolina Panthers        -1.761300   2.839251  -0.620   0.5354    
    ## TeamChicago Bears             0.544416   2.690828   0.202   0.8398    
    ## TeamCincinnati Bengals        4.375839   2.784515   1.571   0.1170    
    ## TeamCleveland Browns         -3.003653   2.688836  -1.117   0.2647    
    ## TeamDallas Cowboys           -0.555374   2.704989  -0.205   0.8374    
    ## TeamDenver Broncos           -4.404600   2.821650  -1.561   0.1194    
    ## TeamDetroit Lions            -0.510607   2.737660  -0.187   0.8521    
    ## TeamGreen Bay Packers        -0.875749   2.833805  -0.309   0.7575    
    ## TeamHouston Texans           -0.367888   2.731737  -0.135   0.8929    
    ## TeamIndianapolis Colts        1.057091   2.803087   0.377   0.7063    
    ## TeamJacksonville Jaguars     -2.626541   2.791131  -0.941   0.3473    
    ## TeamKansas City Chiefs        6.765927   2.646054   2.557   0.0110 *  
    ## TeamLos Angeles Chargers      2.646638   2.729071   0.970   0.3328    
    ## TeamLos Angeles Rams          4.734194   2.855366   1.658   0.0982 .  
    ## TeamMiami Dolphins           -0.173421   2.640617  -0.066   0.9477    
    ## TeamMinnesota Vikings        -0.745475   2.733509  -0.273   0.7852    
    ## TeamNew England Patriots     -0.910910   2.676382  -0.340   0.7338    
    ## TeamNew Orleans Saints        4.712795   2.795463   1.686   0.0927 .  
    ## TeamNew York Giants           1.455695   2.793778   0.521   0.6027    
    ## TeamNew York Jets             0.346904   2.754194   0.126   0.8998    
    ## TeamOakland Raiders          -1.779206   3.069984  -0.580   0.5626    
    ## TeamPhiladelphia Eagles      -1.328978   2.759949  -0.482   0.6304    
    ## TeamPittsburgh Steelers       2.104625   2.817877   0.747   0.4556    
    ## TeamSan Francisco 49ers       1.516286   2.754159   0.551   0.5823    
    ## TeamSeattle Seahawks          0.585574   2.730038   0.214   0.8303    
    ## TeamTampa Bay Buccaneers     -0.941684   2.769838  -0.340   0.7341    
    ## TeamTennessee Titans         -0.489427   2.686685  -0.182   0.8556    
    ## TeamWashington Redskins      -0.350608   2.799352  -0.125   0.9004    
    ## OpponentAtlanta Falcons       3.517678   2.950989   1.192   0.2340    
    ## OpponentBaltimore Ravens     -0.523656   2.770785  -0.189   0.8502    
    ## OpponentBuffalo Bills         6.247599   2.786976   2.242   0.0256 *  
    ## OpponentCarolina Panthers     1.239037   2.855206   0.434   0.6646    
    ## OpponentChicago Bears         1.410058   2.719824   0.518   0.6045    
    ## OpponentCincinnati Bengals    2.766983   2.762498   1.002   0.3172    
    ## OpponentCleveland Browns     -1.240641   2.816106  -0.441   0.6598    
    ## OpponentDallas Cowboys        1.407593   2.742840   0.513   0.6081    
    ## OpponentDenver Broncos       -1.155348   2.728565  -0.423   0.6722    
    ## OpponentDetroit Lions         1.597834   2.722446   0.587   0.5576    
    ## OpponentGreen Bay Packers     2.105126   2.822929   0.746   0.4563    
    ## OpponentHouston Texans        1.990777   3.029228   0.657   0.5115    
    ## OpponentIndianapolis Colts   -0.693819   2.754145  -0.252   0.8012    
    ## OpponentJacksonville Jaguars -1.253949   2.737363  -0.458   0.6472    
    ## OpponentKansas City Chiefs    2.054135   2.833087   0.725   0.4689    
    ## OpponentLos Angeles Chargers  0.506250   2.816866   0.180   0.8575    
    ## OpponentLos Angeles Rams      2.212622   2.703081   0.819   0.4136    
    ## OpponentMiami Dolphins        3.300458   2.805891   1.176   0.2403    
    ## OpponentMinnesota Vikings     0.413526   2.849855   0.145   0.8847    
    ## OpponentNew England Patriots -0.060297   2.750241  -0.022   0.9825    
    ## OpponentNew Orleans Saints    1.824516   3.081440   0.592   0.5542    
    ## OpponentNew York Giants       3.105605   2.745672   1.131   0.2588    
    ## OpponentNew York Jets         3.133113   2.849451   1.100   0.2723    
    ## OpponentOakland Raiders       3.473412   2.882211   1.205   0.2290    
    ## OpponentPhiladelphia Eagles  -3.030111   2.773193  -1.093   0.2753    
    ## OpponentPittsburgh Steelers  -0.573455   2.991013  -0.192   0.8481    
    ## OpponentSan Francisco 49ers   1.560707   2.705871   0.577   0.5644    
    ## OpponentSeattle Seahawks      1.541158   2.765227   0.557   0.5776    
    ## OpponentTampa Bay Buccaneers  0.015818   2.944354   0.005   0.9957    
    ## OpponentTennessee Titans      0.615402   2.821026   0.218   0.8274    
    ## OpponentWashington Redskins   0.918930   2.914950   0.315   0.7528    
    ## TP                            0.065274   0.004869  13.406  < 2e-16 ***
    ## TR                            0.076338   0.007261  10.513  < 2e-16 ***
    ## TT                           -1.842118   0.300103  -6.138 2.23e-09 ***
    ## OP                            0.012319   0.004767   2.584   0.0102 *  
    ## OR                           -0.004595   0.007399  -0.621   0.5349    
    ## OT                            2.250215   0.291226   7.727 1.14e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.431 on 356 degrees of freedom
    ## Multiple R-squared:  0.6682, Adjusted R-squared:  0.6048 
    ## F-statistic: 10.54 on 68 and 356 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ## Response OS :
    ## 
    ## Call:
    ## lm(formula = OS ~ Team + Opponent + TP + TR + TT + OP + OR + 
    ##     OT, data = training)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -14.558  -3.723  -0.009   3.705  18.565 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -4.412897   3.352891  -1.316  0.18897    
    ## TeamAtlanta Falcons          -0.738012   2.802743  -0.263  0.79246    
    ## TeamBaltimore Ravens         -2.819506   2.709416  -1.041  0.29875    
    ## TeamBuffalo Bills             2.764562   2.683175   1.030  0.30355    
    ## TeamCarolina Panthers        -0.324423   2.814399  -0.115  0.90829    
    ## TeamChicago Bears            -0.628798   2.667275  -0.236  0.81377    
    ## TeamCincinnati Bengals       -0.997604   2.760141  -0.361  0.71799    
    ## TeamCleveland Browns         -2.942784   2.665300  -1.104  0.27029    
    ## TeamDallas Cowboys           -1.600108   2.681312  -0.597  0.55104    
    ## TeamDenver Broncos           -3.970514   2.796952  -1.420  0.15660    
    ## TeamDetroit Lions            -0.263935   2.713696  -0.097  0.92257    
    ## TeamGreen Bay Packers         2.491574   2.809000   0.887  0.37568    
    ## TeamHouston Texans           -1.048631   2.707825  -0.387  0.69880    
    ## TeamIndianapolis Colts       -3.632120   2.778551  -1.307  0.19199    
    ## TeamJacksonville Jaguars     -1.397727   2.766700  -0.505  0.61373    
    ## TeamKansas City Chiefs       -0.806471   2.622893  -0.307  0.75866    
    ## TeamLos Angeles Chargers     -1.239725   2.705183  -0.458  0.64703    
    ## TeamLos Angeles Rams          1.711943   2.830373   0.605  0.54567    
    ## TeamMiami Dolphins            1.406479   2.617503   0.537  0.59137    
    ## TeamMinnesota Vikings        -0.480932   2.709582  -0.177  0.85922    
    ## TeamNew England Patriots     -2.864687   2.652955  -1.080  0.28096    
    ## TeamNew Orleans Saints       -0.464112   2.770994  -0.167  0.86708    
    ## TeamNew York Giants           2.444911   2.769324   0.883  0.37791    
    ## TeamNew York Jets             0.160208   2.730086   0.059  0.95324    
    ## TeamOakland Raiders           1.871458   3.043112   0.615  0.53896    
    ## TeamPhiladelphia Eagles      -5.741303   2.735791  -2.099  0.03656 *  
    ## TeamPittsburgh Steelers      -3.008367   2.793212  -1.077  0.28220    
    ## TeamSan Francisco 49ers      -0.887427   2.730051  -0.325  0.74533    
    ## TeamSeattle Seahawks         -0.340553   2.706141  -0.126  0.89993    
    ## TeamTampa Bay Buccaneers     -1.495634   2.745593  -0.545  0.58627    
    ## TeamTennessee Titans         -3.458978   2.663168  -1.299  0.19485    
    ## TeamWashington Redskins       1.261815   2.774849   0.455  0.64958    
    ## OpponentAtlanta Falcons       3.461617   2.925158   1.183  0.23744    
    ## OpponentBaltimore Ravens      2.890054   2.746532   1.052  0.29340    
    ## OpponentBuffalo Bills         1.281876   2.762581   0.464  0.64292    
    ## OpponentCarolina Panthers     2.354713   2.830214   0.832  0.40597    
    ## OpponentChicago Bears         3.503325   2.696017   1.299  0.19463    
    ## OpponentCincinnati Bengals    5.779460   2.738317   2.111  0.03550 *  
    ## OpponentCleveland Browns      0.876996   2.791456   0.314  0.75357    
    ## OpponentDallas Cowboys        1.193852   2.718832   0.439  0.66085    
    ## OpponentDenver Broncos       -1.014507   2.704681  -0.375  0.70781    
    ## OpponentDetroit Lions         1.414091   2.698616   0.524  0.60060    
    ## OpponentGreen Bay Packers     0.336968   2.798219   0.120  0.90422    
    ## OpponentHouston Texans        4.001889   3.002713   1.333  0.18346    
    ## OpponentIndianapolis Colts    3.676228   2.730037   1.347  0.17897    
    ## OpponentJacksonville Jaguars  0.556980   2.713402   0.205  0.83748    
    ## OpponentKansas City Chiefs    8.717115   2.808288   3.104  0.00206 ** 
    ## OpponentLos Angeles Chargers  6.199469   2.792209   2.220  0.02703 *  
    ## OpponentLos Angeles Rams      5.766260   2.679421   2.152  0.03206 *  
    ## OpponentMiami Dolphins        3.266679   2.781330   1.175  0.24098    
    ## OpponentMinnesota Vikings     2.465629   2.824910   0.873  0.38335    
    ## OpponentNew England Patriots  1.915936   2.726167   0.703  0.48264    
    ## OpponentNew Orleans Saints    8.515852   3.054467   2.788  0.00559 ** 
    ## OpponentNew York Giants       3.283758   2.721638   1.207  0.22841    
    ## OpponentNew York Jets         5.068439   2.824510   1.794  0.07359 .  
    ## OpponentOakland Raiders       1.979642   2.856982   0.693  0.48882    
    ## OpponentPhiladelphia Eagles   1.786759   2.748919   0.650  0.51612    
    ## OpponentPittsburgh Steelers   4.296154   2.964832   1.449  0.14821    
    ## OpponentSan Francisco 49ers   3.233480   2.682186   1.206  0.22880    
    ## OpponentSeattle Seahawks      2.802083   2.741022   1.022  0.30734    
    ## OpponentTampa Bay Buccaneers  3.578070   2.918581   1.226  0.22102    
    ## OpponentTennessee Titans      3.236843   2.796333   1.158  0.24783    
    ## OpponentWashington Redskins  -2.035609   2.889435  -0.705  0.48158    
    ## TP                            0.005587   0.004826   1.158  0.24778    
    ## TR                           -0.008681   0.007198  -1.206  0.22855    
    ## TT                            2.258167   0.297476   7.591 2.81e-13 ***
    ## OP                            0.067749   0.004725  14.337  < 2e-16 ***
    ## OR                            0.077952   0.007334  10.629  < 2e-16 ***
    ## OT                           -1.960612   0.288677  -6.792 4.66e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.374 on 356 degrees of freedom
    ## Multiple R-squared:  0.6624, Adjusted R-squared:  0.5979 
    ## F-statistic: 10.27 on 68 and 356 DF,  p-value: < 2.2e-16

``` r
anova(m2)
```

    ## Analysis of Variance Table
    ## 
    ##              Df  Pillai approx F num Df den Df    Pr(>F)    
    ## (Intercept)   1 0.96282   4596.5      2    355 < 2.2e-16 ***
    ## Team         31 0.66583      5.7     62    712 < 2.2e-16 ***
    ## Opponent     31 0.56511      4.5     62    712 < 2.2e-16 ***
    ## TP            1 0.24156     56.5      2    355 < 2.2e-16 ***
    ## TR            1 0.37191    105.1      2    355 < 2.2e-16 ***
    ## TT            1 0.25264     60.0      2    355 < 2.2e-16 ***
    ## OP            1 0.25495     60.7      2    355 < 2.2e-16 ***
    ## OR            1 0.27915     68.7      2    355 < 2.2e-16 ***
    ## OT            1 0.28122     69.4      2    355 < 2.2e-16 ***
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

    ## [1] 6.587461

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

    ## [1] 6.242899

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
    ## 1 122.865 1.415 Average-Avg    29.41641    26.92847
    ## 2 102.980 0.970     Bad-Avg    24.90215    22.50805
    ## 3 273.000 4.000   Best-Best    50.15841    51.95037

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
