Project II ST558
================
Pratap Adhikari
10/9/2020

``` r
popData<- read_csv("OnlineNewsPopularity.csv")
popData
```

    ## # A tibble: 39,644 x 61
    ##    url   timedelta n_tokens_title n_tokens_content n_unique_tokens
    ##    <chr>     <dbl>          <dbl>            <dbl>           <dbl>
    ##  1 http~       731             12              219           0.664
    ##  2 http~       731              9              255           0.605
    ##  3 http~       731              9              211           0.575
    ##  4 http~       731              9              531           0.504
    ##  5 http~       731             13             1072           0.416
    ##  6 http~       731             10              370           0.560
    ##  7 http~       731              8              960           0.418
    ##  8 http~       731             12              989           0.434
    ##  9 http~       731             11               97           0.670
    ## 10 http~       731             10              231           0.636
    ## # ... with 39,634 more rows, and 56 more variables: n_non_stop_words <dbl>,
    ## #   n_non_stop_unique_tokens <dbl>, num_hrefs <dbl>, num_self_hrefs <dbl>,
    ## #   num_imgs <dbl>, num_videos <dbl>, average_token_length <dbl>,
    ## #   num_keywords <dbl>, data_channel_is_lifestyle <dbl>,
    ## #   data_channel_is_entertainment <dbl>, data_channel_is_bus <dbl>,
    ## #   data_channel_is_socmed <dbl>, data_channel_is_tech <dbl>,
    ## #   data_channel_is_world <dbl>, kw_min_min <dbl>, kw_max_min <dbl>,
    ## #   kw_avg_min <dbl>, kw_min_max <dbl>, kw_max_max <dbl>, kw_avg_max <dbl>,
    ## #   kw_min_avg <dbl>, kw_max_avg <dbl>, kw_avg_avg <dbl>,
    ## #   self_reference_min_shares <dbl>, self_reference_max_shares <dbl>,
    ## #   self_reference_avg_sharess <dbl>, weekday_is_monday <dbl>,
    ## #   weekday_is_tuesday <dbl>, weekday_is_wednesday <dbl>,
    ## #   weekday_is_thursday <dbl>, weekday_is_friday <dbl>,
    ## #   weekday_is_saturday <dbl>, weekday_is_sunday <dbl>, is_weekend <dbl>,
    ## #   LDA_00 <dbl>, LDA_01 <dbl>, LDA_02 <dbl>, LDA_03 <dbl>, LDA_04 <dbl>,
    ## #   global_subjectivity <dbl>, global_sentiment_polarity <dbl>,
    ## #   global_rate_positive_words <dbl>, global_rate_negative_words <dbl>,
    ## #   rate_positive_words <dbl>, rate_negative_words <dbl>,
    ## #   avg_positive_polarity <dbl>, min_positive_polarity <dbl>,
    ## #   max_positive_polarity <dbl>, avg_negative_polarity <dbl>,
    ## #   min_negative_polarity <dbl>, max_negative_polarity <dbl>,
    ## #   title_subjectivity <dbl>, title_sentiment_polarity <dbl>,
    ## #   abs_title_subjectivity <dbl>, abs_title_sentiment_polarity <dbl>,
    ## #   shares <dbl>

``` r
#day<- function(day, ...){}
```

### Processing

``` r
# checking if any missing values in the raw data
anyNA(popData)
```

    ## [1] FALSE

No missing values were found.

``` r
# Create train and test data set.
set.seed(2)
train<- sample(1:nrow(popData), size = nrow(popData) *0.7)
test<- dplyr::setdiff(1:nrow(popData), train)

popDataTrain<- popData[train, -1]
popDataTest<- popData[test,-1]
```

### Trai the model

``` r
trCtrl<- trainControl(method="repeatedcv", number=10, repeats=3)
knnFit<- train(shares ~., data=popDataTrain, method="knn",
               trControl=trCtrl,
               preProcess=c("center", "scale"),
               tunelength=10)

knnFit
```

    ## k-Nearest Neighbors 
    ## 
    ## 27750 samples
    ##    59 predictor
    ## 
    ## Pre-processing: centered (59), scaled (59) 
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 24975, 24975, 24975, 24975, 24974, 24975, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   k  RMSE      Rsquared     MAE     
    ##   5  11237.32  0.004132921  3274.174
    ##   7  10941.08  0.005447601  3185.991
    ##   9  10814.72  0.006284535  3148.626
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was k = 9.

### Plot of k vs RMSE

The best number value for neighbors given by the model fit is 9.

``` r
kacc<- tbl_df(knnFit$results[, 1:2])
ggplot(kacc, aes(x=k, y=RMSE)) + geom_point( color="blue") +
  geom_line(color="green") +
  ggtitle(" k vs. Accuracy")
```

![](Project-II_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#Prediction and post Resampling
pred<- predict(knnFit, newdata=popDataTest)
postResample(pred, obs=popDataTest$shares)
```

    ##         RMSE     Rsquared          MAE 
    ## 1.303683e+04 4.556890e-03 3.017838e+03
