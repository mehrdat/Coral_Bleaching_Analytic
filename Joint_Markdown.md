
## Description of the Data

first od all I get a structure to have an idea about the dataset.

``` r
str(coral)
```

    ## 'data.frame':    41361 obs. of  18 variables:
    ##  $ Sample_ID          : int  10324336 10324754 10323866 10328028 10328029 10324021 10328657 10325106 10328498 10322424 ...
    ##  $ Cyclone_Frequency  : num  49.9 51.2 61.5 65.4 65.4 ...
    ##  $ Depth_m            : num  10 14 7 9.02 12.5 11.5 27.7 NA 4.05 NA ...
    ##  $ ClimSST            : num  302 262 299 300 300 ...
    ##  $ Ocean_Name         : chr  "Atlantic" "Pacific" "Atlantic" "Atlantic" ...
    ##  $ Country_Name       : chr  "Cuba" "French Polynesia" "United Kingdom" "United States" ...
    ##  $ Distance_to_Shore  : num  8519 1432 182 313 792 ...
    ##  $ Exposure           : chr  "Exposed" "Exposed" "Exposed" "Exposed" ...
    ##  $ Turbidity          : num  0.0287 0.0262 0.0429 0.0424 0.0424 ...
    ##  $ Date_Year          : int  2005 1991 2006 2006 2006 2005 2005 1998 2005 1998 ...
    ##  $ Bleaching_Level    : chr  "Colony" "Colony" "Colony" "Colony" ...
    ##  $ Temperature_Maximum: num  305 305 304 304 304 ...
    ##  $ SSTA               : num  -0.46 1.29 0.04 -0.07 0 0.27 0.29 0.91 0.35 0.63 ...
    ##  $ TSA                : num  -0.8 1.29 -2.64 -2.27 -2.19 0.17 0.25 0.17 -0.57 -0.17 ...
    ##  $ Percent_Bleaching  : num  50.2 50.7 50.9 50.9 50.9 ...
    ##  $ Temperature_Mean   : num  301 301 300 300 300 ...
    ##  $ Realm_Name         : chr  "Tropical Atlantic" "Eastern Indo-Pacific" "Tropical Atlantic" "Tropical Atlantic" ...
    ##  $ Percent_Cover      : num  NA NA NA NA NA NA NA NA NA NA ...

``` r
summary(coral)
```

    ##    Sample_ID        Cyclone_Frequency    Depth_m          ClimSST     
    ##  Min.   :    9623   Min.   : 18.31    Min.   : 0.000   Min.   :262.1  
    ##  1st Qu.:10311085   1st Qu.: 47.94    1st Qu.: 3.700   1st Qu.:298.6  
    ##  Median :10316277   Median : 50.92    Median : 6.000   Median :300.8  
    ##  Mean   :10128798   Mean   : 52.16    Mean   : 6.922   Mean   :294.2  
    ##  3rd Qu.:10321494   3rd Qu.: 55.73    3rd Qu.:10.000   3rd Qu.:302.0  
    ##  Max.   :10331713   Max.   :105.80    Max.   :90.000   Max.   :307.2  
    ##                                       NA's   :1799     NA's   :113    
    ##   Ocean_Name        Country_Name       Distance_to_Shore    Exposure        
    ##  Length:41361       Length:41361       Min.   :     3.2   Length:41361      
    ##  Class :character   Class :character   1st Qu.:   124.7   Class :character  
    ##  Mode  :character   Mode  :character   Median :   457.5   Mode  :character  
    ##                                        Mean   :  3761.8                     
    ##                                        3rd Qu.:  1785.5                     
    ##                                        Max.   :299218.5                     
    ##                                        NA's   :2                            
    ##    Turbidity        Date_Year    Bleaching_Level    Temperature_Maximum
    ##  Min.   :0.0000   Min.   :1980   Length:41361       Min.   :300.1      
    ##  1st Qu.:0.0335   1st Qu.:2003   Class :character   1st Qu.:304.4      
    ##  Median :0.0522   Median :2007   Mode  :character   Median :305.1      
    ##  Mean   :0.0671   Mean   :2008                      Mean   :305.1      
    ##  3rd Qu.:0.0794   3rd Qu.:2013                      3rd Qu.:305.8      
    ##  Max.   :1.2845   Max.   :2020                      Max.   :313.1      
    ##  NA's   :6                                          NA's   :132        
    ##       SSTA              TSA          Percent_Bleaching Temperature_Mean
    ##  Min.   :-4.6200   Min.   :-11.970   Min.   :  0.000   Min.   :290.9   
    ##  1st Qu.:-0.2500   1st Qu.: -1.810   1st Qu.:  0.000   1st Qu.:299.7   
    ##  Median : 0.2400   Median : -0.740   Median :  0.250   Median :300.8   
    ##  Mean   : 0.2549   Mean   : -0.981   Mean   :  9.619   Mean   :300.4   
    ##  3rd Qu.: 0.7500   3rd Qu.:  0.100   3rd Qu.:  6.000   3rd Qu.:301.6   
    ##  Max.   : 5.9000   Max.   :  5.900   Max.   :100.000   Max.   :303.5   
    ##  NA's   :148       NA's   :148       NA's   :6846      NA's   :132     
    ##   Realm_Name        Percent_Cover   
    ##  Length:41361       Min.   :  0.00  
    ##  Class :character   1st Qu.:  0.62  
    ##  Mode  :character   Median : 12.50  
    ##                     Mean   : 19.42  
    ##                     3rd Qu.: 33.12  
    ##                     Max.   :100.00  
    ##                     NA's   :12455

There is 18 columns and nearly 40000 rows, that seems we have a lot of
missing values. first of all i need to know how much missing values we
have in the dataset.

``` r
coral <- coral[!duplicated(coral), ]
missing_percent <- data.frame(colMeans(is.na(coral)) * 100)
print(missing_percent)
```

    ##                     colMeans.is.na.coral.....100
    ## Sample_ID                            0.000000000
    ## Cyclone_Frequency                    0.000000000
    ## Depth_m                              4.362269641
    ## ClimSST                              0.274005820
    ## Ocean_Name                           0.000000000
    ## Country_Name                         0.000000000
    ## Distance_to_Shore                    0.004849661
    ## Exposure                             0.000000000
    ## Turbidity                            0.014548982
    ## Date_Year                            0.000000000
    ## Bleaching_Level                      0.000000000
    ## Temperature_Maximum                  0.320077595
    ## SSTA                                 0.358874879
    ## TSA                                  0.358874879
    ## Percent_Bleaching                   16.454898157
    ## Temperature_Mean                     0.320077595
    ## Realm_Name                           0.000000000
    ## Percent_Cover                       30.201260912

based on the data above. we create a plot to see how much missing values
we have and compare them.

``` r
missing_percent <- data.frame(colMeans(is.na(coral)) * 100)
colnames(missing_percent)[1] <- "percent"

#missing_percent <- data.frame(column = names(coral), percent = missing_percent)
missing_percent$column <- names(coral)

percent_plot<-ggplot(missing_percent, aes(x = column, y = percent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "Columns", y = "Percent Missing", title = "Percent Missing in Each Column")
percent_plot + geom_text(aes(label = round(percent,2)), vjust = -0.5, color = "black", size = 3)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
vis_miss(coral)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-2-2.png)<!-- --> The
graph shows that percent cover has nearly 30% of the data missing. in
the second place Percent bleaching has nearly 15 % missing value and
Depth with less than 5% has the most missing values.  
After having a closerr look and study the dataset, i came to this
conclusion that although percent cover holds precious data but it is
better to remove the column. Percent bleaching with more than 15%
missing value, has the most important values in the dataset so i cannot
remove that. The Depth column is the same, it is very important and even
is less data set 5% so i keep the column and delete the missing rows
from the data set. I decided to not impute and fill the data in the
Percent Bleaching because it is the target column, so I decided to
remove the missing rows. Filling could happen in other columns with the
missing rows.

``` r
#coral$Percent_Cover <- NULL

coral<- coral%>% dplyr::select(-Percent_Cover)
```

Percent_Cover is removed and the next step is removing the missing info
from the percent_bleaching the most important column. and fill the rest
that are less than 5 % with the median.

``` r
coral <- coral %>% drop_na(Percent_Bleaching)
coral$Bleaching_Level[is.na(coral$Bleaching_Level)] <- "Colony"


coral <- coral %>%
  mutate(Depth_m = replace_na(Depth_m, median(Depth_m, na.rm = TRUE)),
        Distance_to_Shore = replace_na(Distance_to_Shore, median(Distance_to_Shore, na.rm = TRUE)),
        Turbidity = replace_na(Turbidity, median(Turbidity, na.rm = TRUE)),
        ClimSST = replace_na(ClimSST, median(ClimSST, na.rm = TRUE)),
        Temperature_Maximum = replace_na(Temperature_Maximum, median(Temperature_Maximum, na.rm = TRUE)),
        Temperature_Mean = replace_na(Temperature_Mean, median(Temperature_Mean, na.rm = TRUE)),
        SSTA = replace_na(SSTA, median(SSTA, na.rm = TRUE)),
        TSA = replace_na(TSA, median(TSA, na.rm = TRUE)),
                 )

coral=drop_na(coral)
vis_miss(coral)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
missing_percent <- data.frame(colMeans(is.na.data.frame(coral)) * 100)
print(missing_percent)
```

    ##                     colMeans.is.na.data.frame.coral.....100
    ## Sample_ID                                                 0
    ## Cyclone_Frequency                                         0
    ## Depth_m                                                   0
    ## ClimSST                                                   0
    ## Ocean_Name                                                0
    ## Country_Name                                              0
    ## Distance_to_Shore                                         0
    ## Exposure                                                  0
    ## Turbidity                                                 0
    ## Date_Year                                                 0
    ## Bleaching_Level                                           0
    ## Temperature_Maximum                                       0
    ## SSTA                                                      0
    ## TSA                                                       0
    ## Percent_Bleaching                                         0
    ## Temperature_Mean                                          0
    ## Realm_Name                                                0

``` r
coral['Ocean_Name'][coral['Ocean_Name'] == 'Arabian Gulf'] <- 'Persian Gulf'
```

As the graph above shows we have no missing values in the dataset.

Now we ned to see what wecan do with the outliers and skewness.

``` r
column_names<- names(coral)
print(column_names)
```

    ##  [1] "Sample_ID"           "Cyclone_Frequency"   "Depth_m"            
    ##  [4] "ClimSST"             "Ocean_Name"          "Country_Name"       
    ##  [7] "Distance_to_Shore"   "Exposure"            "Turbidity"          
    ## [10] "Date_Year"           "Bleaching_Level"     "Temperature_Maximum"
    ## [13] "SSTA"                "TSA"                 "Percent_Bleaching"  
    ## [16] "Temperature_Mean"    "Realm_Name"

we need to explore to see if we have outliers and then what we can do
about them. to do that I wrote a function to do that just by giveing it
the column name.

``` r
plot_distributions <- function(data, column_name) {
  
  #Boxplot
  boxplot_figure <- ggplot(data, aes(x = factor(1), y = .data[[column_name]])) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", column_name), x = "", y = column_name) +
    theme_minimal()
  print(boxplot_figure)
  
  #  histogram
  histogram_figure <- ggplot(data, aes(x = .data[[column_name]])) +
    geom_histogram(bins=15, fill = "blue", color = "black") + # Adjust binwidth as needed
    labs(title = paste("Histogram of", column_name), x = column_name, y = "Frequency") +
    theme_minimal()
  print(histogram_figure)

    #qqplot_figure <-
      ggplot(data, aes(sample = .data[[column_name]])) +
   stat_qq() +
   stat_qq_line() +
   labs(title = paste("QQ Plot of ", column_name)) +
    theme_minimal()
  
  #print(qqplot_figure)
}
```

## Outliers

``` r
outliers_percentage <- function(x) {
  if (is.numeric(x)) {
    q <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
    iqr <- IQR(x, na.rm = TRUE)
    return(sum(x < (q[1] - 1.5 * iqr) | x > (q[2] + 1.5 * iqr), na.rm = TRUE) / length(x) * 100)
  } else {
    return("No Outlier found!")
  }
}

percent_outliers_df <- function(df) {
  sapply(df, outliers_percentage)
}

# Use the function on a dataframe
columns=c("columns","percentage")
outlier_percent <- data.frame(names=names(coral),percent_outliers_df(coral))
print(outlier_percent)
```

    ##                                   names percent_outliers_df.coral.
    ## Sample_ID                     Sample_ID           9.74342601729843
    ## Cyclone_Frequency     Cyclone_Frequency           3.91536541475591
    ## Depth_m                         Depth_m           1.07679804957334
    ## ClimSST                         ClimSST           17.6234979973298
    ## Ocean_Name                   Ocean_Name          No Outlier found!
    ## Country_Name               Country_Name          No Outlier found!
    ## Distance_to_Shore     Distance_to_Shore             15.98363034771
    ## Exposure                       Exposure          No Outlier found!
    ## Turbidity                     Turbidity           9.43576943170604
    ## Date_Year                     Date_Year          0.156730713414988
    ## Bleaching_Level         Bleaching_Level          No Outlier found!
    ## Temperature_Maximum Temperature_Maximum           4.93701747257212
    ## SSTA                               SSTA            2.2929122888489
    ## TSA                                 TSA           3.23039414872003
    ## Percent_Bleaching     Percent_Bleaching           17.4174261333953
    ## Temperature_Mean       Temperature_Mean           5.00377314680444
    ## Realm_Name                   Realm_Name          No Outlier found!

based on the results above ClimSST has the most outliers. with 17.6
percent.

## Plots

``` r
ggplot(coral, aes(x = Ocean_Name, y = ClimSST, fill = Ocean_Name)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "ClimSST by Ocean Name", x = "Ocean Name", y = "ClimSST")
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot(coral, aes(x = Ocean_Name, y = Percent_Bleaching, fill = Ocean_Name)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Bleaching by Ocean Name", x = "Ocean Name", y = "Bleaching")
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
Atlantic ocean has the most report for the bleaching then Indian ocean.

``` r
#ggplot(coral, aes(x =Bleaching_Level, y = Percent_Bleaching,fill=Ocean_Name)) +
#  geom_bar(stat = "identity",width = 0.5) +
#  theme_minimal() +
#  labs(title = "Bleaching level by bleaching percent", x = "Bleaching_Level", y = #"Bleaching_Percent")



ggplot(coral, aes(x =Bleaching_Level,fill=Ocean_Name)) +
  geom_bar(stat = "count",width = 0.5) +
  theme_minimal() +
  labs(title = "Bleaching level count plot", x = "Bleaching_Level", y = "count")
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
Population has nearly 2 times more report of bleaching than population.
It shows the scope of the impact of the bleaching. In colony level of
bleaching Atlantic and pacific oceans have the most reports of bleached
corals.

``` r
ggplot(coral, aes(x = Percent_Bleaching, y = Temperature_Maximum,color=Realm_Name)) +
  geom_point(alpha=.4) +
  theme_minimal() +
  labs(title = "Bleaching percent by Temperature Maximum", x ="Bleaching_Percent", y =  "Temperature_Maximum")
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
hist(coral$Temperature_Maximum)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
hist(coral$Percent_Bleaching)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-11-3.png)<!-- --> The
range of 304 to 306 includes the majority of the bleaching. And the
majority of the bleached coral, are located in the Tropical Atlantic.

``` r
ggplot(coral, aes(x = Turbidity)) +
  geom_histogram(fill = "steelblue", alpha = 0.7,bins = 15) +
  theme_minimal() +
  labs(title = "Histogram of Turbidity", x = "Turbidity", y = "Frequency")
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ggplot(coral, aes(x = Percent_Bleaching, y = Turbidity,color=Exposure)) +
  geom_point(alpha=.4) +
  theme_minimal() +
  labs(title = "Bleaching percent by Turbidity", x ="Bleaching_Percent", y =  "Turbidity")
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
coral%>%
  dplyr::select(Cyclone_Frequency,Date_Year)%>%
  group_by(Date_Year)%>%
  mutate(Cyclone_Frequency=mean(Cyclone_Frequency))%>%
  
ggplot( aes(x=Date_Year, y=Cyclone_Frequency)) +
  geom_line() + 
  labs(title = "Cyclone by Year", x = "Year", y = "Cyclone") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

In this plot Cyclones are decreasing after 2015. We see a fluctuation in
Cyclones before 2005 but after that it gets more stable and then shows a
bit decreasing.

``` r
coral%>%
dplyr::select(Percent_Bleaching,Date_Year)%>%
group_by(Date_Year)%>%
mutate(Percent_Bleaching=mean(Percent_Bleaching))%>%
  
ggplot( aes(x=Date_Year, y=Percent_Bleaching)) +
  geom_line() + 
  labs(title = "Bleaching by Year", x = "Year", y = "Bleaching") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
coral%>%
  dplyr::select(Percent_Bleaching,Date_Year)%>%
  group_by(Date_Year)%>%
  summarise(Percent_Bleaching=n())%>%
  
ggplot( aes(x=Date_Year, y=Percent_Bleaching)) +
  geom_line() + 
  labs(title = "The Frequency of Bleaching Reports by Year", x = "Year", y = "The frequency of Bleaching") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
coral%>%
  dplyr::select(Percent_Bleaching,Date_Year)%>%
  #group_by(Date_Year)%>%
  filter(Date_Year>2000)%>%
  summarise(Percent_Bleaching=n())
```

    ##   Percent_Bleaching
    ## 1             32049

This plot indicates a huge decrease in cbleaching over time, from 1980
to 2020. if we consider last 20 years the fluctuation is much less than
the early years of the study, and obviously one of the reasons could be
because we have more reports in the last 10 or 20 years. Almost all the
reports and data were gathered after 2000.

``` r
coral%>%
dplyr::select(Temperature_Mean,Date_Year)%>%
group_by(Date_Year)%>%
mutate(Temperature_Mean=mean(Temperature_Mean))%>%
  
ggplot( aes(x=Date_Year, y=Temperature_Mean)) +
  geom_line() + 
  labs(title = "Temperature_Mean by Year", x = "Year", y = "Temperature_Mean") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Since 2000, we see 1 degree of warming in the mean temperature of sea
water.

``` r
#function for ploting
coral_plot <- function(data, column1, column2, y_label,x_label) {
  data %>%
    dplyr::select({{ column1 }}, {{ column2 }}) %>%
    group_by({{ column2 }}) %>%
    summarise(mean_column1 = mean({{ column1 }}, na.rm = TRUE)) %>%
    ggplot(aes(x = {{ column2 }}, y = mean_column1)) +
    geom_line() +
    labs(title = paste(x_label, "by", y_label),
         x = x_label, y = y_label) +
    theme_minimal()
}
coral_plot(coral,SSTA,Date_Year,"Sea Surface Temperature Anomaly","Year")
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
Based on the plot from 2000 to 2020 in average +0.5 degree.

``` r
coral_plot(coral,Turbidity,Date_Year,"Turbidity","Year")
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->
Turbidity is not changing too much from 2000 to 2018 and then we see a
big drop, that i think is not.But in total we can say turbidity
increased 0.5.

``` r
ggplot(coral,aes(x=Temperature_Maximum,y=SSTA,color=Percent_Bleaching))+geom_point()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
There is no specific relationship between SSTA and Percent_Bleaching but
coloring shows around 306 degree of Maximum Temperature and 0 to 20 SSTA
we have the most Bleaching reports. And with increasing the temperature
bleaching is not increasing, rather decreasing.

``` r
ggplot(coral,aes(x=Realm_Name,y=Percent_Bleaching))+
  geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = .4))
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
coral_summary <- coral %>%
  group_by(Exposure) %>%
  summarise(Mean_Percent_Bleaching = mean(Percent_Bleaching))
ggplot(coral_summary, aes(x=Exposure, y=Mean_Percent_Bleaching)) +
  geom_bar(stat='identity')
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-19-2.png)<!-- --> In
the plots above, Bleaching percent is the highest in the tropical
Atlantic and central Indo-Pacific. Those corals which “Sometimes” faced
with the sea currents have more bleaching than those sheltered or
Exposed.

``` r
ggplot(coral,aes(x=as.factor(Bleaching_Level),y=Percent_Bleaching^(1/6)))+
  geom_boxplot()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-20-1.png)<!-- --> I
used a transformer here, in the population level of bleaching the median
is close to zero on the other hand, the median in the colony level is
around 10. Colonies are much more prone to bleach.

``` r
coral %>%
  count(Country_Name) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(y = reorder(Country_Name, n), x = n)) +
  geom_bar(stat = 'identity') 
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-21-1.png)<!-- --> This
plot indicates top10 countries with the highest bleaching reports.
Malyasia and USA with more than 4000 report each, are in the top list.

## Descriptive Statistics

``` r
summary(coral)
```

    ##    Sample_ID        Cyclone_Frequency    Depth_m          ClimSST     
    ##  Min.   :    9623   Min.   : 18.31    Min.   : 0.000   Min.   :262.1  
    ##  1st Qu.:10311014   1st Qu.: 47.94    1st Qu.: 4.000   1st Qu.:298.6  
    ##  Median :10316408   Median : 51.38    Median : 6.000   Median :300.8  
    ##  Mean   :10094645   Mean   : 52.33    Mean   : 7.001   Mean   :294.2  
    ##  3rd Qu.:10322097   3rd Qu.: 56.08    3rd Qu.:10.000   3rd Qu.:302.0  
    ##  Max.   :10331713   Max.   :105.80    Max.   :50.300   Max.   :307.2  
    ##   Ocean_Name        Country_Name       Distance_to_Shore    Exposure        
    ##  Length:34454       Length:34454       Min.   :     3.2   Length:34454      
    ##  Class :character   Class :character   1st Qu.:   126.5   Class :character  
    ##  Mode  :character   Mode  :character   Median :   476.8   Mode  :character  
    ##                                        Mean   :  3672.9                     
    ##                                        3rd Qu.:  1840.5                     
    ##                                        Max.   :299218.5                     
    ##    Turbidity         Date_Year    Bleaching_Level    Temperature_Maximum
    ##  Min.   :0.00000   Min.   :1980   Length:34454       Min.   :300.1      
    ##  1st Qu.:0.03950   1st Qu.:2005   Class :character   1st Qu.:304.4      
    ##  Median :0.05700   Median :2008   Mode  :character   Median :305.1      
    ##  Mean   :0.07552   Mean   :2009                      Mean   :305.1      
    ##  3rd Qu.:0.08420   3rd Qu.:2014                      3rd Qu.:305.8      
    ##  Max.   :1.28450   Max.   :2020                      Max.   :313.1      
    ##       SSTA              TSA           Percent_Bleaching Temperature_Mean
    ##  Min.   :-4.2600   Min.   :-11.9700   Min.   :  0.000   Min.   :290.9   
    ##  1st Qu.:-0.2100   1st Qu.: -1.8100   1st Qu.:  0.000   1st Qu.:299.8   
    ##  Median : 0.2700   Median : -0.7100   Median :  0.250   Median :300.8   
    ##  Mean   : 0.2795   Mean   : -0.9606   Mean   :  9.632   Mean   :300.4   
    ##  3rd Qu.: 0.7700   3rd Qu.:  0.1200   3rd Qu.:  6.060   3rd Qu.:301.6   
    ##  Max.   : 5.9000   Max.   :  5.9000   Max.   :100.000   Max.   :303.5   
    ##   Realm_Name       
    ##  Length:34454      
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ## 

Cyclone_frequency has the median 51.4 and mean 52.3 the mean and median
are close to each other we can say Cyclone is normally distributed
around 52.

``` r
hist(Cyclone_Frequency)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-23-1.png)<!-- --> The
shape of the histogram shows a right skewed normal dist. with minimum 0
and max 50.

``` r
#boxplot()
hist(Depth_m,breaks=50)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

``` r
ggplot(coral, aes(sample = Depth_m)) +
  geom_qq() +
  geom_qq_line()+
  ggtitle("QQplot") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-24-2.png)<!-- -->

``` r
ggplot(coral, aes(sample = (Depth_m)^(1/2.5))) +
  geom_qq() +
  geom_qq_line()+
  ggtitle("QQplot with transformer") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-24-3.png)<!-- --> mean
and median on the Depth variable are close to each other that shows it
is close to normal distribution with the mean 7 and median 6.the
transformer iused for this variable is Depth_m^(1/2.5).

``` r
hist(ClimSST,breaks=50)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
ggplot(coral, aes(sample = ClimSST)) +
  geom_qq() +
  geom_qq_line()+
  ggtitle("QQplot") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-25-2.png)<!-- -->

``` r
hist(log(ClimSST),breaks=50)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-25-3.png)<!-- --> It
seems in ClimSST we have a left skewed distribution with outliers. And
the plot after using the transformer still not normal. i investigated
the data that’s clear why there is more than 5000 times 262.15 in the
ClimSST. I couldn’t find any pattern to explain that.

``` r
ocean_table<-table(coral$Ocean_Name)
summary(coral$Ocean_Name)
```

    ##    Length     Class      Mode 
    ##     34454 character character

``` r
ocean_table
```

    ## 
    ##     Atlantic       Indian      Pacific Persian Gulf      Red Sea 
    ##        13298         2322        17412          369         1053

``` r
barplot(ocean_table,main = "the oceans")
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-26-1.png)<!-- --> We
know from prvious plots that the USA and Malaysia have the most reports
of research that’s the reason that we have a big frequency in Pacific
and Atlantic oceans. Persian Gulf has the least frequency.

``` r
hist(Distance_to_Shore,breaks=100)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
ggplot(coral, aes(sample = Distance_to_Shore)) +
  geom_qq() +
  geom_qq_line()+
  ggtitle("QQplot") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-27-2.png)<!-- -->

``` r
ggplot(coral, aes(sample = log(Distance_to_Shore))) +
  geom_qq() +
  geom_qq_line()+
  ggtitle("QQplot with transformer") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-27-3.png)<!-- -->

``` r
hist(log(Distance_to_Shore),main = "histogram after transformer log()")
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-27-4.png)<!-- -->

minimum distance to shore is 3 meters and the max is 300 kilometers. The
median is 476 and he mean 3671 and the distribution is right
skewed.there’s is a big difference between mean and median that’s why
that is right skewed.

``` r
table_exposure<-table(coral$Exposure)
barplot(table_exposure)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-28-1.png)<!-- --> We
have three category in the Exposure group. Sheltered corals have the
most reports of the study.

``` r
table_level<-table(coral$Bleaching_Level)
barplot(table_level)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-29-1.png)<!-- --> The
bleaching level column is category. Column has two groups Colony and
Population. and population is twice as much as the colony.

``` r
summary(coral$SSTA)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -4.2600 -0.2100  0.2700  0.2795  0.7700  5.9000

``` r
hist(coral$SSTA,breaks=100)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
ggplot(coral, aes(sample = SSTA)) +
  geom_qq() +
  geom_qq_line()+
  ggtitle("QQplot") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-30-2.png)<!-- --> The
mean and Median in SSTA are almost equal .27 and .28 with the min and
max -4.2 and 5.9. The the distribution is normal with some outliers at
two tails.

``` r
summary(coral$TSA)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -11.9700  -1.8100  -0.7100  -0.9606   0.1200   5.9000

``` r
hist(coral$TSA,breaks=200)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
ggplot(coral, aes(sample = TSA)) +
  geom_qq() +
  geom_qq_line()+
  ggtitle("QQplot") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-31-2.png)<!-- --> with
median and mean around -0.71, -0.96 they are nearly close and based on
the plots it seems it is right skewed.

``` r
summary(coral$Percent_Bleaching)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   0.250   9.632   6.060 100.000

``` r
hist(coral$Percent_Bleaching,breaks=25)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
ggplot(coral, aes(sample = Percent_Bleaching)) +
  geom_qq() +
  geom_qq_line()+
  ggtitle("QQplot") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->

``` r
ggplot(coral, aes(sample =log (Percent_Bleaching+1))) +
  geom_qq() +
  geom_qq_line()+
  ggtitle("QQplot with transformer") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-32-3.png)<!-- -->

``` r
hist(log(coral$Percent_Bleaching+1),main = "histogram after transformer")
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-32-4.png)<!-- -->

``` r
#The percentage of the 0s in the Bleaching column

#nrow(subset(coral, Percent_Bleaching==0))*100/nrow(coral)

#Removing the outliers
#q1 <- quantile(coral$Percent_Bleaching, 0.25, na.rm = TRUE)
#q3 <- quantile(coral$Percent_Bleaching, 0.75, na.rm = TRUE)
#iqr <- q3 - q1

# Identify the outliers
#outliers <- coral$Percent_Bleaching < (q1 - 1.5 * iqr) | coral$Percent_Bleaching > #(q3 + 1.5 * iqr)

# Remove the outliers
#coral_clean_of_outliers <- coral[!outliers, ]

#plotting after removing outliers
#hist(coral_clean_of_outliers$Percent_Bleaching,breaks=25)
#ggplot(coral_clean_of_outliers, aes(sample = Percent_Bleaching)) +
#  geom_qq() +
#  geom_qq_line()+
#  ggtitle('QQplot') +
#  theme_minimal()

#hist(log(coral_clean_of_outliers$Percent_Bleaching),main = 'histogram after transformer')

#nrow(coral)-nrow(coral_clean_of_outliers)

#nrow(subset(coral_clean_of_outliers, Percent_Bleaching==0))*100/nrow(coral_clean_of_outliers)
```

Percent Bleaching is a key and important variable in the study that
shows the amount of the bleaching in every report. It seems, it is
highly right skewed. 48 percent of the columns is 0, which is nearly
half of the data. I added 1 to the Percent_Bleaching as a transformer
and used the log() on it to see the distribution. the distribution is
not normal. and it is not just right skewed distribution.

``` r
summary(coral$Temperature_Mean)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   290.9   299.8   300.8   300.4   301.6   303.5

``` r
hist(coral$Temperature_Mean,breaks=25)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
ggplot(coral, aes(sample = Temperature_Mean)) +
  geom_qq() +
  geom_qq_line()+
  ggtitle("QQplot") +
  theme_minimal()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-33-2.png)<!-- -->
Temperature mean in the left tail has some outliers and mean and median
are almost the same. and it is the normal distribution.

``` r
ggplot(coral, aes(x = Realm_Name)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("Realm Name")
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->
there are 8 realm_Name categories, Central Indo-Pacific and Tropical
Atlantic are the highest proportion.

## Hypothesis Test

# One Sample

For the one-sample test I want to test if mean cyclone frequency is 50.

H0: the mean cyclone frequency is 50 , mu=50 H1: the mean is not 50,
mu!=50

``` r
boxplot(coral$Cyclone_Frequency)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
hist(coral$Cyclone_Frequency)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-35-2.png)<!-- -->

``` r
# I had this code but as far as i found out that the nirmal 
#mu <- 50 
#n  <- nrow(coral)
#sd<-sd(coral$Cyclone_Frequency)
#cyclone_frequency_mean <- mean(coral$Cyclone_Frequency)
#test_stat <- (cyclone_frequency_mean-mu)/(sd/sqrt(n))

#p_value_one_sample <- (1-pnorm(test_stat))*2
#p_value_one_sample

# using the t.test()
#one_sample_test_cyclone<-t.test(coral$Cyclone_Frequency,mu=50,alternative = "two.sided")
#one_sample_test_cyclone

# Bootstrapping

mean_hat=c()
for(i in 1:1000) {
  boot_samp = sample(coral$Cyclone_Frequency, length(coral$Cyclone_Frequency), replace = TRUE)
  mean_hat[i] = mean(boot_samp)
}
hist(mean_hat)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-35-3.png)<!-- -->

``` r
mean(mean_hat)
```

    ## [1] 52.32794

``` r
median(mean_hat)
```

    ## [1] 52.32907

``` r
sd(mean_hat)
```

    ## [1] 0.04197805

``` r
quantile(mean_hat,c(0.025,0.975))
```

    ##     2.5%    97.5% 
    ## 52.25002 52.41387

``` r
2*pnorm(50,mean(mean_hat),sd(mean_hat))
```

    ## [1] 0

95% CI is 52.24877 and 52.39952. the null hyp 50 is not within the 95%
CI, we reject the null hypothesis. The pvalue for this test is
calculated and less than 0.05.

\#One Sample -2 I am going to check the one sample hypothesis on TSA.

H0: the mean TSA is 0 H1: the mean is not 0

``` r
hist(coral$TSA)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
boxplot((coral$TSA)^(1/4))
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-36-2.png)<!-- -->

``` r
one_sample_test_TSA<-t.test(coral$TSA,mu=0,alternative = "two.sided")
one_sample_test_TSA
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  coral$TSA
    ## t = -108.44, df = 34453, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.9779729 -0.9432461
    ## sample estimates:
    ##  mean of x 
    ## -0.9606095

``` r
mean_hat_TSA=c()
for(i in 1:1000) {
  boot_samp = sample(coral$TSA, length(coral$TSA), replace = TRUE)
  mean_hat_TSA[i] = mean(boot_samp)
}
#hist(mean_hat_TSA,breaks = 30)
boxplot(mean_hat_TSA)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-36-3.png)<!-- -->

``` r
print(paste("mean: ", mean(mean_hat_TSA)))
```

    ## [1] "mean:  -0.960620453938585"

``` r
print(paste("median: ", median(mean_hat_TSA)))
```

    ## [1] "median:  -0.960533610030766"

``` r
sd(mean_hat_TSA)
```

    ## [1] 0.008713278

``` r
print(paste("Quantile : ", quantile(mean_hat_TSA,c(0.025,0.975))))
```

    ## [1] "Quantile :  -0.978285467579962" "Quantile :  -0.9442915118709"

``` r
2*pnorm(50,mean(mean_hat_TSA),sd(mean_hat_TSA))
```

    ## [1] 2

pvalue in both ways bootstraping and the normal way even with non-normal
dist is less than .05 then we can reject the null hypothesis and we can
conclude that the mean TSA is not zero. the confidence interval is
between -0.9778226, -0.9431075. and the mean is -0.960465.

# Two sample

Two sample hypothesis, of SSTA by bleaching levels that are two
categories of population and colony.

H0: The means of the population and colony are equal
mu.population=mu.coloy : mu.population- mu.colony=0

H1: The means are not equal. mu.population- mu.colony != 0

``` r
#boxplot(coral$SSTA~coral$Bleaching_Level)

ggplot(coral, aes(x = SSTA, fill = Bleaching_Level)) +
  geom_histogram(alpha = 0.5)+ 
  labs(title = "Histogram of SSTA by ",
       x = "Bleaching_Level",
       y = "Frequency") +
  theme_minimal()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
boxplot(coral$SSTA~coral$Bleaching_Level)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-37-2.png)<!-- --> we
are not happy about normality for both categories. it seems a normal
distribution but there are a lot of outliers. so I used the
bootstrapping.

``` r
SSTA_population<-coral$SSTA[coral$Bleaching_Level=="Population"]
SSTA_colony<-coral$SSTA[coral$Bleaching_Level=="Colony"]
boxplot(coral$SSTA~coral$Bleaching_Level)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
hist(SSTA_population)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-38-2.png)<!-- -->

``` r
hist(SSTA_colony)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-38-3.png)<!-- --> We
have two normal distribution but there are a lot of outliers.

``` r
xbarstar_2sample=c()
for(i in 1:1000) {
  boot_samp1 = sample(SSTA_population, length(SSTA_population), replace = TRUE)
  boot_samp2 = sample(SSTA_colony, length(SSTA_colony),
                      replace = TRUE)
  
  xbarstar_2sample[i] = mean(boot_samp1,na.rm=TRUE)-
    mean(boot_samp2,na.rm=TRUE)
}
hist(xbarstar_2sample)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

``` r
boxplot(xbarstar_2sample)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-39-2.png)<!-- -->

``` r
print(paste("the mean difference: ",mean(xbarstar_2sample)))
```

    ## [1] "the mean difference:  -0.0808065891188251"

``` r
print(paste("the difference median: ",median(xbarstar_2sample)))
```

    ## [1] "the difference median:  -0.0807085558522474"

``` r
print(paste("the difference Standard Error: ",sd(xbarstar_2sample)))
```

    ## [1] "the difference Standard Error:  0.00985455517899358"

``` r
print(paste("Confidence Interval: ",quantile(xbarstar_2sample,c(0.025,0.975))))
```

    ## [1] "Confidence Interval:  -0.0999380980195816"
    ## [2] "Confidence Interval:  -0.0607500041722296"

``` r
#two_sample_test_SSTA_bleachingLevel <- t.test(coral$SSTA~coral$Bleaching_Level,alternative = "two.sided")
#two_sample_test_SSTA_bleachingLevel
```

95% CI is between -0.09868988 -0.06054997 and doesn’t contains the null
hypothesis (0), so we reject the null hypothesis. We conclude there is
not a meaningful difference between the SSTA by population or colony
level of bleaching.

# Two sample -2

testing the mean of cyclone frequency in two following years 2018-2019.
H0: the mean of Cyclone frequency is the same in 2018 and 2019 H0:
mu2019 = mu2018 : mu2019 - mu2018 =0 H1: mu2019 != mu2018 : mu2019 -
mu2018!=0

``` r
coral_2019<- coral%>%
  dplyr::select(Cyclone_Frequency,Date_Year)%>%
  filter(Date_Year==2019)

coral_2018<- coral%>%
  dplyr::select(Cyclone_Frequency,Date_Year)%>%
  filter(Date_Year==2018)

hist(coral_2019$Cyclone_Frequency)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

``` r
hist(coral_2018$Cyclone_Frequency)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-40-2.png)<!-- -->

``` r
boxplot(coral_2019$Cyclone_Frequency,coral_2018$Cyclone_Frequency)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-40-3.png)<!-- -->

``` r
t.test(coral_2019$Cyclone_Frequency,coral_2018$Cyclone_Frequency,alternative = "two.sided")
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  coral_2019$Cyclone_Frequency and coral_2018$Cyclone_Frequency
    ## t = -6.5895, df = 2421, p-value = 5.396e-11
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.7738743 -0.9602409
    ## sample estimates:
    ## mean of x mean of y 
    ##  49.76405  51.13110

``` r
# Bootstraping

cyclone_boot=c()
for (i in 1:1000){
  boot_2019=sample(coral_2019$Cyclone_Frequency,length(coral_2019$Cyclone_Frequency), replace=TRUE)
  boot_2018=sample(coral_2018$Cyclone_Frequency,length(coral_2019$Cyclone_Frequency), replace=TRUE)
  cyclone_boot[i]=mean(boot_2019,na.rm=TRUE)-mean(boot_2018,na.rm=TRUE)
}


hist(cyclone_boot)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-40-4.png)<!-- -->

``` r
boxplot(cyclone_boot)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-40-5.png)<!-- -->

``` r
print(paste("the mean difference: ",mean(cyclone_boot)))
```

    ## [1] "the mean difference:  -1.36118881956155"

``` r
print(paste("the difference median: ",median(cyclone_boot)))
```

    ## [1] "the difference median:  -1.36025716694772"

``` r
print(paste("the difference Standard Error: ",sd(cyclone_boot)))
```

    ## [1] "the difference Standard Error:  0.211630316632594"

``` r
print(paste("Confidence Interval: ",quantile(cyclone_boot,c(0.025,0.975))))
```

    ## [1] "Confidence Interval:  -1.7916684232715"  
    ## [2] "Confidence Interval:  -0.959283305227657"

the spread seems similar. and the histogram shows normal distribution
for them. we have numerous outliers, I did the test with th outliers
then tried bootstrapping, confidence interval almost didn’t change. I
conclude that the outliers do not impact the test.

the pvalue is less than 0.05 so we reject the null hypothesis and
conclude the means are not equal. and the means are : 49.76035,
51.11950.

\#ANOVA test.

testing the mean of SSTA over the different oceans and sees.

H0: the mean SSTA in all the groups and oceans are equal H1: the mean at
least in one of them is not equal.

``` r
ggplot(coral, aes(x=Ocean_Name, y=SSTA)) +
  geom_boxplot()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
aov_ssta<-aov(SSTA~Ocean_Name,data = coral)
summary(aov_ssta)
```

    ##                Df Sum Sq Mean Sq F value Pr(>F)    
    ## Ocean_Name      4    174   43.60    63.8 <2e-16 ***
    ## Residuals   34449  23543    0.68                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#boxplot(coral$SSTA~coral$Ocean_Name,ylab = "SSTA", xlab = "Ocean")
```

the spread is similar but we have a lot of outliers so I use
bootstrapping to do the anova test.

``` r
SSTA_Atlantic<-coral$SSTA[coral$Ocean_Name=="Atlantic"]
SSTA_Pacific<-coral$SSTA[coral$Ocean_Name=="Pacific"]
SSTA_Indian<-coral$SSTA[coral$Ocean_Name=="Indian"]
SSTA_Persian<-coral$SSTA[coral$Ocean_Name=="Persian Gulf"]
SSTA_Red<-coral$SSTA[coral$Ocean_Name=="Red Sea"]

fstat<-c()
for(i in 1:1000){
  groupA = sample(SSTA_Atlantic, size=length(SSTA_Atlantic), replace=T)
  groupB = sample(SSTA_Pacific, size=length(SSTA_Pacific), replace=T)
  groupC = sample(SSTA_Indian, size=length(SSTA_Indian), replace=T)
  groupD = sample(SSTA_Persian, size=length(SSTA_Persian), replace=T)
  groupE = sample(SSTA_Red, size=length(SSTA_Red), replace=T)
  
  data_boot<-c(groupA,groupB,groupC,groupD,groupE)
  simdata = data.frame(data_boot,Ocean_Name=sort(coral$Ocean_Name))
  fstat[i]<-summary(aov(data_boot~Ocean_Name, data=simdata))[[1]]$F[1]
}

hist(fstat)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

``` r
freal2<-summary(aov(coral$SSTA~coral$Ocean_Name))[[1]]$F[1]
#is the p-value:
print(paste("The p-value is:",mean(fstat>=freal2)))
```

    ## [1] "The p-value is: 0.286"

The pvalue were checked without dealing with outliers and then
bootstrapping were applied to get the result. 0.25 \> 0.05 so we fail to
reject the null hypothesis. That means at least one group has a
different mean of SSTA between oceans and seas.

# Anova Test - 2

I would test if the temperature is the same from 2010 to 2020

H0: the mean temperature in years 2010 to 2019 are the same. H1: the
mean temprature is not equal from 2010 to 2019

``` r
coral_date<-coral%>%
    filter(Date_Year>=2010 & Date_Year<=2019)

temp_2010<-coral_date$Temperature_Mean[coral_date$Date_Year==2010]
temp_2011<-coral_date$Temperature_Mean[coral_date$Date_Year==2011]
temp_2012<-coral_date$Temperature_Mean[coral_date$Date_Year==2012]
temp_2013<-coral_date$Temperature_Mean[coral_date$Date_Year==2013]
temp_2014<-coral_date$Temperature_Mean[coral_date$Date_Year==2014]
temp_2015<-coral_date$Temperature_Mean[coral_date$Date_Year==2015]
temp_2016<-coral_date$Temperature_Mean[coral_date$Date_Year==2016]
temp_2017<-coral_date$Temperature_Mean[coral_date$Date_Year==2017]
temp_2018<-coral_date$Temperature_Mean[coral_date$Date_Year==2018]
temp_2019<-coral_date$Temperature_Mean[coral_date$Date_Year==2019]

fstat<-c()
for(i in 1:1000){
  group2010 = sample(temp_2010, size=length(temp_2010), replace=T)
  group2011 = sample(temp_2011, size=length(temp_2011), replace=T)
  group2012 = sample(temp_2012, size=length(temp_2012), replace=T)
  group2013 = sample(temp_2013, size=length(temp_2013), replace=T)
  group2014 = sample(temp_2014, size=length(temp_2014), replace=T)
  group2015 = sample(temp_2015, size=length(temp_2015), replace=T)
  group2016 = sample(temp_2016, size=length(temp_2016), replace=T)
  group2017 = sample(temp_2017, size=length(temp_2017), replace=T)
  group2018 = sample(temp_2018, size=length(temp_2018), replace=T)
  group2019 = sample(temp_2019, size=length(temp_2019), replace=T)

  
  data_boot<-c(group2010,group2011,group2012,group2013,group2014,group2015,group2016,group2017,group2018,group2019)
  
  simdata = data.frame(data_boot,Date_sorted=sort(coral_date$Date_Year))
  fstat[i]<-summary(aov(data_boot~Date_sorted, data=simdata))[[1]]$F[1]
}

hist(fstat)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
freal2<-summary(aov(coral_date$Temperature_Mean~coral_date$Date_Year))[[1]]$F[1]
#is the p-value:
print(paste("The p-value is:",mean(fstat>=freal2)))
```

    ## [1] "The p-value is: 0.511"

We reject the null hypothesis 0.503 \> 0.05 and conclude that the mean
temperature is not the same from 2010 to 2019

# Chisq Test

testing of to see if the oceans and Exposure’s of the corals are
independent of each other. H0: Oceans and exposures are independent of
each other H1: Oceans and exposures are not independent of each other

``` r
ocean_exposure_table<-table(coral$Ocean_Name, coral$Exposure)
chi_ocean_exposure<-chisq.test(ocean_exposure_table)

chi_ocean_exposure$expected
```

    ##               
    ##                  Exposed Sheltered  Sometimes
    ##   Atlantic     4718.4086 7510.0854 1069.50595
    ##   Indian        823.8942 1311.3565  186.74935
    ##   Pacific      6178.1419 9833.4793 1400.37882
    ##   Persian Gulf  130.9289  208.3939   29.67722
    ##   Red Sea       373.6264  594.6849   84.68866

the expected values are greater that 5 so the assumption are met.

``` r
chi_ocean_exposure
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  ocean_exposure_table
    ## X-squared = 5211.9, df = 8, p-value < 2.2e-16

pvalue is less than .05 so we reject the null hypothesis and conclude
that the ocean and the exposure are dependent of each other. in other
word they associate to each other.

# Regression

To check the linear relationship between variables in the coral data set
we have to do the correlation test to see if we have any kind of linear
relationship.

``` r
coral %>% select(where(is.numeric))%>%pairs()
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

``` r
# a few examples
plot(coral$Turbidity,coral$Depth_m)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-46-2.png)<!-- -->

``` r
plot(coral$Temperature_Mean , coral$Cyclone_Frequency)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-46-3.png)<!-- -->

``` r
plot(coral$Temperature_Mean, coral$TSA)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-46-4.png)<!-- -->

``` r
plot(coral$Cyclone_Frequency, coral$Percent_Bleaching)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-46-5.png)<!-- -->

``` r
plot(coral$Cyclone_Frequency, coral$Temperature_Maximum)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-46-6.png)<!-- -->

``` r
plot(coral$Temperature_Mean,coral$TSA)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-46-7.png)<!-- -->

``` r
plot(coral$TSA , coral$Turbidity)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-46-8.png)<!-- -->

``` r
ClimSST_outlier_free <- coral %>% filter(ClimSST != 262.15)
ClimSST_outlier_free <- coral[sapply(ClimSST_outlier_free, is.numeric)]

pairs(ClimSST_outlier_free)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-46-9.png)<!-- -->

``` r
#========= check the linearity
```

From the plots above i chose the TSA and ClimSST to do the regression
test. first i do the regression test without taking care and dealing
with outliers.

H0: Slope=0 H1: slope!=0

``` r
boxplot(coral$TSA)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

``` r
hist(coral$TSA)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-47-2.png)<!-- -->

We have a normal dist in TSA with a lot of outliers. So we do a linear
regression first to see what we get.

we do the correlation test to see how is the linear reationship between
the variables. p=0 H0: there no linear relationship between TSA and
ClimSST. p!=0

``` r
cor.test(coral$TSA,coral$ClimSST, method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  coral$TSA and coral$ClimSST
    ## t = 12.85, df = 34452, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.05854978 0.07956746
    ## sample estimates:
    ##        cor 
    ## 0.06906628

pvalue \<.05 and we reject the null hypothesis. That means the there is
a linear relationship between two variables. Now i can do the regression
test to see if there is a slope.

``` r
reg_result<-lm(TSA~ClimSST, data = coral)
summary(reg_result)
```

    ## 
    ## Call:
    ## lm(formula = TSA ~ ClimSST, data = coral)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -10.9920  -0.8489   0.2453   1.0731   6.8171 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -3.2204752  0.1760835  -18.29   <2e-16 ***
    ## ClimSST      0.0076818  0.0005978   12.85   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.64 on 34452 degrees of freedom
    ## Multiple R-squared:  0.00477,    Adjusted R-squared:  0.004741 
    ## F-statistic: 165.1 on 1 and 34452 DF,  p-value: < 2.2e-16

no I remove the outliers on ClimSST because it’s obvious that there wasa
problem with one of the entries.

``` r
ClimSST_outlier_free <- coral %>% filter(ClimSST != 262.15)
boxplot(ClimSST_outlier_free$ClimSST)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-50-1.png)<!-- -->

``` r
hist(ClimSST_outlier_free$ClimSST)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-50-2.png)<!-- --> I
removed most part of the mistake data in ClimSST and saves in in new
variable. If i imputed the mistake variable , 17% of a specific data,
median or mean would make bias so i removed them.

``` r
ClimSST_outlier_free <- coral %>% filter(ClimSST != 262.15)

data<-ClimSST_outlier_free
BootstrapSLR<-function(data, mod_formula, rep){
  coef_table<-c()
  for(i in 1:rep){
    data_boot<-data[sample(1:nrow(data),
                           size=nrow(data),replace=T),]
    lm_boot<-lm(mod_formula,data=data_boot)
    coef_table<-rbind(coef_table,coef(lm_boot))
  }
  coef_table
}

mod_formula<- formula (TSA ~ ClimSST)
lm_coral_500<-BootstrapSLR(ClimSST_outlier_free,mod_formula,500)
hist(lm_coral_500[,1])
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

``` r
boxplot(lm_coral_500[,1])
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-51-2.png)<!-- -->

``` r
hist(lm_coral_500[,2])
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-51-3.png)<!-- -->

``` r
boxplot(lm_coral_500[,2])
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-51-4.png)<!-- -->

``` r
t(apply(lm_coral_500,2,quantile,probs=c(0.025,0.975)))
```

    ##                     2.5%        97.5%
    ## (Intercept) -155.9836686 -148.2991056
    ## ClimSST        0.4896655    0.5151461

``` r
quantile(lm_coral_500[,1],c(0.025,0.975))#95% CI for intercept
```

    ##      2.5%     97.5% 
    ## -155.9837 -148.2991

``` r
quantile(lm_coral_500[,2],c(0.025,0.975))#95% CI for slope
```

    ##      2.5%     97.5% 
    ## 0.4896655 0.5151461

``` r
lm_coral_5000<-BootstrapSLR(ClimSST_outlier_free,mod_formula,5000)
hist(lm_coral_5000[,1])
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-51-5.png)<!-- -->

``` r
hist(lm_coral_5000[,2])
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-51-6.png)<!-- -->

``` r
print(paste("The mean is :",mean(lm_coral_5000[,2])))
```

    ## [1] "The mean is : 0.502845613598046"

``` r
t(apply(lm_coral_5000,2,quantile,probs=c(0.025,0.975)))
```

    ##                     2.5%        97.5%
    ## (Intercept) -156.1242353 -148.1552567
    ## ClimSST        0.4891942    0.5156233

``` r
apply(lm_coral_500,2,mean)
```

    ## (Intercept)     ClimSST 
    ## -152.329939    0.503051

``` r
apply(lm_coral_5000,2,mean)
```

    ##  (Intercept)      ClimSST 
    ## -152.2688287    0.5028456

``` r
TSA_res= -151.999254 + 0.501952 * ClimSST_outlier_free$ClimSST
```

Average stopping distance will increase by 0.501952 degree for every one
unit ClimSST

H0: slope = 0, i.e. no linear relationship between TSA and ClimSST

95% CI for slope is (0.4893382 , 0.5147673 ) doesn’t contain the null
(0), so we reject H0. And conclude there is significant linear
relationship between dist and speed.

``` r
multiplehistograms<-function(X){
  for(i in 1:ncol(X)){
    hist(X[,i],main=colnames(X)[i],xlab=colnames(X)[i])
  }
}
par(mfrow=c(2,1))
multiplehistograms(lm_coral_5000)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

``` r
multiplehistograms(lm_coral_500)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-52-2.png)<!-- -->

``` r
apply(lm_coral_500,2,mean)
```

    ## (Intercept)     ClimSST 
    ## -152.329939    0.503051

``` r
apply(lm_coral_5000,2,mean)
```

    ##  (Intercept)      ClimSST 
    ## -152.2688287    0.5028456

``` r
fit<- -151.999254 + 0.501952 * ClimSST_outlier_free$ClimSST
res<-ClimSST_outlier_free$TSA-fit
dev.off()
```

    ## null device 
    ##           1

``` r
plot(fit,res,ylab="Residuals from bootstrap", xlab="Fitted values from bootstrap")
qqnorm(scale(res))
```

based on the intercept and slope we gt the formula gets a regression
formula. the result of the regression ,the spread of residuals on fitted
values seems normal. the qqplot shows a bit distanced from the diagonal
line but seems alright. when compare with the primary formula and
regression test and compare it with the resul after bootstrapping there
is not much difference between them.

As earleir mentioned 17% o the data were input wrongly. So based on the
result of the regression we can fill them based on the new formula.

``` r
climsst_outlier <- coral %>% filter(ClimSST == 262.15)

climsst_outlier$ClimSST <- (climsst_outlier$TSA + 151.999254) / 0.501952

coral$ClimSST[coral$ClimSST == 262.15] <- climsst_outlier$ClimSST

ggplot(coral, aes(ClimSST)) + geom_histogram(bins = 30)
```

![](Joint_Markdown_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

I imputed the data that was mistake and outlier in ClimSST based on the
regression line and the result i got from the bootstrapped regression.
and t the end we have a normal distribution for ClimSST.
