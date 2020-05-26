
Student Wellness
================
Aman Panwar


**1. Problem Context**

Physical inactivity has been responsible for six percent of the global mortality and it has been ranked as the fourth leading risk factor of death (World Health Organization, 2010). For the age group of 5???17 years, physical activity provides essential health benefits for children and youth (Janssen, 2007; Janssen & Leblanc, 2010; Physical Activity Guidelines Advisory Committee, 2008). In addition, the behaviors related to health during the early teenage years between 11 and 16 years are crucial because the patterns of the health-related habits in their adulthoods emerge at this time (Gregory & Lowe, 2000). In this context, the importance of students' health and the necessity to figure out of the factors influencing the students' physical fitness have been emphasizing. Safe neighborhoods that are free of crime and violence are an integral component of healthy neighborhoods. In addition to direct physical and mental impacts of crime, fear of crime and violence inhibit the use of community assets for physical exercise at nearby parks and playgrounds or walking or bicycling to commute to local destinations for basic needs (Fowler et al., 2009; Takagi, Ken'ichi, & Kawachi, 2012).

In this project, we decided to use multivariate statistical techniques to obtain deeper insights into the relationship between the rate of crime of an area and physical fitness of students living there.

**Datasets:**

Two datasets: physical fitness and crime datasets were used in this project.

Physical fitness dataset had physical fitness score for students who attended grades five, seven, and nine in California public schools from 1998 to 2018.

Crime dataset had the rate of crime which represents the number of violent crimes per 1,000 population. Four types of crimes (murder, rape, robbery, and assault) and the total number of crimes are reported from years 2000 to 2013 at the geographical levels of city/towns, counties, regions, and state.

Libraries to perform multivariate analysis

``` r
library(dplyr)
library(reshape2)
library(imp4p)
library(MVA)
library('ResourceSelection')
library('webshot')
```

**2. Data Cleaning**

Physical fitness dataset

``` r
#Physical fitness data
p_fit = read.csv("Physical_fitness.csv")
#Changes name of the county column to make column name consistent in both datasets
names(p_fit)[5] <- "county_name"

#Subsets data to include only necessary columns
p_fit_1  <- p_fit[, c("Year", "county_name", "Grade.Lev", "Percent", "Strata")]
head(p_fit_1)
```

    ##        Year county_name Grade.Lev Percent Strata
    ## 1 1998-1999     Alameda   Grade 5   21.17   None
    ## 2 1998-1999     Alameda   Grade 7   22.18   None
    ## 3 1998-1999     Alameda   Grade 9   19.65   None
    ## 4 1998-1999      Amador   Grade 5    8.78   None
    ## 5 1998-1999      Amador   Grade 7    1.40   None
    ## 6 1998-1999      Amador   Grade 9   22.94   None

``` r
#The crime dataset had data for only 14 years so data for all years except 2000-2013 was removed from the physical fitness dataset.

#Removes unnecessary rows from Year columns to get data only for 14 years (2000-2013)
p_fit_2 <- p_fit_1[!(p_fit_1$Year=="1998-1999"|p_fit_1$Year=="1999-2000"
                     |p_fit_1$Year=="2014-2015"|p_fit_1$Year=="2015-2016"|p_fit_1$Year=="2016-2017"|p_fit_1$Year=="2017-2018"|    
                       p_fit_1$county_name == "California"|p_fit_1$county_name == "Ca Education Authority"),]

#Selects data for all students
p_fit_3 <- subset(p_fit_2, Strata=="None")

#Checks for missing values in each column
colSums(is.na(p_fit_3))
```

    ##        Year county_name   Grade.Lev     Percent      Strata 
    ##           0           0           0          15           0

``` r
#County column will be used to merge both datasets
#Groups data by county
p_fit_group <- group_by(p_fit_3,county_name)

#Calculates average physical fitness scores for all students from all grades and years and grouped the data by county
summ_fit <- summarize(p_fit_group, avg_fitness_score =mean(Percent))
head(summ_fit)
```

    ## # A tibble: 6 x 2
    ##   county_name avg_fitness_score
    ##   <chr>                   <dbl>
    ## 1 Alameda                  31.0
    ## 2 Alpine                   NA  
    ## 3 Amador                   32.8
    ## 4 Butte                    32.3
    ## 5 Calaveras                33.1
    ## 6 Colusa                   26.4

Crime dataset

``` r
#Crime dataset
v_crime <- read.csv("crime.csv")

# Selects necessary columns
v_crime_2 <- v_crime[, c("reportyear", "county_name", "strata_level_name", "geotype", "rate")]

# Subsets data based on county
v_crime_3 <- subset(v_crime_2, geotype=="CO")

head(v_crime_3)
```

    ##    reportyear county_name                     strata_level_name geotype
    ## 6        2000     Alameda                    Aggravated assault      CO
    ## 7        2000     Alameda                         Forcible rape      CO
    ## 8        2000     Alameda Murder and non-negligent manslaughter      CO
    ## 9        2000     Alameda                               Robbery      CO
    ## 10       2000     Alameda                   Violent crime total      CO
    ## 11       2000      Alpine                    Aggravated assault      CO
    ##           rate
    ## 6  3.779763822
    ## 7  0.393422366
    ## 8  0.076190951
    ## 9  2.332828395
    ## 10 6.582205534
    ## 11 7.450331126

``` r
#converts crime rate from char to numeric
v_crime_3$rate <- as.numeric(v_crime_3$rate)

#Checks for missing values in each column
colSums(is.na(v_crime_3))
```

    ##        reportyear       county_name strata_level_name           geotype 
    ##                 0                 0                 0                 0 
    ##              rate 
    ##                 0

``` r
#groups data by county
v_crime_group <- group_by(v_crime_3,county_name,strata_level_name )
head(v_crime_group)
```

    ## # A tibble: 6 x 5
    ## # Groups:   county_name, strata_level_name [6]
    ##   reportyear county_name strata_level_name                     geotype   rate
    ##        <int> <chr>       <chr>                                 <chr>    <dbl>
    ## 1       2000 Alameda     Aggravated assault                    CO      3.78  
    ## 2       2000 Alameda     Forcible rape                         CO      0.393 
    ## 3       2000 Alameda     Murder and non-negligent manslaughter CO      0.0762
    ## 4       2000 Alameda     Robbery                               CO      2.33  
    ## 5       2000 Alameda     Violent crime total                   CO      6.58  
    ## 6       2000 Alpine      Aggravated assault                    CO      7.45

``` r
#summarizes average crime rate for each county for time period between 2000-2013
summ_crime <- summarize(v_crime_group, avg_crime_rate= mean(rate)) 
head(summ_crime )
```

    ## # A tibble: 6 x 3
    ## # Groups:   county_name [2]
    ##   county_name strata_level_name                     avg_crime_rate
    ##   <chr>       <chr>                                          <dbl>
    ## 1 Alameda     Aggravated assault                            3.45  
    ## 2 Alameda     Forcible rape                                 0.339 
    ## 3 Alameda     Murder and non-negligent manslaughter         0.0907
    ## 4 Alameda     Robbery                                       3.38  
    ## 5 Alameda     Violent crime total                           7.27  
    ## 6 Alpine      Aggravated assault                            6.92

``` r
#summarizes average crime rate for each type of crime for each county 
v_crime_4 <- dcast(summ_crime, county_name~strata_level_name, value.var = "avg_crime_rate")
# Change the name of the columns
colnames(v_crime_4) <- c("county_name", "Aggravated_assault", "Forcible_rape", "Murder",
                         "Robbery", "Violent_crime_total")
head(v_crime_4)
```

    ##   county_name Aggravated_assault Forcible_rape     Murder    Robbery
    ## 1     Alameda           3.454232     0.3389187 0.09065344 3.38361939
    ## 2      Alpine           6.920425     1.1025820 0.12425404 0.06848377
    ## 3      Amador           2.715927     0.4751976 0.01700189 0.22984023
    ## 4       Butte           2.236485     0.4136845 0.04101024 0.70385548
    ## 5   Calaveras           2.146321     0.3083362 0.04219656 0.22369852
    ## 6      Colusa           2.062313     0.2745729 0.03175350 0.28663056
    ##   Violent_crime_total
    ## 1            7.267424
    ## 2            8.215745
    ## 3            3.437967
    ## 4            3.395035
    ## 5            2.723563
    ## 6            2.655270

**Data merging**

``` r
merged_data <- merge(summ_fit, v_crime_4, by="county_name")
#removes categorical column to allow replacement of missing values with median
merged_data.1 <- merged_data[, -1]

#Handling of Missing Values

# Missing value imputation with median
for(q in 1:ncol(merged_data.1)){
  merged_data.1[is.na(merged_data.1[,q]),q] <- median(merged_data.1[,q], na.rm=TRUE)
}

colSums(is.na(merged_data.1))
```

    ##   avg_fitness_score  Aggravated_assault       Forcible_rape              Murder 
    ##                   0                   0                   0                   0 
    ##             Robbery Violent_crime_total 
    ##                   0                   0

``` r
#No missing values in merged dataset
head(merged_data.1)
```

    ##   avg_fitness_score Aggravated_assault Forcible_rape     Murder    Robbery
    ## 1          30.95929           3.454232     0.3389187 0.09065344 3.38361939
    ## 2          30.76298           6.920425     1.1025820 0.12425404 0.06848377
    ## 3          32.78595           2.715927     0.4751976 0.01700189 0.22984023
    ## 4          32.28238           2.236485     0.4136845 0.04101024 0.70385548
    ## 5          33.09738           2.146321     0.3083362 0.04219656 0.22369852
    ## 6          26.37119           2.062313     0.2745729 0.03175350 0.28663056
    ##   Violent_crime_total
    ## 1            7.267424
    ## 2            8.215745
    ## 3            3.437967
    ## 4            3.395035
    ## 5            2.723563
    ## 6            2.655270

**3. Data Visualization**

``` r
#Bivariate Boxplot to find the outliers
county <- merged_data$county_name
merged_data.2 <- cbind(county,merged_data.1)
bvbox(merged_data.2[, c("avg_fitness_score","Violent_crime_total")])
text(merged_data.2[, c("avg_fitness_score", "Violent_crime_total")], labels = abbreviate(merged_data$county_name), cex = 0.7)
```

![](https://github.com/panwaraman3031/Student_Wellness/blob/master/1.png)


The bivariate plot indicates that there were 5 outlier counties that needed to be removed to get more reliable results, which increases the reliability of drawn conclusions from this analysis.

``` r
#Removes outliers
outlier <- match(c("Alpine", "San Joaquin", "Imperial", "El Dorado", "Marin"), merged_data.2$county)

#Dataset without any outliers
merged_data.o <- merged_data.2[-outlier, ]

#Scatterplot with labels and rugs(scales)
plot(merged_data.o$avg_fitness_score, merged_data.o$Violent_crime_total)
rug(merged_data.o$avg_fitness_score, side=1)
rug(merged_data.o$Violent_crime_total, side = 2)
text(merged_data.o$avg_fitness_score, merged_data.o$Violent_crime_total, cex = 0.6, labels = abbreviate(merged_data.o$county))
```
![](https://github.com/panwaraman3031/Student_Wellness/blob/master/2.png)

**4. Dimension Reduction Analysis**

There are 6 continuous variables in our dataset, and it can be difficult if someone needs to derive insight from this dataset with several variables. To help overcome this problem, PCA (Principal Component Analysis) was performed to explain the dataset comprehensively with minimum possible variables.PCA was performed on the fitness-crime(merged) dataset with outliers removed from it.

``` r
#Removes categorical column for PCA
merged_data.3 <- merged_data.o[,-1]
# Principal component analysis
merged_data.3_pca <- princomp(merged_data.3, cor = T) 
summary(merged_data.3_pca)
```

    ## Importance of components:
    ##                           Comp.1    Comp.2    Comp.3     Comp.4     Comp.5
    ## Standard deviation     1.8170478 1.1462090 0.8189177 0.68032007 0.50106573
    ## Proportion of Variance 0.5502771 0.2189659 0.1117710 0.07713923 0.04184448
    ## Cumulative Proportion  0.5502771 0.7692430 0.8810140 0.95815325 0.99999773
    ##                              Comp.6
    ## Standard deviation     3.688972e-03
    ## Proportion of Variance 2.268085e-06
    ## Cumulative Proportion  1.000000e+00

``` r
#Biplot for PCA analysis
biplot(merged_data.3_pca, col=c("black", "red"), cex = 0.65)
```

![](https://github.com/panwaraman3031/Student_Wellness/blob/master/3.png)

``` r
merged_data.3_pca$loadings
```

    ## 
    ## Loadings:
    ##                     Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6
    ## avg_fitness_score    0.384  0.181  0.729  0.441  0.306       
    ## Aggravated_assault  -0.426 -0.384 -0.104  0.655         0.476
    ## Forcible_rape              -0.754  0.477 -0.444              
    ## Murder              -0.474  0.216        -0.270  0.810       
    ## Robbery             -0.405  0.452  0.420 -0.219 -0.446  0.457
    ## Violent_crime_total -0.529         0.229  0.244 -0.216 -0.749
    ## 
    ##                Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6
    ## SS loadings     1.000  1.000  1.000  1.000  1.000  1.000
    ## Proportion Var  0.167  0.167  0.167  0.167  0.167  0.167
    ## Cumulative Var  0.167  0.333  0.500  0.667  0.833  1.000

Interpretation of PCA:

The first two principal components (PC) represents 77% of the total variance and it is therefore enough to use first two PC to represent the original dataset. Based on their respective loadings, PC 1 can be thought to represent all crimes except Forcible\_rape and Robbery and PC 2 can be thought to represent Forcible\_rape and Robbery. A simple interpretation of PC1 score could be that county with higher PC1 score will have lower crime rates especially for the crimes with higher absolute magnitude of PC loadings and higher physical fitness level.

**4. Cluster Analysis**

We performed cluster analysis for discovering all possible clusters of most possible homogeneous observations in our fitness-crime dataset.

``` r
# Kdepairs plot for detecting clusters
kdepairs(merged_data.o[,2:7])
```

![](https://github.com/panwaraman3031/Student_Wellness/blob/master/4.png)

Based on KDE plot, there are one or two clusters in all our data.

We considered two clustering methods:hierarchical and K-means clustering.

In hierarchical clustering, data is not partitioned into particular groups in a single step. The process requires a series of partitions that can run from a single cluster containing all individuals to n clusters, each containing a single individual. This clustering method uses the distance matrix (standardized Euclidean distance) and is based on the choices of distance between groups further divided into three categories: single, complete and average.

``` r
# Hierarchical clustering ---- Single, complete,average
merged_data.o_hc <- hclust(dist(merged_data.3), "single")
merged_data.o_hc <- hclust(dist(merged_data.3), "complete")
merged_data.o_hc <- hclust(dist(merged_data.3), "average")
```

``` r
# Constructs scree plot to determine number of hierarchical clusters
merged_data.3.s <- dist(scale(merged_data.o[,-1]))
hc1 <- hclust(merged_data.3.s, "average")
plot(rev(hc1$height))
```

![](https://github.com/panwaraman3031/Student_Wellness/blob/master/5.png)

The screeplot shows that there are 2 clusters in our data.

``` r
ct <- cutree(hc1, 2)
#plots the clustered data
plot(merged_data.o[, c(2,7)], col = ct, main = "HC Clusters") 
text( merged_data.o[, c(2,7)],labels =abbreviate(merged_data.o$county), col = ct,pch=" ")
```

![](https://github.com/panwaraman3031/Student_Wellness/blob/master/6.png)

``` r
#K-means clustering 
#removes first column
merged_data.o.1=merged_data.o[,-1]

#scree plot
maxc = 20
plot.wgss = function(merged_data.o.1, maxc) {
  wss = numeric(maxc)
  for (i in 1:maxc)
    wss[i] = kmeans(merged_data.o.1, centers=i, nstart = 20)$tot.withinss
  plot(1:maxc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", main="Scree Plot")
}
plot.wgss(merged_data.o.1, 20)
```

![](https://github.com/panwaraman3031/Student_Wellness/blob/master/7.png)

``` r
km <- kmeans(scale(merged_data.o.1), centers = 3)

plot(merged_data.o[, c(2,7)], col =km$cluster, main = "kmeans Clusters") 
```

![](https://github.com/panwaraman3031/Student_Wellness/blob/master/8.png)

We eventually concluded that hierarchical based clustering method is the best method to segregate our observations into two clusters because it was evident in its plot that there was clear separation between the observations from two clusters. Although, in the absence of information on true clusters, it is difficult to be completely confident if the data has been well segregated.

**Supporting Information:**

Please check these documents for additional information.

Student-Wellness.Rmd: Rmarkdown file that contains the source code.

Student-Wellness-report: Detailed word report of this project.

Physical_fitness.csv: Physical fitness data

Crime.csv: Crime data

**References:** 

● Fowler, P. J. et al. (2009). Community violence: A meta-analysis on the effect of exposure and mental health outcomes of children and adolescents. Dev Psychopathol, 21(1), 227−259. 

● Takagi, D., Ken'ichi, I., & Kawachi, I. (2012). Neighborhood social capital and crime victimization: Comparison of spatial regression analysis. Soc Sci Med. 75(10), 1895−902.
