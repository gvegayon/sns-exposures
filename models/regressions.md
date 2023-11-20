# Example of diffnet regressions

``` r
library(data.table)
library(texreg)
```

    Version:  1.38.6
    Date:     2022-04-06
    Author:   Philip Leifeld (University of Essex)

    Consider submitting praise using the praise or praise_interactive functions.
    Please cite the JSS article in your publications -- see citation("texreg").

``` r
library(lmtest)
```

    Loading required package: zoo


    Attaching package: 'zoo'

    The following objects are masked from 'package:base':

        as.Date, as.Date.numeric

``` r
dat <- fread("data/sns_model_data.csv")

# Filtering data to the toa date
dat_filtered <- dat[is.na(toa_smoke) | (year <= toa_smoke)]

# And only keeping individuals in times 1 and 4
# dat_filtered <- dat_filtered[year %in% c(1, 4)]
```

``` r
model0 <- glm(
  tobacco ~
    sibsmoke + adultdrink + rooms + factor(year) + Female +
    Grades + Hispanic + adultsmoke +
    factor(school),
  data = dat_filtered,
  family = binomial(link = "logit")
)
```

We now print the model using the `texreg` package:

``` r
texreg::knitreg(model0)
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>
Statistical models
</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">
 
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 1
</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
(Intercept)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.74
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.56)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
sibsmoke
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.15<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.19)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
adultdrink
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.43<sup>\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.17)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
rooms
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.07
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.06)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(year)2
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-1.12<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.25)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(year)3
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.38
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.24)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(year)4
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.31
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.21)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Female
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.25
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.17)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Grades
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.59<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.10)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Hispanic
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.70<sup>\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.26)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
adultsmoke
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.05
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.19)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(school)112
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.57<sup>\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.26)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(school)113
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.04
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.26)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(school)114
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.30
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.26)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(school)115
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.21
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.26)
</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
AIC
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1060.14
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
BIC
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1142.23
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Log Likelihood
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-515.07
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Deviance
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1030.14
</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
Num. obs.
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1759
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="2">
<sup>\*\*\*</sup>p \< 0.001; <sup>\*\*</sup>p \< 0.01; <sup>\*</sup>p \<
0.05
</td>
</tr>
</tfoot>
</table>

The variables room, female, and adult smoke were not significant.
Rerunning the regression:

``` r
model1 <- glm(
  tobacco ~
    sibsmoke + adultdrink + factor(year) +
    Grades + Hispanic +
    I(school == 112),
  data = dat_filtered,
  family = binomial(link = "logit")
)
```

Printing the output:

``` r
texreg::knitreg(list(model0, model1))
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>
Statistical models
</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">
 
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 1
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 2
</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
(Intercept)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.74
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.65
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.56)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.46)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
sibsmoke
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.15<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.13<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.19)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.19)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
adultdrink
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.43<sup>\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.41<sup>\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.17)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.17)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
rooms
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.07
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.06)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(year)2
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-1.12<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-1.12<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.25)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.25)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(year)3
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.38
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.38
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.24)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.23)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(year)4
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.31
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.31
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.21)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.21)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Female
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.25
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.17)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Grades
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.59<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.59<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.10)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.10)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Hispanic
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.70<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.69<sup>\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.26)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.23)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
adultsmoke
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.05
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.19)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(school)112
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.57<sup>\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.26)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(school)113
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.04
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.26)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(school)114
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.30
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.26)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(school)115
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.21
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.26)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
school == 112TRUE
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.58<sup>\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
(0.20)
</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
AIC
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1060.14
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1065.78
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
BIC
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1142.23
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1115.20
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Log Likelihood
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-515.07
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-523.89
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Deviance
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1030.14
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1047.78
</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
Num. obs.
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1759
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1791
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="3">
<sup>\*\*\*</sup>p \< 0.001; <sup>\*\*</sup>p \< 0.01; <sup>\*</sup>p \<
0.05
</td>
</tr>
</tfoot>
</table>

# Exposure models

## Exposure model 1

``` r
model_exp1 <- glm(
  tobacco ~
    sibsmoke + adultdrink + 
    factor(year) + # I(year == 1) +
    Grades + Hispanic + 
    I(school == 112) +
    exposure_smoke,
  data = dat_filtered,
  family = binomial(link = "logit")
)
```

## Exposure model 2

``` r
model_exp2 <- glm(
  tobacco ~
    sibsmoke + adultdrink + 
    factor(year) + # I(year == 1) +
    Grades + Hispanic + 
    I(school == 112) +
    exposure_count,
  data = dat_filtered,
  family = binomial(link = "logit")
)
```

## Exposure model 3

``` r
model_exp3 <- glm(
  tobacco ~
    sibsmoke + adultdrink + 
    factor(year) + # I(year == 1) +
    Grades + Hispanic + 
    I(school == 112) +
    exposure_smoke_se,
  data = dat_filtered,
  family = binomial(link = "logit")
)
```

## Exposure model 4

``` r
model_exp4 <- glm(
  tobacco ~
    sibsmoke + adultdrink + 
    factor(year) + # I(year == 1) +
    Grades + Hispanic + 
    I(school == 112) +
    exposure_female,
  data = dat_filtered,
  family = binomial(link = "logit")
)
```

## Exposure model 5

``` r
model_exp5 <- glm(
  tobacco ~
    sibsmoke + adultdrink + 
    factor(year) + # I(year == 1) +
    Grades + Hispanic + 
    I(school == 112) +
    exposure_2steps,
  data = dat_filtered,
  family = binomial(link = "logit")
)
```

## Exposure model 6

``` r
model_exp6 <- glm(
  tobacco ~
    sibsmoke + adultdrink + 
    factor(year) + # I(year == 1) +
    Grades + Hispanic + 
    I(school == 112) +
    exposure_indegree,
  data = dat_filtered,
  family = binomial(link = "logit")
)
```

## Expossure model 7

``` r
model_exp7 <- glm(
  tobacco ~
    sibsmoke + adultdrink + 
    factor(year) + # I(year == 1) +
    Grades + Hispanic + 
    I(school == 112) +
    exposure_perceived,
  data = dat_filtered,
  family = binomial(link = "logit")
)
```

## Exposure model 8

``` r
model_exp8 <- glm(
  tobacco ~
    sibsmoke + adultdrink + 
    factor(year) + # I(year == 1) +
    Grades + Hispanic + 
    I(school == 112) +
    exposure_density,
  data = dat_filtered,
  family = binomial(link = "logit")
)
```

## Exposure model 9

``` r
model_exp9 <- glm(
  tobacco ~
    sibsmoke + adultdrink + 
    factor(year) + # I(year == 1) +
    Grades + Hispanic + 
    I(school == 112) +
    exposure_team,
  data = dat_filtered,
  family = binomial(link = "logit")
)
```

## Exposure model 10

``` r
model_exp10 <- glm(
  tobacco ~
    sibsmoke + adultdrink + 
    factor(year) + # I(year == 1) +
    Grades + Hispanic +
    I(school == 112) +
    exposure_simmelian,
  data = dat_filtered,
  family = binomial(link = "logit")
)
```

Processing the models

``` r
fitted_models <- list(
  model1,
  model_exp1,
  model_exp2,
  model_exp3,
  model_exp4,
  model_exp5,
  model_exp6,
  model_exp7,
  model_exp8,
  model_exp9,
  model_exp10
  )

# We have to exponentiate the coefficients and
# remove the se so texreg doesn't print it
odds <- lapply(fitted_models, \(x) {
  x <- extract(x)
  x@coef <- exp(x@coef)
  x@se   <- numeric()
  x
})
```

``` r
texreg::knitreg(fitted_models, single.row = TRUE)
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>
Statistical models
</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">
 
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 1
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 2
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 3
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 4
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 5
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 6
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 7
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 8
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 9
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 10
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 11
</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
(Intercept)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.65 (0.46)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-2.33 (0.63)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-2.03 (0.61)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-1.52 (0.61)<sup>\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-2.27 (0.62)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-2.26 (0.63)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-2.24 (0.62)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-2.03 (0.61)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-2.23 (0.62)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-1.83 (0.60)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-1.88 (0.60)<sup>\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
sibsmoke
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.13 (0.19)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.99 (0.25)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.99 (0.25)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.99 (0.25)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.99 (0.25)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.97 (0.25)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.00 (0.25)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.99 (0.25)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.00 (0.25)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.98 (0.25)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.96 (0.25)<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
adultdrink
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.41 (0.17)<sup>\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.38 (0.21)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.36 (0.21)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.34 (0.21)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.39 (0.21)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.37 (0.21)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.37 (0.21)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.36 (0.21)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.40 (0.21)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.35 (0.21)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.39 (0.21)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(year)2
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-1.12 (0.25)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(year)3
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.38 (0.23)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.01 (0.31)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.92 (0.30)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.89 (0.29)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.99 (0.31)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.98 (0.31)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.98 (0.31)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.97 (0.31)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.97 (0.31)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.85 (0.30)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.88 (0.30)<sup>\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(year)4
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.31 (0.21)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.90 (0.27)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.82 (0.27)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.75 (0.27)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.91 (0.27)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.88 (0.27)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.87 (0.27)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.90 (0.27)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.86 (0.27)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.81 (0.27)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.81 (0.27)<sup>\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Grades
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.59 (0.10)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.49 (0.13)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.53 (0.13)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.55 (0.13)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.50 (0.13)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.51 (0.13)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.50 (0.13)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.54 (0.13)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.51 (0.13)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.56 (0.13)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-0.56 (0.13)<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Hispanic
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.69 (0.23)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.44 (0.29)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.49 (0.29)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.59 (0.28)<sup>\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.45 (0.29)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.45 (0.29)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.46 (0.29)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.50 (0.28)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.46 (0.29)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.54 (0.28)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.52 (0.28)
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
school == 112TRUE
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.58 (0.20)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.05 (0.27)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.03 (0.28)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.80 (0.28)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.01 (0.27)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.05 (0.28)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.03 (0.27)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.97 (0.27)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.06 (0.27)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.94 (0.27)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.99 (0.27)<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_smoke
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.30 (0.37)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_count
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.18 (0.09)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_smoke_se
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-1.35 (0.66)<sup>\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_female
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.23 (0.36)<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_2steps
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.27 (0.47)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_indegree
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.08 (0.36)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_perceived
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.85 (0.34)<sup>\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_density
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.08 (0.35)<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_team
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.33 (0.57)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_simmelian
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.46 (0.69)<sup>\*</sup>
</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
AIC
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1065.78
</td>
<td style="padding-left: 5px;padding-right: 5px;">
677.20
</td>
<td style="padding-left: 5px;padding-right: 5px;">
685.40
</td>
<td style="padding-left: 5px;padding-right: 5px;">
684.31
</td>
<td style="padding-left: 5px;padding-right: 5px;">
677.99
</td>
<td style="padding-left: 5px;padding-right: 5px;">
682.14
</td>
<td style="padding-left: 5px;padding-right: 5px;">
680.36
</td>
<td style="padding-left: 5px;padding-right: 5px;">
683.35
</td>
<td style="padding-left: 5px;padding-right: 5px;">
680.33
</td>
<td style="padding-left: 5px;padding-right: 5px;">
688.61
</td>
<td style="padding-left: 5px;padding-right: 5px;">
685.03
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
BIC
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1115.20
</td>
<td style="padding-left: 5px;padding-right: 5px;">
723.62
</td>
<td style="padding-left: 5px;padding-right: 5px;">
731.82
</td>
<td style="padding-left: 5px;padding-right: 5px;">
730.73
</td>
<td style="padding-left: 5px;padding-right: 5px;">
724.41
</td>
<td style="padding-left: 5px;padding-right: 5px;">
728.56
</td>
<td style="padding-left: 5px;padding-right: 5px;">
726.78
</td>
<td style="padding-left: 5px;padding-right: 5px;">
729.77
</td>
<td style="padding-left: 5px;padding-right: 5px;">
726.75
</td>
<td style="padding-left: 5px;padding-right: 5px;">
735.03
</td>
<td style="padding-left: 5px;padding-right: 5px;">
731.45
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Log Likelihood
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-523.89
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-329.60
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-333.70
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-333.15
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-330.00
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-332.07
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-331.18
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-332.67
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-331.16
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-335.31
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-333.51
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Deviance
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1047.78
</td>
<td style="padding-left: 5px;padding-right: 5px;">
659.20
</td>
<td style="padding-left: 5px;padding-right: 5px;">
667.40
</td>
<td style="padding-left: 5px;padding-right: 5px;">
666.31
</td>
<td style="padding-left: 5px;padding-right: 5px;">
659.99
</td>
<td style="padding-left: 5px;padding-right: 5px;">
664.14
</td>
<td style="padding-left: 5px;padding-right: 5px;">
662.36
</td>
<td style="padding-left: 5px;padding-right: 5px;">
665.35
</td>
<td style="padding-left: 5px;padding-right: 5px;">
662.33
</td>
<td style="padding-left: 5px;padding-right: 5px;">
670.61
</td>
<td style="padding-left: 5px;padding-right: 5px;">
667.03
</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
Num. obs.
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1791
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="12">
<sup>\*\*\*</sup>p \< 0.001; <sup>\*\*</sup>p \< 0.01; <sup>\*</sup>p \<
0.05
</td>
</tr>
</tfoot>
</table>

``` r
texreg::knitreg(
  odds, main = "Odds ratios"
)
```

<table class="texreg" style="margin: 10px auto;border-collapse: collapse;border-spacing: 0px;caption-side: bottom;color: #000000;border-top: 2px solid #000000;">
<caption>
Statistical models
</caption>
<thead>
<tr>
<th style="padding-left: 5px;padding-right: 5px;">
 
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 1
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 2
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 3
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 4
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 5
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 6
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 7
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 8
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 9
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 10
</th>
<th style="padding-left: 5px;padding-right: 5px;">
Model 11
</th>
</tr>
</thead>
<tbody>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
(Intercept)
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.52
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.10<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.13<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.22<sup>\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.10<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.10<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.11<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.13<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.11<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.16<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.15<sup>\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
sibsmoke
</td>
<td style="padding-left: 5px;padding-right: 5px;">
3.08<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.70<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.69<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.68<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.68<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.63<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.71<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.69<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.71<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.66<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.62<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
adultdrink
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.51<sup>\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.46
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.43
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.41
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.48
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.44
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.44
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.43
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.49
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.42
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.47
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(year)2
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.33<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(year)3
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.68
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.74<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.51<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.43<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.70<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.67<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.65<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.63<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.63<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.34<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.41<sup>\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
factor(year)4
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.73
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.45<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.27<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.12<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.47<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.42<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.38<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.47<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.36<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.25<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.24<sup>\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Grades
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.55<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.61<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.59<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.58<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.61<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.60<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.61<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.58<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.60<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.57<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.57<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Hispanic
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.00<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.55
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.63
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.80<sup>\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.56
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.57
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.58
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.65
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.58
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.72
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.68
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
school == 112TRUE
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.79<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.86<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.80<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.22<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.75<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.87<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.81<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.63<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.90<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.55<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.69<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_smoke
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
3.69<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_count
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.20
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_smoke_se
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
0.26<sup>\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_female
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
3.42<sup>\*\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_2steps
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
3.56<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_indegree
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.95<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_perceived
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.34<sup>\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_density
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
2.94<sup>\*\*</sup>
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_team
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1.39
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
exposure_simmelian
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
 
</td>
<td style="padding-left: 5px;padding-right: 5px;">
4.29<sup>\*</sup>
</td>
</tr>
<tr style="border-top: 1px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
AIC
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1065.78
</td>
<td style="padding-left: 5px;padding-right: 5px;">
677.20
</td>
<td style="padding-left: 5px;padding-right: 5px;">
685.40
</td>
<td style="padding-left: 5px;padding-right: 5px;">
684.31
</td>
<td style="padding-left: 5px;padding-right: 5px;">
677.99
</td>
<td style="padding-left: 5px;padding-right: 5px;">
682.14
</td>
<td style="padding-left: 5px;padding-right: 5px;">
680.36
</td>
<td style="padding-left: 5px;padding-right: 5px;">
683.35
</td>
<td style="padding-left: 5px;padding-right: 5px;">
680.33
</td>
<td style="padding-left: 5px;padding-right: 5px;">
688.61
</td>
<td style="padding-left: 5px;padding-right: 5px;">
685.03
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
BIC
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1115.20
</td>
<td style="padding-left: 5px;padding-right: 5px;">
723.62
</td>
<td style="padding-left: 5px;padding-right: 5px;">
731.82
</td>
<td style="padding-left: 5px;padding-right: 5px;">
730.73
</td>
<td style="padding-left: 5px;padding-right: 5px;">
724.41
</td>
<td style="padding-left: 5px;padding-right: 5px;">
728.56
</td>
<td style="padding-left: 5px;padding-right: 5px;">
726.78
</td>
<td style="padding-left: 5px;padding-right: 5px;">
729.77
</td>
<td style="padding-left: 5px;padding-right: 5px;">
726.75
</td>
<td style="padding-left: 5px;padding-right: 5px;">
735.03
</td>
<td style="padding-left: 5px;padding-right: 5px;">
731.45
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Log Likelihood
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-523.89
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-329.60
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-333.70
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-333.15
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-330.00
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-332.07
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-331.18
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-332.67
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-331.16
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-335.31
</td>
<td style="padding-left: 5px;padding-right: 5px;">
-333.51
</td>
</tr>
<tr>
<td style="padding-left: 5px;padding-right: 5px;">
Deviance
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1047.78
</td>
<td style="padding-left: 5px;padding-right: 5px;">
659.20
</td>
<td style="padding-left: 5px;padding-right: 5px;">
667.40
</td>
<td style="padding-left: 5px;padding-right: 5px;">
666.31
</td>
<td style="padding-left: 5px;padding-right: 5px;">
659.99
</td>
<td style="padding-left: 5px;padding-right: 5px;">
664.14
</td>
<td style="padding-left: 5px;padding-right: 5px;">
662.36
</td>
<td style="padding-left: 5px;padding-right: 5px;">
665.35
</td>
<td style="padding-left: 5px;padding-right: 5px;">
662.33
</td>
<td style="padding-left: 5px;padding-right: 5px;">
670.61
</td>
<td style="padding-left: 5px;padding-right: 5px;">
667.03
</td>
</tr>
<tr style="border-bottom: 2px solid #000000;">
<td style="padding-left: 5px;padding-right: 5px;">
Num. obs.
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1791
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
<td style="padding-left: 5px;padding-right: 5px;">
1284
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="font-size: 0.8em;" colspan="12">
<sup>\*\*\*</sup>p \< 0.001; <sup>\*\*</sup>p \< 0.01; <sup>\*</sup>p \<
0.05
</td>
</tr>
</tfoot>
</table>
