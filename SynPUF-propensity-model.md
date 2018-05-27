SynPUF-propensity-model
================
Erika Braithwaite
2018-05-23

First select individuals above 40 at the time they first showed up in the dataset; then exclude anyone died within one year; then select individuals with statin prescription after 2009-01-01 (set first prescription date as cohort entry date); then exclude anyone who took lipid-lowering drugs before 2009-01-01; also exclude anyone who had dialysis one year before cohort entry

### Table 1. Covariate balance in total sample N = 839

|                            |  low statin | high statin |  SMD  |
|----------------------------|:-----------:|:-----------:|:-----:|
| n                          |     6425    |     1036    |       |
| hypertension = Yes (%)     | 2214 (34.5) |  375 (36.2) | 0.036 |
| hypercholesterol = Yes (%) | 1607 (25.0) |  285 (27.5) | 0.057 |
| heart.failure = Yes (%)    |  732 (11.4) |  105 (10.1) | 0.041 |
| PVD = Yes (%)              |  126 ( 2.0) |  22 ( 2.1)  | 0.011 |
| injury.poison = Yes (%)    |  991 (15.4) |  167 (16.1) | 0.019 |
| hospital = Yes (%)         |  947 (14.7) |  171 (16.5) | 0.049 |
| lab.test = Yes (%)         | 3168 (49.3) |  526 (50.8) | 0.029 |

### Estimate propensity score with nearest neighbor 1:1 matching

``` r
model = matchit(statin ~ hypertension + hypercholesterol + heart.failure + 
                injury.poison + hospital + lab.test, method = 'nearest', data = dat)

kable(model$nn)
```

|           |  Control|  Treated|
|-----------|--------:|--------:|
| All       |     6425|     1036|
| Matched   |     1036|     1036|
| Unmatched |     5389|        0|
| Discarded |        0|        0|

### Pruned data

|                            | low statin | high statin |    SMD    |
|----------------------------|:----------:|:-----------:|:---------:|
| n                          |    1036    |     1036    |           |
| hypertension = Yes (%)     | 375 (36.2) |  375 (36.2) | &lt;0.001 |
| hypercholesterol = Yes (%) | 285 (27.5) |  285 (27.5) | &lt;0.001 |
| heart.failure = Yes (%)    | 105 (10.1) |  105 (10.1) | &lt;0.001 |
| PVD = Yes (%)              |  12 ( 1.2) |  22 ( 2.1)  |   0.076   |
| injury.poison = Yes (%)    | 167 (16.1) |  167 (16.1) | &lt;0.001 |
| hospital = Yes (%)         | 171 (16.5) |  171 (16.5) | &lt;0.001 |
| lab.test = Yes (%)         | 526 (50.8) |  526 (50.8) | &lt;0.001 |

### Comparing SMD between matched and unmatched sample

|     variable     | unmatched sample | matched sample |
|:----------------:|:----------------:|:--------------:|
|   hypertension   |       0.04       |      0.00      |
| hypercholesterol |       0.06       |      0.00      |
|   heart.failure  |       0.04       |      0.00      |
|        PVD       |       0.01       |      0.08      |
|   injury.poison  |       0.02       |      0.00      |
|     hospital     |       0.05       |      0.00      |
|     lab.test     |       0.03       |      0.00      |

### Distribution of standardized mean differences before and after matching

![](SynPUF-propensity-model_files/figure-markdown_github/distribution-plot-matched-1.png)

### Distribution of propensity scores in matched and unmatched sample

![](SynPUF-propensity-model_files/figure-markdown_github/distribution-plot-unmatched-1.png)

### Outcome models

    ## Warning: Removed 1 rows containing missing values (geom_pointrange).

![](SynPUF-propensity-model_files/figure-markdown_github/Plot-models-1.png)
