# Pi-estimate-in-R
To practice data visualization in R, I implemented a Monte Carlo and numerical estimation of pi using the unit circle. 

Monte Carlo and numeric estimations of pi
================
John Brown
9/2/2022

## Preface

Here I’ll approximate pi with a Monte Carlo simulation and through
integration. I’ll do this through use of a semi unit circle function:
![f(x) = (1-x^2)^\frac{1}{2}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;f%28x%29%20%3D%20%281-x%5E2%29%5E%5Cfrac%7B1%7D%7B2%7D "f(x) = (1-x^2)^\frac{1}{2}")

``` r
#install.packages("latex2exp")
library(latex2exp)
```


``` r
library(ggplot2)

x = seq(-1, 1, .0001)
fun <- function(x) {(1 - x^2)^.5}
y = fun(x)

half_circle = data.frame(x, y)

p <- ggplot(half_circle, aes(x = x, y= y)) +
  geom_line(color = "aquamarine") +
  theme_minimal() +
  coord_fixed() +
  labs(y = "f(x)") 
  
p + 
  xlim(c(-1.2,1.2)) +
  ylim(c(0, 1.2))+
  annotate("text", x = .95, y= 1.1, label = TeX("$f(x) = (1-x^2)^{1/2}$"), parse = TRUE) +
  labs(title = "Semi Unit Circle")
```

![](Pi_estimate_in_R_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Quarter unit circle

To simplify the simulation, I’ll set the x axis limits from 0 to 1. This
gives a quarter of the unit circle. Around it, I’ll plot the unit square

``` r
x_coords = c(0, 1, 0, 1)
y_coords = c(0, 0, 1, 1)
unit_square = data.frame(x_coords, y_coords)

quarter_p = p + 
  xlim(c(0,1.2)) +
  ylim(c(0, 1.2)) +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), fill = NA, col = "deeppink2") +
  labs(title = "Quarter Unit Circle")

quarter_p
```


![](Pi_estimate_in_R_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Since the radius of the unit circle is 1 and the formula for the area of
a circle is
![\pi r^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cpi%20r%5E2 "\pi r^2"),
the area of the unit circle is
![\pi](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cpi "\pi").
Therefore the area of the quarter unit circle is
![\frac{\pi}{4}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cfrac%7B%5Cpi%7D%7B4%7D "\frac{\pi}{4}").
For a Monte Carlo estimation of
![\pi](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cpi "\pi")
I’ll sample from a bivariate standard uniform distribution. The standard
uniform distribution has an equal probability of producing values from 0
to 1. So, each bivariate sample will be a point within the bounds of the
unit square, see below:

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# bivariate sampling from the standard uniform distrubution
x_sample <- runif(100, 0, 1)
y_sample <- runif(100, 0, 1)

bivariate_df <- data.frame(x_sample, y_sample)
bivariate_df <- bivariate_df %>%
  mutate(in_circle = sqrt(x_sample^2 + y_sample^2) <= 1)

quarter_p + 
  geom_point(data = bivariate_df, aes(x_sample , y_sample, col = in_circle)) 
```
![](Pi_estimate_in_R_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

The above plot shows 100 randomly sampled points. If we count the number
of (x, y) pairs the satisfy
![\sqrt{x^2 + y^2} \\ \leq \\ 1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csqrt%7Bx%5E2%20%2B%20y%5E2%7D%20%5C%20%5Cleq%20%5C%201 "\sqrt{x^2 + y^2} \ \leq \ 1")
(blue dots) and divide by the total number of points sampled (100), we
will get the proportion of points that fell within the quarter unit
circle. this should approximate
![\frac{\pi}{4}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cfrac%7B%5Cpi%7D%7B4%7D "\frac{\pi}{4}")

## Monte Carlo simulation

I’ll now repeat this process with 1 million points and plot distrubution
of points that satisfy
![\sqrt{x^2 + y^2} \\ \leq \\ 1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csqrt%7Bx%5E2%20%2B%20y%5E2%7D%20%5C%20%5Cleq%20%5C%201 "\sqrt{x^2 + y^2} \ \leq \ 1")
The shape of the histogram should approximate the are under the quarter
unit circle

``` r
# Makes a list of 10 lists that will each contain 2K  newly generated points over 
#50 iterations to reach 1 mil points
l1 <- list()

list_of_list <- list(l1,l1,l1,l1,l1,l1,l1,l1,l1,l1)

# Makes histogram of Monte Carlo simulation
num_attempts = 0
master_list = list()

for (itr in 1:50)
{
  for (idx in 1:10)
  {
    while (length(list_of_list[[idx]]) < 2000)
      {
        x1 = runif(1, 0, 1)
        y1 = runif(1, 0, 1)
        radicand = x1^2 + y1^2
          if (sqrt(radicand) <= 1)
          {
            list_of_list[[idx]] <- append(list_of_list[[idx]], y1)
            radicand = NULL
          }
        num_attempts = num_attempts +1
        
      }
  }
  points <- unlist(list_of_list)
  master_list <- append(master_list, points)
  list_of_list <- list(l1,l1,l1,l1,l1,l1,l1,l1,l1,l1)
}


master_list <- unlist(master_list)
master_list <- data.frame(master_list)

ggplot(master_list, aes(x = master_list, fill = cut(master_list,100))) +
  geom_histogram(show.legend = FALSE, binwidth = 0.01,) +
  theme_minimal() +
  xlim(0,1) +
  scale_fill_discrete(h = c(180, 360), c = 150, l = 80)
```


![](Pi_estimate_in_R_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Now 1,000,000 divided by the total number of points generated will give
us an approximation of the area under the Quarter unit circle. When this
number is multiplied by 4 we get an estimation of pi

``` r
prop_in_circle = 1000000 / num_attempts
MC_pi <- prop_in_circle * 4
MC_pi
```

    ## [1] 3.140637

## Numeric estimation of pi

For a numerical estimate of pi, we simply integrate under the Quarter
unit circle function. Below I’ll show
![\pi \approx\int_0^1 \left(1-x^2\right)^{\frac{1}{2}} dx \\ \\ \* \\ 4](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cpi%20%5Capprox%5Cint_0%5E1%20%5Cleft%281-x%5E2%5Cright%29%5E%7B%5Cfrac%7B1%7D%7B2%7D%7D%20dx%20%5C%20%5C%20%2A%20%5C%204 "\pi \approx\int_0^1 \left(1-x^2\right)^{\frac{1}{2}} dx \ \ * \ 4")

``` r
integrand = function(x) {(1-x^2)^.5}
answer <- integrate(integrand, lower = 0, upper = 1)
quarter_area <- answer[[1]]
pi_est <- quarter_area * 4
pi_est
```

    ## [1] 3.141593
