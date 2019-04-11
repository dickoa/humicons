
<!-- README.md is generated from README.Rmd. Please edit that file -->

An R package to easily insert humanitarian web icons into rmarkdown,
shiny and as text (to include in `ggplot2`).

### Shiny

```` 

```r
humicons::humicon("Agriculture")
```
````

### R markdown

```` 

```r
humicons::humicon_rmd("Agriculture")
```
````

### Text

```` 

```r
humicons::humicon_chr("Agriculture")
```
````

The **development** version can be installed from GitLab using:

``` r
# install.packages("remotes")
remotes::install_gitlab("dickoa/humicons")
```
