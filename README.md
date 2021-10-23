# ParallelTrendsPlot

A very simple package to draw plots that help to assess the parallel trends assumption in DID regression with additional control variables. For background and an example see my blog post here (the package is introduced fairly at the end of the post):

http://skranz.github.io/r/2021/10/20/ParallelTrendsPlot.html

## Installation

The package is hosted on [r-universe](https://skranz.r-universe.dev/ui#builds). To install it run:

```r
options(repos = c(
    skranz = 'https://skranz.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
install.packages('ParallelTrendsPlot')
```
