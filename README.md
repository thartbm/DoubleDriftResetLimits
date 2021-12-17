# Double Drift Reset Limits

R analyses and PsychoPy experiments to test limits to spontaneous resets in the double-drift illusion. We expected to see that resets of this illusion would be limited in space: the perceived position of the stimulus can't be more than some distance away from the real position. We actually find no limits, but rather a broad distribution of reset points over time.

Here is the pre-print:

[Measuring the double-drift illusion with hand trajectories](https://doi.org/10.1101/2021.08.06.455415)

Accompanies the data in this Open Science Framework repository:

https://osf.io/72ndu/

## "Running" Manuscript.Rmd

Software needed: 
- [R](https://cran.r-project.org/)
- [RStudio](https://www.rstudio.com/products/rstudio/download/) (free desktop version for your OS)
- [ImageMagick](https://imagemagick.org/)

When installing ImageMagick, make sure to install the developer files as well, in particular R will want to have `magick-baseconfig.h`.

*Linux:* ensure you have: `make` (probably depends on having a C compiler)

*Windows:* lacks some command line tools, so they are here: [Rtools](https://cran.r-project.org/bin/windows/Rtools) This mostly makes the mingw64 C compiler available to R: useful!

Make sure to follow the steps that put rtools on your system PATH though. Perhaps this is the easiest way to do it is to run this in the R console:

`write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)`

On any system, you might want to add `devtools` as well. There's a [devtool guide](https://www.r-project.org/nosvn/pandoc/devtools.html) about this.

Once you open the `Manuscript.Rmd` file, you should be able to "knit" the manuscript to Word, PDF or HTML (somewhere at the top of the document, there should be a 'knit' drop down menu). When you select your output of choice, the first (hidden) chunk of R code will:

1. Set up the R environment (most time consuming step),
2. Source all project-specific functions that we wrote and use for the manuscript
3. Download the raw data from OSF, if not already there
4. And pre-process the data for all other functions  

The most time-consuming is probably the first step, which is meant to (try to) install the versions of packages that I relied on while writing the code. It's done by this line in the `Manuscript.Rmd` file:

`renv::restore()`

This is the line that is most likely to fail, so after tinkering with stuff, you might want to retry it. It can also take a lot of time, unfortunately.

I've tried running it on my old Linux system, which worked well because most of the packages are also old and can be copied from the systems installed versions. I've also tried running this in Windows with a new install of R, RStudio and Rtools, and it takes ages, because a hundred old packages need to be downloaded and, often, compiled. Here is the current output, which lists the package dependencies:

> renv::restore()
The following package(s) will be updated:

# CRAN ===============================
- MASS           [7.3-54 -> 7.3-51.4]
- Matrix         [1.3-4 -> 1.2-17]
- R6             [2.5.1 -> 2.5.0]
- Rcpp           [1.0.7 -> 1.0.6]
- boot           [1.3-28 -> 1.3-23]
- cli            [3.1.0 -> 2.2.0]
- colorspace     [2.0-2 -> 1.4-1]
- cpp11          [0.4.2 -> 0.2.6]
- crayon         [1.4.2 -> 1.4.0]
- curl           [4.3.2 -> 4.2]
- digest         [0.6.29 -> 0.6.27]
- fansi          [0.5.0 -> 0.4.2]
- foreign        [0.8-81 -> 0.8-72]
- ggplot2        [3.3.5 -> 3.3.3]
- glue           [1.6.0 -> 1.4.2]
- highr          [0.9 -> 0.8]
- isoband        [0.2.5 -> 0.2.3]
- knitr          [1.37 -> 1.31]
- lattice        [0.20-45 -> 0.20-38]
- mgcv           [1.8-38 -> 1.8-31]
- nlme           [3.1-153 -> 3.1-142]
- nloptr         [1.2.2.3 -> 1.2.2.2]
- nnet           [7.3-16 -> 7.3-12]
- rlang          [0.4.12 -> 0.4.10]
- scales         [1.1.1 -> 1.0.0]
- stringi        [1.7.6 -> 1.4.3]
- tibble         [3.1.6 -> 3.0.6]
- tidyselect     [1.1.1 -> 1.1.0]
- utf8           [1.2.2 -> 1.1.4]
- viridisLite    [0.4.0 -> 0.3.0]
- withr          [2.4.3 -> 2.4.1]
- xfun           [0.29 -> 0.20]
- yaml           [2.2.1 -> 2.2.0]
- BH             [* -> 1.69.0-1]
- MatrixModels   [* -> 0.4-1]
- SparseM        [* -> 1.77]
- abind          [* -> 1.4-5]
- askpass        [* -> 1.1]
- assertthat     [* -> 0.2.1]
- base64enc      [* -> 0.1-3]
- cachem         [* -> 1.0.4]
- car            [* -> 3.0-5]
- carData        [* -> 3.0-2]
- cellranger     [* -> 1.1.0]
- clipr          [* -> 0.7.0]
- crul           [* -> 0.7.4]
- data.table     [* -> 1.14.0]
- ez             [* -> 4.4-0]
- fastmap        [* -> 1.1.0]
- forcats        [* -> 0.5.1]
- fs             [* -> 1.3.1]
- haven          [* -> 2.2.0]
- hms            [* -> 0.5.2]
- htmltools      [* -> 0.4.0]
- httpcode       [* -> 0.2.0]
- httr           [* -> 1.4.2]
- jsonlite       [* -> 1.6]
- magick         [* -> 2.0]
- maptools       [* -> 0.9-8]
- markdown       [* -> 1.1]
- memoise        [* -> 2.0.0]
- mime           [* -> 0.9]
- openssl        [* -> 1.4.3]
- openxlsx       [* -> 4.1.0]
- optimx         [* -> 2020-4.2]
- osfr           [* -> 0.2.8]
- pdftools       [* -> 3.0.1]
- plyr           [* -> 1.8.4]
- praise         [* -> 1.0.0]
- prettyunits    [* -> 1.1.1]
- progress       [* -> 1.2.2]
- qpdf           [* -> 1.1]
- quantreg       [* -> 5.52]
- readr          [* -> 1.3.1]
- readxl         [* -> 1.3.1]
- rematch        [* -> 1.0.1]
- reshape2       [* -> 1.4.3]
- rio            [* -> 0.5.16]
- rmarkdown      [* -> 2.7]
- rprojroot      [* -> 2.0.2]
- rsvg           [* -> 1.3]
- signal         [* -> 0.7-7]
- sp             [* -> 1.3-1]
- svglite        [* -> 2.0.0]
- sys            [* -> 3.4]
- systemfonts    [* -> 1.0.1]
- testthat       [* -> 2.1.1]
- tinytex        [* -> 0.17]
- triebeard      [* -> 0.3.0]
- urltools       [* -> 1.7.3]
- zip            [* -> 2.0.2]

Do you want to proceed? [y/N]: y
