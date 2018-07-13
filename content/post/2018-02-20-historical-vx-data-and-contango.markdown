---
title: Historical /VX data and Contango
author: Jason Taylor
date: '2018-02-27'
slug: historical-vx-data-and-contango
categories:
  - Data
  - Volatility
tags:
  - Contango
  - /VX
  - Volatility
---

The [CBOE](http://cfe.cboe.com/market-data/historical-data) site makes historical data available for personal use and this post describes a method 
for gathering and processing this data for analysis. 

<!--more-->  

#### Setup global option, load libraries:


```r
knitr::opts_chunk$set(message = FALSE, tidy.opts = list(width.cutoff = 60)) 
suppressWarnings(suppressMessages(suppressPackageStartupMessages({
  library_list <- c("xml2", "rvest", "stringr", "dplyr", "tidyr", "utils", "ggplot2",
                    "purrr", "lubridate", "stats", "zoo", "knitr", "kableExtra")
  lapply(library_list, require, character.only = TRUE)})))
```
#### Gathering the data from CBOE site
VX - Cboe S&P 500 Volatility Index (VIX) Futures Price and Volume Detail:


```r
url <- "http://cfe.cboe.com/market-data/historical-data"
regex_vx <- "CFE_[A-Z]{1}[0-9]{2}_(VX){1}(\\.){1}(csv){1}" # REGEX for VX files on site
page <- xml2::read_html(url)

file_paths <- page %>%
  rvest::html_nodes("a") %>%       # find all links
  rvest::html_attr("href") %>%     # get the url
  stringr::str_subset("\\.csv")    # all csv files
  
file_paths <- as.data.frame(file_paths) %>%
  dplyr::mutate(web_url = "http://cfe.cboe.com",
                link = paste0(web_url, file_paths)) %>%
  tidyr::separate(file_paths, into = c("blank", "Publish", "ScheduledTask", "MktData", 
                                       "datahouse", "file_name"), sep = "/") %>%
  dplyr::select(file_name, link) %>%
  dplyr::filter(grepl(regex_vx, link))

#mapply(utils::download.file, url = file_paths$link, destfile = file_paths$file_name)
```

#### Data Processing

* Bind files  
* Split contract name into year, month, contract symbol  
* Calculate contango as % of front month - back month / front month settle  
* Add front month and back month as new columns for clarity and future use in analysis  


```r
vx_files = as.data.frame(list.files(pattern = regex_vx), stringsAsFactors = FALSE) %>%
  stats::setNames(., "file_name")

vx_data <- purrr::map_df(vx_files$file_name, utils::read.table, blank.lines.skip = TRUE,
                         fill = TRUE, header = FALSE, quote = '', sep = ",",
                         stringsAsFactors = FALSE) %>%
  dplyr::select(1:11) %>% 
  stats::setNames(., c("quote_date", "contract", "open", "high", "low", "close",
                       "settle", "change", "volume", "efp", "open_interest")) %>%
  dplyr::filter(!grepl("Trade", quote_date),
                !grepl("CFE", quote_date)) %>%
  dplyr::mutate(quote_date = as.Date(quote_date, format = "%m/%d/%Y"),
                contract = gsub("\\(", "", contract),
                contract = gsub("\\)", "", contract)) %>%
  tidyr::separate(contract, c("contract", "month", "year"), sep = " ") %>%
  dplyr::mutate(month = zoo::as.yearmon(month, "%b"),
                month = lubridate::month(month),
                year = paste0("20", year)) %>%
  dplyr::mutate_at(vars(4:13), funs(as.numeric)) %>%
  dplyr::group_by(quote_date) %>%
  dplyr::arrange(year, month) %>%
  dplyr::mutate(# diff each monthly contract settlement
                contango = c(NA, diff(settle)), 
                # shift forward a month
                diff_month = dplyr::lead(contango, 1), 
                # distribute the value across all months
                diff_front_back = dplyr::nth(diff_month, 1), 
                contango_perc = round(100 * (diff_front_back / dplyr::first(settle)),
                                      digits = 2),
                front_month = dplyr::nth(settle, 1),
                back_month = dplyr::nth(settle, 2)) %>%
  # Filter to the front month row since we are interested in M1/M2 contango
  dplyr::filter(row_number() == 1) %>% 
  dplyr::ungroup() %>%
  dplyr::select(-c(efp, contango))
```

#### Output recent dates and summary statistics


```r
knitr::kable(tail(vx_data[ , c(1, 15:17)]))
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> quote_date </th>
   <th style="text-align:right;"> contango_perc </th>
   <th style="text-align:right;"> front_month </th>
   <th style="text-align:right;"> back_month </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2018-02-08 </td>
   <td style="text-align:right;"> -22.95 </td>
   <td style="text-align:right;"> 28.100 </td>
   <td style="text-align:right;"> 21.650 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-02-09 </td>
   <td style="text-align:right;"> -24.84 </td>
   <td style="text-align:right;"> 27.175 </td>
   <td style="text-align:right;"> 20.425 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-02-12 </td>
   <td style="text-align:right;"> -23.23 </td>
   <td style="text-align:right;"> 25.825 </td>
   <td style="text-align:right;"> 19.825 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-02-13 </td>
   <td style="text-align:right;"> -21.41 </td>
   <td style="text-align:right;"> 25.225 </td>
   <td style="text-align:right;"> 19.825 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-02-14 </td>
   <td style="text-align:right;"> -18.27 </td>
   <td style="text-align:right;"> 21.870 </td>
   <td style="text-align:right;"> 17.875 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2018-02-15 </td>
   <td style="text-align:right;"> -1.14 </td>
   <td style="text-align:right;"> 17.525 </td>
   <td style="text-align:right;"> 17.325 </td>
  </tr>
</tbody>
</table>

```r
summary(vx_data)
```

```
##    quote_date           contract             month             year     
##  Min.   :2004-03-26   Length:3498        Min.   : 1.000   Min.   :2004  
##  1st Qu.:2007-09-17   Class :character   1st Qu.: 4.000   1st Qu.:2007  
##  Median :2011-03-07   Mode  :character   Median : 7.000   Median :2011  
##  Mean   :2011-03-07                      Mean   : 6.566   Mean   :2011  
##  3rd Qu.:2014-08-26                      3rd Qu.:10.000   3rd Qu.:2014  
##  Max.   :2018-02-15                      Max.   :12.000   Max.   :2018  
##                                                                         
##       open             high             low             close       
##  Min.   :  0.00   Min.   :  0.00   Min.   :  0.00   Min.   :  0.00  
##  1st Qu.: 14.95   1st Qu.: 15.40   1st Qu.: 14.55   1st Qu.: 14.90  
##  Median : 20.09   Median : 20.80   Median : 19.50   Median : 20.00  
##  Mean   : 44.25   Mean   : 45.18   Mean   : 43.31   Mean   : 44.13  
##  3rd Qu.: 36.05   3rd Qu.: 37.25   3rd Qu.: 34.10   3rd Qu.: 35.79  
##  Max.   :212.40   Max.   :225.80   Max.   :206.00   Max.   :215.50  
##  NA's   :1        NA's   :1        NA's   :1        NA's   :1       
##      settle           change              volume         open_interest   
##  Min.   :  8.75   Min.   :-119.9200   Min.   :     0.0   Min.   :   113  
##  1st Qu.: 15.33   1st Qu.:  -0.7000   1st Qu.:   814.8   1st Qu.: 10882  
##  Median : 20.80   Median :  -0.1500   Median :  9991.5   Median : 37320  
##  Mean   : 46.02   Mean   :  -0.1934   Mean   : 39860.8   Mean   : 70475  
##  3rd Qu.: 41.36   3rd Qu.:   0.4500   3rd Qu.: 69188.5   3rd Qu.:119694  
##  Max.   :216.20   Max.   :  33.7000   Max.   :567407.0   Max.   :372068  
##                                                                          
##    diff_month       diff_front_back    contango_perc       front_month    
##  Min.   :-191.700   Min.   :-191.700   Min.   :-100.000   Min.   :  8.75  
##  1st Qu.:   0.500   1st Qu.:   0.500   1st Qu.:   2.300   1st Qu.: 15.33  
##  Median :   1.350   Median :   1.350   Median :   6.720   Median : 20.80  
##  Mean   :   2.391   Mean   :   2.391   Mean   :   5.856   Mean   : 46.02  
##  3rd Qu.:   2.650   3rd Qu.:   2.650   3rd Qu.:  10.520   3rd Qu.: 41.36  
##  Max.   :  56.060   Max.   :  56.060   Max.   :  42.680   Max.   :216.20  
##                                                                           
##    back_month    
##  Min.   :  0.00  
##  1st Qu.: 16.38  
##  Median : 21.80  
##  Mean   : 48.41  
##  3rd Qu.: 38.09  
##  Max.   :206.70  
## 
```

#### The following details should be reviewed prior to using the data for further analysis.  
* The summary statistics reveal 1 record that is missing the daily pricing details
(open, high, low, close)  
* 125 records have an (open, high, low, or close) value equal to 0  
* 9 records between 2004-2006 have a contango % of -100%  

#### Plotting the distribution of the contango %  
* shows a normal distibution  
* including the outlier -100% data points  


```r
ggplot(data = vx_data, aes(contango_perc)) + 
  geom_histogram(breaks = seq(-100, 50, by = 2), 
                 col = "black", 
                 fill = "grey", 
                 alpha = .2) + 
  labs(title = "Histogram for Contango %") +
  labs(x = "Contango %", y = "Count") + 
  xlim(c(-100, 50)) + 
  ylim(c(0, 550))
```

<img src="/post/2018-02-20-historical-vx-data-and-contango_files/figure-html/plot-1.png" width="672" />

#### Save Results


```r
saveRDS(vx_data, file = paste0("vx_data_", Sys.Date(), ".RDS"))
```

