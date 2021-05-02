---
title: "Sentiment Analysis of Domestic Abuse"
author: "Lizbeth Vazquez-Jorge"
date: "`r Sys.Date()`"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
A package extending the capability of [RedditExtractoR](https://github.com/RedditExtractoR/RedditExtractoR) 
by extracting and measuring sentiment analysis on comments.

## Install

Install from GitHub with the following code:

```{r install, eval = FALSE}
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("Lizbethvj/SAoDA")
```

This package is extracting Reddit search results from Reddit's own JSON API and transforms the results into a dataframe.
## Usage

There is one function currently live for SAoDA. What the function does is that it extracts data based on domestic abuse as well as taking the most popular post and running the sentiment analysis on those comments.
The limitations is that it can only give up to 100 results because Reddit only allows 100 results from the JSON API.

## Usage instructions 

minimum input for the function is the "search" argument, which is a character string. search_reddit function allows to run one query at a time. If you have more than one searches, use a `for` loop or function from the `apply` family.


Currently, this must be done at the start of every session.

Next, the fun begins with <code>SAoDA</code>.

Its first argument takes any Reddit query.

The next argument determines how long an open stream of Reddit comments will be
collected In order to gather information.
If the user prefers to use Reddit's 
Search API, the next argument allows the user to specify the number of Reddit posts
to extract.


