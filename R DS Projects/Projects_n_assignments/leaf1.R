---
title: "MyLeaflet"
author: "Gracy"
date: "24 December 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r leaf}
library(leaflet)
my_map <- leaflet() %>%
addTiles()
my_map
```