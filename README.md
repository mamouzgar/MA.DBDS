---
title: "README"
output: html_document
date: '2023-01-19'
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

To install the package.
```{r }
devtools::install_github('mamouzgar/MA.DBDS', dependencies = TRUE)
```

Then load the library
```{r }
library(tidyverse)
library(MA.DBDS)
```

Test the package works by running
```{r}
test = generate_example()
```

You can run it on your dataset using:
```{r}
df_dbds = MA.DBDS::performKNN_DensityBasedDownsampling(input.data = test,downsample_n = 50,features = c('x','y'))
```



