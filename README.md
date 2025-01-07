# RsNLME Model Executor <img src='vignettes/img/ModelExecutor.png' align="right" style = "float:right; height: 150px;" alt="ModelExecutor package logo."/>
## Overview

`Certara.RsNLME.ModelExecutor` is an R package and Shiny application used to execute an RsNLME model.

Use the GUI to add additional output tables, specify engine parameters, select various run types, and more!

<img src='vignettes/img/fitmodel.gif' alt="Gif showing how to execute a simple model in the Shiny GUI."/>

## Installation

### Windows

```r
install.packages("Certara.RsNLME.ModelExecutor",
                 repos = c("https://certara.jfrog.io/artifactory/certara-cran-release-public/",
                           "https://cloud.r-project.org"),
                 method = "libcurl")

```

### Linux

```r
install.packages("Certara.RsNLME.ModelExecutor",
                 repos = c("https://certara.jfrog.io/artifactory/certara-cran-release-public/",
                           "https://cloud.r-project.org"))

```

## Usage

Use `Certara.RsNLME.ModelExecutor` to launch the model execution interface with a model object.

```r
library(Certara.RsNLME.ModelExecutor)
library(Certara.RsNLME)

model <- pkmodel(numCompartments=2,
                 data = pkData,
                 ID = "Subject",
                 Time = "Act_Time",
                 A1 = "Amount",
                 CObs = "Conc")

modelExecutorUI(model = model)

```
