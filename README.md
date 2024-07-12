# Project 2

### 1. Brief description of the app and its purpose.

`FDAex` is a RShuny app created to generate numerical and graphical insights on occurrence of adverse events in Humans and Animals. The source API is `openFDA`, provided by the Food and Drug Administration (FDA).It also has functionality to display and download source data by querying the `openFDA API` directly. It contains an "About" Tab which states the purpose and main features. The earliest record is from started from Jan 1 2004.

### 2. A list of packages needed to run the app.

```         
library(shiny)
library(shinythemes)
library(httr)
library(jsonlite)
library(glue)
library(tidyverse)
library(ggplot2)
library(see)
library(easystats)
```

### 3. A line of code that would install all the packages used.

```         
install.packages(c(
"shiny",
"httr",
"jsonlite",
"glue",
"tidyverse",
"ggplot2",
"see",
"easystats",
"shinythemes"
)
)
```

### 4. The shiny::runGitHub() code

```         
 
library(shiny) 
shiny::runGitHub(repo = "project2", username = "smitmiyani23", subdir = "FDAex")
```