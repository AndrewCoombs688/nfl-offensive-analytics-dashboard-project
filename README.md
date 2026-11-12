# NFL Offensive Analytics Dashboard

## Overview

An interactive NFL offensive analytics dashboard developed using R and Shiny. This project uses NFL play-by-play data to analyze team offensive performance, evaluate play-calling tendencies, and explore how statistical modeling can inform offensive strategy.

The dashboard allows users to select NFL teams and weeks, examine key offensive metrics, compare passing and rushing efficiency, and predict the likelihood of a successful offensive play.

**This project is ongoing and data, metrics and analysis are being updated gradually.**

## Contributors
This project was developed collaboratively by a team of three:
- **Andy Coombs** andy.coombs@unh.edu : Statistical modeling, statistical logic, and application development
- **Panayiota Papantonakis** panayiota.papantonakis@unh.edu : Statistical modeling, statistical logic, and application development
- **Sadie Rogers** sadie.rogers@unh.edu : NFL domain knowledge and football strategy insights

## Data Source

NFL play-by-play data is retrieved using the [`nflreadr`](https://nflreadr.nflverse.com/) R package.

- **Season:** 2026
- **Observations:** Individual offensive plays
- **Key Variables:** Team, play type, EPA, down, yards to go, field position, and score differential.

## Running the Application

1. Clone this repository.
2. Open the project in RStudio.
3. Install the required packages:

```r
install.packages(c("shiny", "nflreadr", "dplyr", "ggplot2", "tidyr"))
```

4. Press the 'Run App' button
