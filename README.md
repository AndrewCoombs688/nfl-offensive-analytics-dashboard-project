# NFL Offensive Analytics Dashboard

## Overview

An interactive NFL offensive analytics dashboard developed using R and Shiny. This project uses NFL play-by-play data to analyze team offensive performance, evaluate play-calling tendencies, and explore how statistical modeling can inform offensive strategy.

The dashboard allows users to select NFL teams and weeks, examine key offensive metrics, compare passing and rushing efficiency, and predict the likelihood of a successful offensive play.

**This dashboard project is ongoing and data, metrics and analysis are being updated incrementally.**

## Features

- **Offensive Performance:** Analyze passing and rushing efficiency using Expected Points Added (EPA).
- **Red Zone Analysis:** Evaluate offensive performance inside the opponent's 20-yard line.
- **Play Success Rate:** Examine the percentage of plays generating positive EPA.
- **Situational Analysis:** Explore third-down efficiency, third-and-long situations, and fourth-down conversions.
- **Player Performance:** Identify top-performing offensive players based on average EPA.
- **Predictive Modeling:** Apply team-specific logistic regression models to estimate the probability of a successful offensive play.
- **Interactive Visualizations:** Explore team performance through an interactive Shiny dashboard.

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
install.packages(c("shiny", "nflreadr"))
```

4. Run the application:

```r
shiny::runApp()
```
