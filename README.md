
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GFuel Ratings

<!-- badges: start -->

<!-- badges: end -->

Below is the copied text from the info tab of the dashboard. This
dashboard was also submitted to the [Shiny
Contest 2020](https://blog.rstudio.com/2020/02/12/shiny-contest-2020-is-here/).
Link to the submission can be found
[here](https://community.rstudio.com/t/gfuel-ratings-2020-shiny-contest-submission/56521).

# GFuel Ratings

## What is this?

My girlfriend Lexi and I decided it would be fun to try the powdered
energy brand “GFuel” after seeing many of our favorite internet
personalities endorsing it and partnering with the company. We initially
bought a full container of sour grape and a variety sample pack. I
thought it would be fun to track our ratings per flavor. As someone who
works with data and creates shiny dashboards at work, what better idea
to do with this data than to create a pretty slick dashboard? :) This is
a fun little project Lexi and I have been working on\! We will be
updating this as we try different flavors over time. Hopefully our
ratings and findings will bring joy to those who come across this.

## What is GFuel?

It’s gamer fuel, zero-sugar fuel for gamers\! GFuel’s website can
explain this better than myself\! Check out this page on [their
website](https://gfuel.com/pages/energy-for-everyone) for their
information and FAQ.

## How was this built?

[R](https://www.r-project.org/about.html) and
[Shiny](https://shiny.rstudio.com/). Source code can be found
[here](https://github.com/KoderKow/gfuel_dash)\!

## What is the data source?

We use [Google Sheets](https://www.google.com/sheets/about/) as a data
source and the R package
[googlesheets4](https://github.com/tidyverse/googlesheets4) to read in
the data. Raw data can be found
[here](https://docs.google.com/spreadsheets/d/11JC2nnhlY6pg6zDEuzSMJ8uVDfezTswmXGeQxFPoJOk/edit?usp=sharing).

## Additional Info

  - The paired Wilcox test is used for the “Same Taste?” statistic
  - The twitter image URLs are harvested each time the dashboard loads
    to avoid the issue of URLs changing with avatar updates
  - All flavor types are hotlinks (minus the plots) to the flavor on
    GFuel’s website
  - This dashboard needs no updates, aside from UI additions, only the
    data source needs to be updated. The data, profiles, plots, and
    tables are made directly from the google sheet

## Learning Points

  - Packages:
    [googlesheets4](https://github.com/tidyverse/googlesheets4) and
    [shinydashboardplus](https://github.com/RinteRface/shinydashboardPlus)
  - Setting up a google sheet as a data source for a Shiny app
  - [RStudio Cloud](https://rstudio.cloud/)

*Note: I am not affiliated or endorsed by GFuel or Gamma Labs*
