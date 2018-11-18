Examining France at WC 2018
================

-   [Data Import and Tidying](#data-import-and-tidying)
    -   [Get Events Data](#get-events-data)
    -   [Add Minutes Played](#add-minutes-played)
    -   [Tidy Location Data](#tidy-location-data)
    -   [Variable Calculation](#variable-calculation)
-   [Investigation](#investigation)
    -   [Team: Passes per match](#team-passes-per-match)
    -   [France: Shot Map](#france-shot-map)
-   [Source](#source)

Nick D. Ungson

**Last updated:** 2018-11-18 15:11:25

Below, I use some procedures and concepts described by FC rSTATS during our meeting on 2018-11-08 (and on their website) to examine France's performance at the *2018 FIFA World Cup*.

Data Import and Tidying
=======================

Get Events Data
---------------

Below, I load the necessary packages for analyses. To facilitate processing, I commented out code that uses StatsBombFreeEvents() to get data from SB and instead import `"events.RDS"`, which was saved from SB on 2018-11-18.

``` r
#devtools::install_github("statsbomb/StatsBombR")
require(StatsBombR)
require(tidyverse)
require(FC.rSTATS)

# load event data
#events <- StatsBombFreeEvents()
# save events as .RDS
#saveRDS(events, "events.RDS")

events <- readRDS("events.RDS")

# select only World Cup 2018 data
events <- events %>% filter(competition_id == 43)
```

Add Minutes Played
------------------

Below, I use the `get.minutesplayed(events)` function to get all timing data per player, calculate a total minutes player variable, which is then appended onto the larger `events` dataframe using `merge()`.

``` r
# get timing data per player
time.data <- get.minutesplayed(events)

# sumarise as total minutes played 
min.played.per.player <- time.data %>% 
  group_by(player.name) %>% 
  summarize(tot.min.played = sum(minutes.played))

# merge: add total minutes to events dataframe 
events <- merge(events, min.played.per.player, by = "player.name")

rm(time.data, min.played.per.player)
```

Tidy Location Data
------------------

Since StatsBomb data `$location` (i.e., pass orgin, shot location) consists of a list of c(., x, y), the following code separates that into a separate `x` and `y` variables.

``` r
events <- events %>% 
  separate(location, c("extra.location","x","y")) %>% 
  select(-extra.location) %>% 
  mutate(x = as.numeric(as.character(x)), 
         y = as.numeric(as.character(y)))

# do same for pass end location
events <- events %>% 
  separate(pass.end_location, c("extra.location","pass.end.x","pass.end.y")) %>% 
  select(-extra.location) %>% 
  mutate(pass.end.x = as.numeric(as.character(pass.end.x)), 
         pass.end.y = as.numeric(as.character(pass.end.y)))
```

Variable Calculation
--------------------

``` r
events <- events %>% 
  mutate(type.name = ifelse(type.name == "Pass" & 
                              pass.length >= quantile(events$pass.length, probs = c(0.95), na.rm = T), 
                            "pass_long", type.name)) %>% 
  mutate(type.name = ifelse(type.name == "Pass" & 
                              pass.end.x >= 102 & pass.end.y > 18 & pass.end.y < 62, 
                            "pass_into_box", type.name)) %>% 
  mutate(type.name = ifelse(type.name == "Shot" & x >= 102 & y > 18 & y < 62, 
                            "shot_in_box", type.name)) %>% 
  mutate(type.name = ifelse(type.name == "Shot" & x < 102 & (y < 18 | y > 62), 
                            "shot_out_box", type.name)) %>%
  mutate(type.name = ifelse(type.name == "Pressure" & x >= 80, 
                            "pressure_high", type.name))
```

Investigation
=============

Team: Passes per match
----------------------

Display all teams at the World Cup and sort by number of passes per game.

``` r
events %>% 
  filter(type.name == "Pass") %>% 
  group_by(team.name) %>% 
  summarize(matches = length(unique(match_id)), 
            pass_per_match = round(n() / matches, 0)) %>%
  arrange(desc(pass_per_match)) %>% 
  print(n = 32)
```

    ## # A tibble: 32 x 3
    ##    team.name    matches pass_per_match
    ##    <chr>          <int>          <dbl>
    ##  1 Spain              4            812
    ##  2 Germany            3            606
    ##  3 Argentina          4            562
    ##  4 Saudi Arabia       3            555
    ##  5 Brazil             5            524
    ##  6 England            7            514
    ##  7 Croatia            7            513
    ##  8 Switzerland        4            498
    ##  9 Belgium            7            487
    ## 10 Japan              4            485
    ## 11 Australia          3            468
    ## 12 Poland             3            456
    ## 13 Portugal           4            452
    ## 14 Colombia           4            447
    ## 15 Tunisia            3            444
    ## 16 France             7            416
    ## 17 Peru               3            415
    ## 18 Uruguay            5            406
    ## 19 Egypt              3            392
    ## 20 Mexico             4            388
    ## 21 Denmark            4            383
    ## 22 Nigeria            3            372
    ## 23 Morocco            3            371
    ## 24 Serbia             3            345
    ## 25 Costa Rica         3            330
    ## 26 Russia             5            330
    ## 27 Panama             3            304
    ## 28 Senegal            3            303
    ## 29 South Korea        3            284
    ## 30 Sweden             5            280
    ## 31 Iceland            3            239
    ## 32 Iran               3            204

France: Shot Map
----------------

First, subset data to only include shots by France.

``` r
fr.data <- events %>% 
  filter(team.name == "France" & 
           (type.name == "Shot" | 
              type.name == "shot_in_box" | 
              type.name == "shot_out_box") & 
           shot.type.name != "Penalty")

# add goal count to help plotting
fr.data$goal <- ifelse(fr.data$shot.outcome.name == "Goal", "1", "0")
```

Now add to shot map (created using `create_StatsBomb_ShotMap()`, a function modified from [FC r STATS](https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/3.CreateShotMaps.md).

``` r
# read function
source("functions/create_StatsBomb_ShotMap.R")

fr.plot <- create_StatsBomb_ShotMap("#ffffff", "#A9A9A9", "#ffffff", "#000000") 

fr.plot + geom_point(data = fr.data, aes(x = y, 
                                y = x, 
                                size = shot.statsbomb_xg, 
                                color = goal)) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = c("#F42A41", "#00209F")) + 
  geom_text(aes(x = 2, y = 70, label = fr.data$team.name[1]), 
            hjust = 0, vjust = 0.5, size = 5, colour = "#00209F") +
  geom_text(aes(x = 2, y = 66, 
                label = paste0("Non-Penalty Expected Goals (xG): ", 
                               round(sum(fr.data$shot.statsbomb_xg), 2))), 
            hjust = 0, vjust = 0.5, size = 3) + 
  geom_text(aes(x = 2, y = 64, 
                label = paste0("Actual Goals (Non-Penalty): ", 
                               round(sum(as.numeric(fr.data$goal)), 0))), 
            hjust = 0, vjust = 0.5, size = 3) + 
  geom_text(aes(x = 2, y = 62, 
                label = paste0("xG Difference: ", 
                               round(sum(as.numeric(fr.data$goal)), 0) - 
                                 round(sum(fr.data$shot.statsbomb_xg), 2))), 
            hjust = 0, vjust = 0.5, size = 3)
```

![](WC-France_files/figure-markdown_github/unnamed-chunk-7-1.png)

Source
======

All data above from StatsBomb.

![](statsbomb-logo.jpg)
