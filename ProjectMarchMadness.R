# https://www.kaggle.com/datasets/andrewsundberg/college-basketball-dataset/
# Data from the 2013-2023 Division I college basketball seasons.
# cbb.csv has seasons 2013-2019 and seasons 2021-2023 combined.
# cbb20.csv is kept separate from the other seasons, because there was no postseason due to the Coronavirus.
# csv files were downloaded Sunday November 12, 2023

library(tidyverse)
library(caret)

getwd() ##### run this command to get working directory ###
cbb <- file.path(getwd(), "Projects/R/Project/MarchMadness/cbb.csv")
#cbb20 <- file.path(getwd(), "Projects/R/Project/MarchMadness/cbb20.csv")

###########
# read csv
cbb_csv <- read_csv(cbb)
rm(cbb)

#################
# pre-processing
postseason_levels <- c("R68", "R64", "R32", "S16", "E8", "F4", "2ND", "Champions")
dat <- cbb_csv |>
  mutate(SEED = as.integer(SEED), # coercion warning
         POSTSEASON = factor(POSTSEASON, levels = postseason_levels)) |>
  filter(!is.na(SEED))
levels(dat$POSTSEASON) <- c("R68", "R64", "R32", "S16", "E8", "F4", "2ND", "1ST")
names(dat) <- make.names(names(dat)) # fix invalid column names
edx <- dat |> filter(YEAR < 2023)
final_holdout_set <- dat |> filter(YEAR == 2023)
rm(postseason_levels, dat, cbb_csv)

##########################
# validate pre-processing
edx |>
  group_by(YEAR) |>
  summarize(n = n())
final_holdout_set |>
  group_by(YEAR) |>
  summarize(n = n())

# Trends
ylabels <- c("R68", "R64", "R32", "S16", "E8", "F4", "2ND", "1ST")
others <- c("R68", "R64", "R32", "S16")
blevels <- c("< E8", "E8", "F4", "2ND", "1ST")

# ADJOE trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(ADJOE, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Adjusted Offensive Efficiency") + ylab("Postseason Levels") +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 108, linetype = 2, color = "darkgray") +
  coord_cartesian(xlim = c(100, 130), ylim = c(1, 8))

# ADJOE boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(ADJOE, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Adjusted Offensive Efficiency") + ylab("Postseason Levels")

# ADJDE trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(ADJDE, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Adjusted Defensive Efficiency") + ylab("Postseason Levels") +
  scale_x_reverse() +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 104, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# ADJDE boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(ADJDE, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Adjusted Defensive Efficiency") + ylab("Postseason Levels") +
  scale_x_reverse()
  
# BARTHAG trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(BARTHAG, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Power Rating") + ylab("Postseason Levels") +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 0.66, linetype = 2, color = "darkgray") +
  coord_cartesian(xlim = c(0.5, 1.0), ylim = c(1, 8))

# BARTHAG boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(BARTHAG, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Power Rating") + ylab("Postseason Levels")
  
# EFG_O trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(EFG_O, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Effective Field Goal Percentage Shot") + ylab("Postseason Levels") +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 47, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# EFG_O boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(EFG_O, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Effective Field Goal Percentage Shot") + ylab("Postseason Levels")
  
# EFG_D trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(EFG_D, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Effective Field Goal Percentage Allowed") + ylab("Postseason Levels") +
  scale_x_reverse() +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 53, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# EFG_D boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(EFG_D, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Effective Field Goal Percentage Allowed") + ylab("Postseason Levels") +
  scale_x_reverse()
  
# TOR trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(TOR, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Turnover Percentage Allowed (Turnover Rate)") + ylab("Postseason Levels") +
  scale_x_reverse() +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 20.4, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# TOR boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(TOR, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Turnover Percentage Allowed (Turnover Rate)") + ylab("Postseason Levels") +
  scale_x_reverse()
  
# TORD trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(TORD, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Turnover Percentage Committed (Steal Rate)") + ylab("Postseason Levels") +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 13.4, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# TORD boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(TORD, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Turnover Percentage Committed (Steal Rate)") + ylab("Postseason Levels")
  
# ORB trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(ORB, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Offensive Rebound Rate") + ylab("Postseason Levels") +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 25.5, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# ORB boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(ORB, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Offensive Rebound Rate") + ylab("Postseason Levels")
  
# DRB trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(DRB, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Offensive Rebound Rate Allowed") + ylab("Postseason Levels") +
  scale_x_reverse() +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 35.7, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# DRB boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(DRB, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Offensive Rebound Rate Allowed") + ylab("Postseason Levels") +
  scale_x_reverse()

# FTR trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(FTR, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Free Throw Rate") + ylab("Postseason Levels") +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 26, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# FTR boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(FTR, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Free Throw Rate") + ylab("Postseason Levels")

# FTRD trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(FTRD, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Free Throw Rate Allowed") + ylab("Postseason Levels") +
  scale_x_reverse() +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 45, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# FTRD boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(FTRD, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Free Throw Rate Allowed") + ylab("Postseason Levels") +
  scale_x_reverse()

# X2P_O trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(X2P_O, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Two-Point Shooting Percentage") + ylab("Postseason Levels") +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 45.5, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# X2P_O boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(X2P_O, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Two-Point Shooting Percentage") + ylab("Postseason Levels")
  
# X2P_D trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(X2P_D, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Two-Point Shooting Percentage Allowed") + ylab("Postseason Levels") +
  scale_x_reverse() +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 52, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# X2P_D boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(X2P_D, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Two-Point Shooting Percentage Allowed") + ylab("Postseason Levels") +
  scale_x_reverse()

# X3P_O trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(X3P_O, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Three-Point Shooting Percentage") + ylab("Postseason Levels") +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 29.5, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# X3P_O boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(X3P_O, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Three-Point Shooting Percentage") + ylab("Postseason Levels")
  
# X3P_D trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(X3P_D, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Three-Point Shooting Percentage Allowed") + ylab("Postseason Levels") +
  scale_x_reverse() +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 37.2, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# X3P_D boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(X3P_D, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Three-Point Shooting Percentage Allowed") + ylab("Postseason Levels") +
  scale_x_reverse()
  
# ADJ_T trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(ADJ_T, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Adjusted Tempo") + ylab("Postseason Levels") +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 59, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# ADJ_T boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(ADJ_T, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Adjusted Tempo") + ylab("Postseason Levels")
  
# WAB trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(WAB, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("Wins Above Bubble") + ylab("Postseason Levels") +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = -2, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# WAB boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(WAB, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("Wins Above Bubble") + ylab("Postseason Levels")
  
# SEED trend
edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  ggplot(aes(SEED, POSTSEASON)) + 
  geom_smooth() +
  geom_point(aes(color = cut(POSTSEASON, c(0, 4, 5, 6, 8, Inf))), 
             alpha = 0.5, show.legend = FALSE, size = 3) +
  scale_color_manual(values = c("slategray", "red", "orange", "darkgreen")) +
  facet_wrap(~ YEAR) +
  xlab("March Madness Seed") + ylab("Postseason Levels") +
  scale_x_reverse() +
  scale_y_continuous(breaks = 1:8, labels = ylabels) +
  geom_vline(xintercept = 11.5, linetype = 2, color = "darkgray") +
  coord_cartesian(ylim = c(1, 8))

# SEED boxplot
edx |>
  mutate(OTHER = POSTSEASON %in% others) |>
  mutate(POSTSEASON = ifelse(OTHER, "< E8", as.character(POSTSEASON))) |>
  mutate(POSTSEASON = factor(POSTSEASON, levels = blevels)) |>
  ggplot(aes(SEED, POSTSEASON, fill = POSTSEASON)) + 
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = c("slategray", "red", "orange", "darkgreen", "forestgreen")) +
  xlab("March Madness Seed") + ylab("Postseason Levels") +
  scale_x_reverse()

rm(ylabels, others, blevels)

################################
# build algorithm with finalists

# training/test
train_set <- edx |>
  mutate(POSTSEASON = as.numeric(POSTSEASON))
test_set <- final_holdout_set |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  filter(ADJOE > 108 & ADJDE < 104) # ignore this line to run without filter

# train with 3 predictors
fit_glm <- train(POSTSEASON ~ BARTHAG + WAB + SEED, method = "glm", data = train_set)
fit_gam <- train(POSTSEASON ~ BARTHAG + WAB + SEED, method = "gamLoess", data = train_set)
fit_knn <- train(POSTSEASON ~ BARTHAG + WAB + SEED, method = "knn", data = train_set)
fit_rf <- train(POSTSEASON ~ BARTHAG + WAB + SEED, method = "rf", data = train_set, tuneGrid = data.frame(mtry = 2))

# predict
pred_glm <- predict(fit_glm, test_set)
pred_gam <- predict(fit_gam, test_set)
pred_knn <- predict(fit_knn, test_set)
pred_rf <- predict(fit_rf, test_set)

rm(fit_glm, fit_gam, fit_knn, fit_rf)

# test
test_set <- test_set |>
  mutate(glm = pred_glm,
         gam = pred_gam,
         knn = pred_knn,
         rf = pred_rf,
         ensemble = (glm + gam + knn + rf) / 4)
elite_glm <- test_set |> arrange(desc(glm)) |> head(8)
elite_gam <- test_set |> arrange(desc(gam)) |> head(8)
elite_knn <- test_set |> arrange(desc(knn)) |> head(8)
elite_rf <- test_set |> arrange(desc(rf)) |> head(8)
elite_ensemble <- test_set |> arrange(desc(ensemble)) |> head(8)

rm(pred_glm, pred_gam, pred_knn, pred_rf)

# score function
score_for_elite <- function(elite_pred) {
  first_round <- elite_pred |> filter(POSTSEASON >= 3) |> nrow()
  second_round <- elite_pred |> filter(POSTSEASON >= 4) |> nrow()
  sweet_sixteen <- elite_pred |> filter(POSTSEASON >= 5) |> nrow()
  elite_eight <- elite_pred |> head(4) |> filter(POSTSEASON >= 6) |> nrow()
  final_four <- elite_pred |> head(2) |> filter(POSTSEASON >= 7) |> nrow()
  champion <- elite_pred[1,] |> filter(POSTSEASON == 8) |> nrow()
  first_round + second_round * 2 + sweet_sixteen * 4 + elite_eight * 8 + final_four * 16 + champion * 32
}

# scores
score_for_elite(elite_glm) # with filter - 18, without filter - 18
score_for_elite(elite_gam) # 32, 32
score_for_elite(elite_knn) # 11, 11
score_for_elite(elite_rf) # ~31, ~31
score_for_elite(elite_ensemble) # 24, 24

rm(elite_glm, elite_gam, elite_knn, elite_rf, elite_ensemble)

# how many teams were filtered out?
final_holdout_set |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  filter(ADJOE > 108 & ADJDE < 104) |> # filter
  nrow() # 50
final_holdout_set |>
  mutate(POSTSEASON = as.numeric(POSTSEASON)) |>
  nrow() # 68

rm(train_set, test_set)

#############
# algorithm 3

# index for training/test sets
test_index_for_year <- function(year) {
  edx |> 
    mutate(index = as.numeric(rownames(edx))) |>
    filter(YEAR == year) |>
    select(index) |>
    pull(index)
}

# prediction helper function
pred_for_elite_eight <- function(train_set, test_set, method, test_index) {
  train_set <- train_set |> 
    select(-TEAM, -CONF, -G, -W, -YEAR) |>
    mutate(POSTSEASON = as.numeric(POSTSEASON))

  fit <- train(POSTSEASON ~ ., method = method, data = train_set)
  y_hat <- predict(fit, test_set) 
  edx[test_index,] |>
    mutate(POSTSEASON = as.numeric(POSTSEASON),
           y = y_hat) |>
    arrange(desc(y)) |>
    select(TEAM, POSTSEASON, y) |>
    head(8)
}

# cross validation - compute all scores by year (took ~14 min)
scores <- sapply(unique(edx$YEAR), function(year) {
  test_index <- test_index_for_year(year)
  test_set <- edx[test_index,]
  train_set <- edx[-test_index,]

  # glm 
  elite_glm <- pred_for_elite_eight(train_set, test_set, "glm", test_index)
  score_glm <- score_for_elite(elite_glm)
  # gam 
  elite_gam <- pred_for_elite_eight(train_set, test_set, "gamLoess", test_index)
  score_gam <- score_for_elite(elite_gam)
  # knn 
  elite_knn <- pred_for_elite_eight(train_set, test_set, "knn", test_index)
  score_knn <- score_for_elite(elite_knn)
  # random forest 
  elite_rf <- pred_for_elite_eight(train_set, test_set, "rf", test_index)
  score_rf <- score_for_elite(elite_rf)
  
  c(year, score_glm, score_gam, score_knn, score_rf)
})
scores
#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
# [1,] 2016 2015 2018 2019 2017 2014 2013 2021 2022
# [2,]  100   84   91   72   44   53  106   81   44
# [3,]  100  118   97   68   50   45  110   65   92
# [4,]  101   72   51   68   49   44   59   41   61
# [5,]   68  116   83   56   56   41   50   65   55
tscores <- as_tibble(t(scores), .name_repair = NULL)
names(tscores) <- c("season", "glm", "gam", "knn", "rf")
tscores <- tscores |> arrange(season)
tscore_c <- as.integer(round(colMeans(tscores)))
tscores |> 
  mutate(season = as.character(season)) |>
  add_row(season = "avg", glm = tscore_c[2], gam = tscore_c[3], knn = tscore_c[4], rf = tscore_c[5]) |> 
  mutate(avg = (glm + gam + knn + rf) / 4)
rm(scores, tscores, tscore_c)

###################################
# train=edx, test=final_holdout_set
train_set <- edx |>
  select(-TEAM, -CONF, -G, -W, -YEAR) |>
  mutate(POSTSEASON = as.numeric(POSTSEASON))
test_set <- final_holdout_set |>
  mutate(POSTSEASON = as.numeric(POSTSEASON))

# train with all 18 predictors (took ~ 2min)
fit_glm <- train(POSTSEASON ~ ., method = "glm", data = train_set)
fit_gam <- train(POSTSEASON ~ ., method = "gamLoess", data = train_set)
fit_knn <- train(POSTSEASON ~ ., method = "knn", data = train_set)
fit_rf <- train(POSTSEASON ~ ., method = "rf", data = train_set)

# predict
pred_glm <- predict(fit_glm, test_set)
pred_gam <- predict(fit_gam, test_set)
pred_knn <- predict(fit_knn, test_set)
pred_rf <- predict(fit_rf, test_set)

rm(fit_glm, fit_gam, fit_knn, fit_rf)

# test
test_set <- test_set |>
  mutate(glm = pred_glm,
         gam = pred_gam,
         knn = pred_knn,
         rf = pred_rf,
         ensemble = (glm + gam + knn + rf) / 4)
elite_glm <- test_set |> arrange(desc(glm)) |> head(8)
elite_gam <- test_set |> arrange(desc(gam)) |> head(8)
elite_knn <- test_set |> arrange(desc(knn)) |> head(8)
elite_rf <- test_set |> arrange(desc(rf)) |> head(8)
elite_ensemble <- test_set |> arrange(desc(ensemble)) |> head(8)

rm(pred_glm, pred_gam, pred_knn, pred_rf)

# scores
score_for_elite(elite_glm) # 87
score_for_elite(elite_gam) # 82
score_for_elite(elite_knn) # 55
score_for_elite(elite_rf)  # ~32
score_for_elite(elite_ensemble) # 80 

rm(elite_glm, elite_gam, elite_knn, elite_rf, elite_ensemble)

# x scaled
# x_scaled <- edx |> 
#   select(-TEAM, -CONF, -G, -W, -POSTSEASON, -YEAR) |>
#   data.matrix()
# x_mean <- colMeans(x_scaled)
# x_sd <- colSds(x_scaled)
# x_scaled <- sweep(x_scaled, 2, x_mean)
# x_scaled <- sweep(x_scaled, 2, x_sd, FUN="/")
# pca <- prcomp(x_scaled)
# summary(pca)

# y
# y <- as.numeric(edx$POSTSEASON)