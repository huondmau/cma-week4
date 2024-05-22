# Exercise 4

# a) Specify a temporal windows for in which to measure Euclidean distances.
# b) Measure the distance from every point to every other point within this temporal window
# c) Remove “static points”: These are points where the average distance is less than a given threshold. This segments the trajectory into subtrajectories.
# d) Now remove short subtrajectories: These are trajectories with a short duration (whereas “short” is tbd).

library("readr")
library("dplyr")
library("sf")
library("ggplot2")

wildschwein <- read_delim("Datasets/wildschwein_BE_2056.csv", ",")
messenger <- read_delim("Datasets/example_route.csv")


# Careful! What Timezone is assumed?

messenger <- messenger |>
  st_as_sf(coords = c("x", "y"), crs = 2056, remove = FALSE)

# plot the messenger route

ggplot(messenger, aes(x, y)) +
  geom_sf()

# Step a): Specify a temporal window 
distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

messenger <- messenger |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  # distance to pos -30 minutes
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  # distance to pos -15 minutes
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)), # distance to pos +15 mintues
    nPlus2  = distance_by_element(geometry, lead(geometry, 2))  # distance to pos +30 minutes
  )

messenger <- messenger |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2))
  ) |>
  ungroup()

# look at the values and distribution of stepMean
boxplot(messenger$stepMean)

messenger <- messenger |>
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

messenger_filter <- messenger |>
  filter(!static)


messenger_filter |>
  ggplot(aes(x, y)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

#Task 4: Segment-based analysis

rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}

your_data_frame <- your_data_frame |>
  mutate(segment_id = rle_id(static))

# Task 5: Similarity measures

# Task 6: Calculate similarity

