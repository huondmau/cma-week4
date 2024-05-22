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

messenger <- messenger |>
  mutate(segment_id = rle_id(static))

# Task 5: Similarity measures

pedestrian <- read_delim("Datasets/pedestrian.csv")

# convert to sf
pedestrian <- pedestrian |>
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE)

ggplot(pedestrian, aes(E, N)) +
  geom_sf() +
  facet_wrap(~TrajID)

# Task 6: Calculate similarity

install.packages("SimilarityMeasures")
library(SimilarityMeasures)

help(package = "SimilarityMeasures")

# Extract coordinates for each trajectory
trajectory1 <- pedestrian %>% filter(TrajID == 1) %>% st_coordinates()
trajectory2 <- pedestrian %>% filter(TrajID == 2) %>% st_coordinates()
trajectory3 <- pedestrian %>% filter(TrajID == 3) %>% st_coordinates()
trajectory4 <- pedestrian %>% filter(TrajID == 4) %>% st_coordinates()
trajectory5 <- pedestrian %>% filter(TrajID == 5) %>% st_coordinates()
trajectory6 <- pedestrian %>% filter(TrajID == 6) %>% st_coordinates()

# Initialize an empty data frame
results <- data.frame(
  Trajectory1 = integer(),
  Trajectory2 = integer(),
  DTW = numeric(),
  EditDist = numeric(),
  Frechet = numeric(),
  LCSS = numeric(),
  stringsAsFactors = FALSE
)

# Define a function to perform comparisons and store results
compare_trajectories <- function(traj1, traj2, id1, id2) {
  dtw_result <- DTW(traj1, traj2)
  editdist_result <- EditDist(traj1, traj2)
  frechet_result <- Frechet(traj1, traj2)
  lcss_result <- LCSS(traj1, traj2)
  
  return(data.frame(
    Trajectory1 = id1,
    Trajectory2 = id2,
    DTW = dtw_result,
    EditDist = editdist_result,
    Frechet = frechet_result,
    LCSS = lcss_result,
    stringsAsFactors = FALSE
  ))
}

# Compare trajectory 1 with trajectories 2-6
results <- rbind(results, compare_trajectories(trajectory1, trajectory2, 1, 2))
results <- rbind(results, compare_trajectories(trajectory1, trajectory3, 1, 3))
results <- rbind(results, compare_trajectories(trajectory1, trajectory4, 1, 4))
results <- rbind(results, compare_trajectories(trajectory1, trajectory5, 1, 5))
results <- rbind(results, compare_trajectories(trajectory1, trajectory6, 1, 6))

results
