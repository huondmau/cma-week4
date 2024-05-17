library("readr")
library("dplyr")
library("sf")
library("ggplot2")

wildschwein <- read_delim("Datasets//wildschwein_BE_2056.csv", ",")

# Careful! What Timezone is assumed?
sabi <- wildschwein |>
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE) |>
  filter(TierName == "Sabi", DatetimeUTC >= "2015-07-01", DatetimeUTC < "2015-07-03")

sabi


# map sabi's acticity
ggplot(sabi, aes(E, N, color = DatetimeUTC)) +
  geom_point() +
  geom_path() +
  coord_fixed()


# Step a): Specify a temporal window 
distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

# Step b): Measure the distance from every point to every other point within this temporal windows
sabi <- sabi |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  # distance to pos -30 minutes
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  # distance to pos -15 minutes
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)), # distance to pos +15 mintues
    nPlus2  = distance_by_element(geometry, lead(geometry, 2))  # distance to pos +30 minutes
  )

# Now we want to calculate the mean distance of nMinus2, nMinus1, nPlus1, nPlus2 for each row
sabi <- sabi |>
  rowwise() |>
  mutate(stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2))) |>
  ungroup()

sabi

# Step c): Remove “static points”

sabi <- sabi |>
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

sabi_filter <- sabi |>
  filter(!static)

sabi_filter |>
  ggplot(aes(E, N)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")
