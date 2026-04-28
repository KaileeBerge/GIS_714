library(sf)
library(terra)
library(geodata)
library(dplyr)
library(tidyverse)
library(spOccupancy)
library(ggplot2)
library(tmap)

#State county maps
nc <- st_read(system.file("shape/nc.shp", package="sf"))
#hydro 
MajorHydro <- st_read("Major_Hydrography_(Streams_Rivers)")

#Set CRS
nc <- st_transform(nc, 4269)
MajorHydro <- st_transform(MajorHydro, 4269)

#Occurrence points
mitch_points <- st_read("NCSU_aquatics_202511_Mitch/NCSU_aquatics_202511/ncnhp_nheo_points.shp")

#CRS
nc <- st_transform(nc, 4326)
MajorHydro <- st_transform(MajorHydro, 4326)
mitch_points <- st_transform(mitch_points, 4326)

#Mitch Data as species occurrence points
mitch_points <- st_read("NCSU_aquatics_202511_Mitch/NCSU_aquatics_202511/ncnhp_nheo_points.shp")
mitch_nheo <- st_read("NCSU_aquatics_202511_Mitch/NCSU_aquatics_202511/ncnhp_nheo.shp")

ggplot() +
  geom_sf(data = mitch_points, color = "black") + # Plot the point on to
  geom_sf(data = mitch_nheo, color = "red") + # Plot the point on to
  geom_sf(data = nc, fill = NA, color = "black")+
  labs(title = "NHP Data") +
  theme_minimal()

#Clean NC data:
mitch_points$COM_NAME <- as.factor(mitch_points$COM_NAME)
summary(mitch_points$COM_NAME) 

mitch_points <- mitch_points %>%
  separate(col = DESCR,
           into = c("CollectionDate", "Descript"),
           sep = ";",
           fill = "right")

mitch_points <- mitch_points %>%
  separate(col = CollectionDate,
           into = c("CollectionYear", "CollectionMonth","CollectionDay"),
           sep = "-",
           fill = "right")

Mitch_Madtom <- mitch_points %>%
  filter(COM_NAME == "Carolina Madtom")

Mitch_Madtom$CollectionYear <- as.numeric(Mitch_Madtom$CollectionYear)
summary(Mitch_Madtom$CollectionYear)

Mitch_Madtom_group1 <- Mitch_Madtom %>%
  filter(CollectionYear <=1985)
summary(Mitch_Madtom_group1$CollectionYear)

#get the modal year
get_mode_function <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
get_mode_function(Mitch_Madtom_group1$CollectionYear)
get_mode_function(Mitch_Madtom$CollectionYear)



Mitch_Madtom_group1$CollectionMonth <- as.factor(Mitch_Madtom_group1$CollectionMonth)
summary(Mitch_Madtom_group1$CollectionMonth)
#modal month = May

Mitch_Madtom_group2 <- Mitch_Madtom %>%
  filter(CollectionYear > 1985 & CollectionYear <=2005)
summary(Mitch_Madtom_group2$CollectionYear)
#get the modal year
get_mode_function(Mitch_Madtom_group2$CollectionYear)
Mitch_Madtom_group2$CollectionMonth <- as.factor(Mitch_Madtom_group2$CollectionMonth)
summary(Mitch_Madtom_group2$CollectionMonth)
#modal month = june

#g3
Mitch_Madtom_group3 <- Mitch_Madtom %>%
  filter(CollectionYear > 2006 & CollectionYear <=2016)
summary(Mitch_Madtom_group3$CollectionYear)
#get the modal year
get_mode_function(Mitch_Madtom_group3$CollectionYear)
Mitch_Madtom_group3$CollectionMonth <- as.factor(Mitch_Madtom_group3$CollectionMonth)
summary(Mitch_Madtom_group3$CollectionMonth)
#modal month = june

#g4

Mitch_Madtom_group4 <- Mitch_Madtom %>%
  filter(CollectionYear > 2017 & CollectionYear <=2024)
summary(Mitch_Madtom_group4$CollectionYear)
#get the modal year
get_mode_function(Mitch_Madtom_group4$CollectionYear)

Mitch_Madtom_group4$CollectionMonth <- as.factor(Mitch_Madtom_group4$CollectionMonth)
summary(Mitch_Madtom_group4$CollectionMonth)
#modal month = june and july

#Mitch_Madtom <- mitch_points %>%
 # filter(COM_NAME == "Carolina Madtom") %>%
  #drop_na(geometry)

ggplot() +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = Mitch_Madtom, color="red", size=1) +
  theme_minimal() +
  labs(title="Carolina Madtom Occurrences")

#Environmental Data

#USGS Data:

#Stream_segment has all six species.
#use Occurrence_Stream_Intersection.shp
stream_seg_occurrence <- file.choose()
stream_seg_occurrence <- st_read(stream_seg_occurrence)
plot(stream_seg_occurrence)
summary(stream_seg_occurrence$seg_id_nat)

#use the dynamic GDFL0ESM2G_historical_r1i1p1_seg_1976_2005
stream_data <- file.choose()
stream_data <- read.csv(stream_data)
stream_data <- stream_data %>%
  filter(seg_id >= 4991 & seg_id <=7503)

#NLCD land use data
landuse_1985 <- file.choose()
landuse_1985 <- rast(landuse_1985)
crs(landuse_1985)
#landuse_1985 <- project(landuse_1985, crs(Mitch_Madtom))
plot(landuse_1985)

landuse_2005 <- file.choose()
landuse_2005 <- rast(landuse_2005)
#landuse_2005 <- project(landuse_2005, crs(Mitch_Madtom))
plot(landuse_2005)

landuse_2007 <- file.choose()
landuse_2007 <- rast(landuse_2007)
crs(landuse_2007)
#landuse_2007 <- project(landuse_2007, crs(Mitch_Madtom))
plot(landuse_2007)

landuse_2024 <- file.choose()
landuse_2024 <- rast(landuse_2024)
crs(landuse_2024)
#landuse_2024 <- project(landuse_2024, crs(Mitch_Madtom))
plot(landuse_2024)

#Get environmental data for each point

stream_seg_occurrence$COM_NAME <- as.factor(stream_seg_occurrence$COM_NAME)
summary(stream_seg_occurrence$COM_NAME)

Madtom_stream_segment <- stream_seg_occurrence %>%
  filter(COM_NAME == "Carolina Madtom")

summary(Madtom_stream_segment$seg_id_nat)
Madtom_stream_segment <- Madtom_stream_segment %>% 
  rename(seg_id = seg_id_nat)

Binded <- left_join(Madtom_stream_segment, stream_data, by = "seg_id", relationship = "many-to-many")

head(Binded)

ggplot() +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = Binded, aes(color = seg_id), size = 2) +   # points colored by seg_Id
  scale_color_viridis_c(option = "plasma") +    # nice continuous color scale
  theme_minimal() +
  labs(title="Carolina Madtom Occurrences")

#link to NLCD data
nlcd_extract <- terra::extract(
  landuse_2007,
  vect(Binded),
  fun = modal,        # most common land cover class within each polygon
  na.rm = TRUE,
  bind = FALSE
)

Binded$nlcd_value <- nlcd_extract[, 2]  # column 1 is ID, column 2 is the value
nlcd_classes <- data.frame(
  nlcd_value = c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 
                 51, 52, 71, 72, 73, 74, 81, 82, 90, 95),
  nlcd_label = c("Open Water", "Perennial Ice/Snow",
                 "Developed, Open Space", "Developed, Low Intensity",
                 "Developed, Medium Intensity", "Developed, High Intensity",
                 "Barren Land", "Deciduous Forest", "Evergreen Forest",
                 "Mixed Forest", "Dwarf Scrub", "Shrub/Scrub",
                 "Herbaceous", "Sedge/Herbaceous", "Lichens", "Moss",
                 "Hay/Pasture", "Cultivated Crops",
                 "Woody Wetlands", "Emergent Herbaceous Wetlands")
)

Binded <- merge(Binded, nlcd_classes, by = "nlcd_value", all.x = TRUE)

ggplot() +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = Binded, aes(color = nlcd_value), size = 2) +   # points colored by seg_Id
  scale_color_viridis_c(option = "plasma") +    # nice continuous color scale
  theme_minimal() +
  labs(title="Carolina Madtom Occurrences")

presences <- Binded %>%
  distinct(seg_id, .keep_all = TRUE) %>%   # one row per unique segment
  mutate(presence = 1)

n.presences <- nrow(presences)
cat("Unique presence sites:", n.presences, "\n")

table(presences$nlcd_label)
table(Binded$nlcd_label)

# 2. BACKGROUND POINTS ###################
# Reproject study area to match Binded CRS if needed
#use ClippedStreamSegments
study_area <- file.choose()
study_area <- st_read(study_area)
#plot(study_area)
summary(study_area$seg_id_nat)

background_segs <- study_area %>%
  slice_sample(n = n.presences * 10) %>%             # 10x presence points
  mutate(presence = 0)

head(background_segs)
head(stream_data)
background_segs <- background_segs %>% 
  rename(seg_id = seg_id_nat)

background_pts <- background_segs %>%
  left_join(stream_data, by = "seg_id")

background_pts <- drop_na(background_pts)

#link to NLCD data
nlcd_extract_background <- terra::extract(
  landuse_2007,
  vect(background_pts),
  fun = modal,        # most common land cover class within each polygon
  na.rm = TRUE,
  bind = FALSE
)

background_pts$nlcd_value <- nlcd_extract_background[, 2]  # column 1 is ID, column 2 is the value
background_pts <- merge(background_pts, nlcd_classes, by = "nlcd_value", all.x = TRUE)

head(background_pts)

ggplot() +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = background_pts, aes(color = nlcd_value), size = 2) +   # points colored by seg_Id
  scale_color_viridis_c(option = "plasma") +    # nice continuous color scale
  theme_minimal() +
  labs(title="Carolina Madtom Occurrences")

summary(background_pts$nlcd_label)
background_pts$nlcd_label <- as.factor(background_pts$nlcd_label)

ggplot() +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = background_pts, aes(color = nlcd_label), size = 2) +   # points colored by seg_Id
  #scale_color_viridis_c(option = "plasma") +    # nice continuous color scale
  theme_minimal() +
  labs(title="Carolina Madtom Occurrences")


# 4. COMBINE PRESENCES + BACKGROUND ###########

hydro_vars <- c("ma3", "ml17", "dh1", "dl1",
                "fh1", "ra1", "spr_mag", "sum_mag")

# Make sure presences also have nlcd_class (from earlier extraction)
presences <- presences %>%
  mutate(
    nlcd_class = case_when(
      nlcd_value %in% c(41, 42, 43)     ~ "forest",
      nlcd_value %in% c(21, 22, 23, 24) ~ "developed",
      nlcd_value %in% c(81, 82)         ~ "agriculture",
      nlcd_value %in% c(90, 95)         ~ "wetland",
      TRUE                              ~ "other"
    )
  )

presences <- drop_na(presences)
all_sites <- bind_rows(presences, background_pts) %>%
  mutate(nlcd_class = factor(nlcd_class,
                             levels = c("forest", "agriculture",
                                        "developed", "wetland", "other")))
n.sites <- nrow(all_sites)
cat("Total sites (presence + background):", n.sites, "\n")

ggplot() +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = presences, aes(color = nlcd_label), size = 2) +   # points colored by seg_Id
  #scale_color_viridis_c(option = "plasma") +    # nice continuous color scale
  theme_minimal() +
  labs(title="Carolina Madtom Occurrences")

table(presences$nlcd_label)
table(background_pts$nlcd_label)

library(flextable)
make_nlcd_flextable <- function(data, label) {
  data.frame(table(data$nlcd_label)) |>
    rename(`Land Cover Class` = Var1, Count = Freq) |>
    mutate(`%` = paste0(round(Count / sum(Count) * 100, 1), "%")) |>
    arrange(desc(Count)) |>
    flextable() |>
    set_caption(paste("NLCD Land Cover Distribution —", label)) |>
    bold(part = "header") |>
    autofit() |>
    theme_booktabs()
}

# Save to .docx
library(officer)
doc <- read_docx() |>
  body_add_flextable(make_nlcd_flextable(presences, "Presence Points")) |>
  body_add_par("") |>
  body_add_flextable(make_nlcd_flextable(background_pts, "Background Points"))

print(doc, target = "nlcd_tables.docx")


table(all_sites$nlcd_label)


ggplot() +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = all_sites, aes(color = nlcd_label), size = 2) +   # points colored by seg_Id
  #scale_color_viridis_c(option = "plasma") +    # nice continuous color scale
  theme_minimal() +
  labs(title="Carolina Madtom Occurrences")

# 5. COORDINATES ####################
coords <- presences %>%
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(x, y) %>%
  as.matrix()

unique_segs <- unique(presences$seg_id)
n.segs      <- length(unique_segs)
grid.index  <- match(presences$seg_id, unique_segs)

cat("Unique segments (spatial knots):", n.segs,           "\n")
cat("Total records:                  ", nrow(all_sites),  "\n")

# One coordinate pair per unique segment
seg_coords <- presences %>%
  mutate(
    x = st_coordinates(st_centroid(.))[, 1],
    y = st_coordinates(st_centroid(.))[, 2]
  ) %>%
  st_drop_geometry() %>%
  group_by(seg_id) %>%
  summarise(
    x = mean(x),
    y = mean(y),
    .groups = "drop"
  ) %>%
  arrange(match(seg_id, unique_segs)) %>%
  select(x, y) %>%
  as.matrix()

# Verify uniqueness of segment coordinates
stopifnot(!anyDuplicated(seg_coords))
cat("Unique coordinate pairs confirmed:", nrow(seg_coords), "\n")

# 2. REBUILD ALL DATA OBJECTS AT RECORD LEVEL
n.sites <- nrow(presences)
y       <- matrix(presences$presence, nrow = n.sites, ncol = 1)

# Occupancy covariates — one row per record
# Save scaling parameters for use in prediction later
hydro_means <- sapply(
  presences %>% st_drop_geometry() %>% select(all_of(hydro_vars)),
  mean, na.rm = TRUE
)
hydro_sds <- sapply(
  presences %>% st_drop_geometry() %>% select(all_of(hydro_vars)),
  sd, na.rm = TRUE
)

scale_with_params <- function(x, mn, sd) (x - mn) / sd

occ.covs <- presences %>%
  st_drop_geometry() %>%
  mutate(
    ma3     = scale_with_params(ma3,     hydro_means["ma3"],     hydro_sds["ma3"]),
    ml17    = scale_with_params(ml17,    hydro_means["ml17"],    hydro_sds["ml17"]),
    dh1     = scale_with_params(dh1,     hydro_means["dh1"],     hydro_sds["dh1"]),
    dl1     = scale_with_params(dl1,     hydro_means["dl1"],     hydro_sds["dl1"]),
    fh1     = scale_with_params(fh1,     hydro_means["fh1"],     hydro_sds["fh1"]),
    ra1     = scale_with_params(ra1,     hydro_means["ra1"],     hydro_sds["ra1"]),
    spr_mag = scale_with_params(spr_mag, hydro_means["spr_mag"], hydro_sds["spr_mag"]),
    sum_mag = scale_with_params(sum_mag, hydro_means["sum_mag"], hydro_sds["sum_mag"])
  ) %>%
  mutate(
    nlcd_class = factor(case_when(
      nlcd_value %in% c(41, 42, 43)     ~ "forest",
      nlcd_value %in% c(21, 22, 23, 24) ~ "developed",
      nlcd_value %in% c(81, 82)         ~ "agriculture",
      nlcd_value %in% c(90, 95)         ~ "wetland",
      TRUE                              ~ "other"
    ), levels = c("forest", "agriculture", "developed", "wetland", "other"))
  ) %>%
  select(all_of(hydro_vars), nlcd_class) %>%
  as.data.frame()

head(occ.covs)
occ.covs <- drop_na(occ.covs)
summary(occ.covs)

# Detection covariates — one row per record #################
roads_sf <-file.choose()
roads_sf <- st_read(roads_sf)
roads_sf <- st_transform(roads_sf, st_crs(all_sites))
dist_to_road <- st_distance(st_centroid(presences), roads_sf) %>%
  apply(1, min) %>%
  as.numeric()

road_mean <- mean(dist_to_road, na.rm = TRUE)
road_sd   <- sd(dist_to_road,   na.rm = TRUE)

det.covs <- list(
  dist_road = matrix(
    (dist_to_road - road_mean) / road_sd,
    nrow = n.sites, ncol = 1
  )
)
summary(det.covs)

# 4. DATA LIST ################
data_list <- list(
  y          = y,
  occ.covs   = occ.covs,
  det.covs   = det.covs,
  coords     = seg_coords,   # segment-level coordinates
  grid.index = grid.index    # maps each record to its segment
)

# 5. PRIORS & INITS####################
dist_range <- dist(seg_coords) %>% as.vector() %>% range()
phi_lower  <- 3 / dist_range[2]
phi_upper  <- 3 / (dist_range[1] * 0.05)

priors <- list(
  beta.normal  = list(mean = 0, var = 2.72),
  alpha.normal = list(mean = 0, var = 2.72),
  phi.unif     = c(phi_lower, phi_upper),
  sigma.sq.ig  = c(2, 1)
)

inits <- list(
  beta     = 0,
  alpha    = 0,
  phi      = mean(c(phi_lower, phi_upper)),
  sigma.sq = 1,
  w        = rep(0, n.segs)   # spatial random effect at segment level
)

# 6. FIT spPGOcc #################

set.seed(123)

out <- spPGOcc(
  occ.formula  = ~ ma3 + ml17 + dh1 + dl1 + fh1 +
    ra1 + spr_mag + sum_mag + nlcd_class,
  det.formula  = ~ dist_road,
  data         = data_list,
  inits        = inits,
  priors       = priors,
  n.neighbors  = min(15, n.segs - 1),  # can't exceed n.segs - 1
  cov.model    = "exponential",
  n.batch      = 900,
  batch.length = 25,
  n.burn       = 5000,
  n.thin       = 5,
  n.chains     = 3,
  tuning       = list(phi = 0.5),
  verbose      = TRUE,
  n.report     = 100
)
# 7. DIAGNOSTICS #######################
summary(out)
#residuals(out)
#ppcOcc(out, "freeman-tukey",1)


# Rhat < 1.1 and ESS > 400 for all parameters
plot(out$beta.samples,  density = FALSE)   # occupancy coefficients
plot(out$alpha.samples, density = FALSE)   # detection coefficients
plot(out$theta.samples, density = FALSE)   # phi, sigma.sq

# Posterior predictive check
ppc_out <- ppcOcc(out, fit.stat = "freeman-tukey", group = 1)
summary(ppc_out)
# Bayesian p-value ideally 0.1 – 0.9
#Bayesian p-value:  0.6698 

# Effective spatial range
phi_samples <- out$theta.samples[, "phi"]
eff_range   <- 3 / phi_samples
cat("\nEffective spatial range (m):",
    round(mean(eff_range)), "±", round(sd(eff_range)), "\n")

beta_df <- data.frame(
  term  = colnames(out$beta.samples),
  mean  = apply(out$beta.samples, 2, mean),
  lower = apply(out$beta.samples, 2, quantile, 0.025),
  upper = apply(out$beta.samples, 2, quantile, 0.975)
)

ggplot(beta_df, aes(x = mean, y = reorder(term, mean))) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(
    x = "Effect size (logit scale)",
    y = NULL,
    title = "Occupancy model coefficients"
  )

#plot detection params

alpha <- out$alpha.samples

alpha_df <- data.frame(
  term  = colnames(alpha),
  mean  = apply(alpha, 2, mean),
  lower = apply(alpha, 2, quantile, 0.025),
  upper = apply(alpha, 2, quantile, 0.975)
)


ggplot(alpha_df, aes(x = mean, y = reorder(term, mean))) +
  geom_point(size = 2, color = "steelblue") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(
    x = "Detection effect (logit scale)",
    y = NULL,
    title = "Detection model coefficients"
  )

theta <- out$theta.samples

sp_df <- data.frame(
  phi= theta[, "phi"],
  sigma_sq= theta[, "sigma.sq"]
)

sp_long <- pivot_longer(
  sp_df,
  cols = everything(),
  names_to = "param",
  values_to = "value"
)

ggplot(sp_long, aes(x = value)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  facet_wrap(~param, scales = "free") +
  theme_minimal() +
  labs(
    title = "Spatial parameters posterior distributions",
    x = NULL,
    y = "Density"
  )

par(mfrow = c(1,3))

plot(density(theta[, "phi"]), main = "phi")
plot(density(theta[, "sigma.sq"]), main = "sigma^2")
plot(density(3/theta[, "phi"]), main = "spatial range")


# 8. PREDICT ACROSS FULL STUDY AREA###############

#use stream segments for entire NC
#use NC-streamSegments.shp
nc_segments <-file.choose()
nc_segments <- st_read(nc_segments)
nc_segments <- st_transform(nc_segments, st_crs(all_sites))

nc_segments <- nc_segments %>% 
  rename(seg_id = seg_id_nat)

head(nc_segments)

#reload in the stream data (previous was subset to only our stream segs w/ occurrence)
#dynamic_GFDL-ESM2G_historical_r1i1p1_seg_1976_2005
#note, try to find 2024 specific stream data
stream_data_nc <- file.choose()
stream_data_nc <- read.csv(stream_data_nc)
#stream_data_nc <- stream_data_nc %>%
#  rename(seg_id = seg_id_nat)
head(stream_data_nc)

pred_sites <- nc_segments %>%
  left_join(stream_data_nc, by = "seg_id")
head(pred_sites)

nlcd_extract_predict <- terra::extract(
  landuse_2024,
  vect(pred_sites),
  fun = modal,        # most common land cover class within each polygon
  na.rm = TRUE,
  bind = FALSE
)

head(nlcd_extract_predict)
pred_sites$nlcd_value <- nlcd_extract_predict[, 2]  # column 1 is ID, column 2 is the value
pred_sites <- merge(pred_sites, nlcd_classes, by = "nlcd_value", all.x = TRUE)

head(pred_sites)

pred_sites <- pred_sites %>%
  mutate(
    nlcd_class = factor(case_when(
      nlcd_value %in% c(41, 42, 43)     ~ "forest",
      nlcd_value %in% c(21, 22, 23, 24) ~ "developed",
      nlcd_value %in% c(81, 82)         ~ "agriculture",
      nlcd_value %in% c(90, 95)         ~ "wetland",
      TRUE                              ~ "other"
    ), levels = c("forest", "agriculture", "developed", "wetland", "other"))
  ) %>%
  mutate(
    ma3     = scale_with_params(ma3,     hydro_means["ma3"],     hydro_sds["ma3"]),
    ml17    = scale_with_params(ml17,    hydro_means["ml17"],    hydro_sds["ml17"]),
    dh1     = scale_with_params(dh1,     hydro_means["dh1"],     hydro_sds["dh1"]),
    dl1     = scale_with_params(dl1,     hydro_means["dl1"],     hydro_sds["dl1"]),
    fh1     = scale_with_params(fh1,     hydro_means["fh1"],     hydro_sds["fh1"]),
    ra1     = scale_with_params(ra1,     hydro_means["ra1"],     hydro_sds["ra1"]),
    spr_mag = scale_with_params(spr_mag, hydro_means["spr_mag"], hydro_sds["spr_mag"]),
    sum_mag = scale_with_params(sum_mag, hydro_means["sum_mag"], hydro_sds["sum_mag"])
  )





# Design matrix — must match occ.formula exactly
X.0 <- model.matrix(
  ~ ma3 + ml17 + dh1 + dl1 + fh1 + ra1 + spr_mag + sum_mag + nlcd_class,
  data = st_drop_geometry(pred_sites)
)

# Prediction coordinates — one per segment
coords.0 <- pred_sites %>%
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(x, y) %>%
  as.matrix()

pred_out <- predict(
  out,
  X.0      = X.0,
  coords.0 = coords.0,
  verbose  = TRUE,
  n.report = 100
)

# Summarise
pred_sites$psi_mean <- apply(pred_out$psi.0.samples, 2, mean)
pred_sites$psi_sd   <- apply(pred_out$psi.0.samples, 2, sd)
pred_sites$psi_lo   <- apply(pred_out$psi.0.samples, 2, quantile, 0.025)
pred_sites$psi_hi   <- apply(pred_out$psi.0.samples, 2, quantile, 0.975)

# 9. MAP #####################
ggplot(pred_sites) +
  geom_sf(data = nc, fill = NA) +
  #geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(aes(color = psi_mean), size = 0.8) +
  scale_color_viridis_c(
    name   = "P(occupancy)",
    option = "plasma",
    limits = c(0, 1)
  ) +
  labs(
    title    = "Carolina Madtom predicted occupancy probability",
  ) +
  theme_minimal()

ggplot(pred_sites) +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = Mitch_Madtom, color="red", size=1) +
  geom_sf(aes(color = psi_mean), size = 0.8) +
  scale_color_viridis_c(
    name   = "P(occupancy)",
    option = "plasma",
    limits = c(0, 1)
  ) +
  labs(
    title    = "Carolina Madtom predicted occupancy probability",
  ) +
  theme_minimal()


#Predicting Future Scenarios##############
landuse_BAU <- file.choose()
landuse_BAU <- rast(landuse_BAU)
crs(landuse_BAU)
plot(landuse_BAU)

landuse_opt <- file.choose()
landuse_opt <- rast(landuse_opt)
crs(landuse_opt)
#landuse_1985 <- project(landuse_1985, crs(Mitch_Madtom))
plot(landuse_opt)

landuse_pess <- file.choose()
landuse_pess <- rast(landuse_pess)
crs(landuse_pess)
#landuse_1985 <- project(landuse_1985, crs(Mitch_Madtom))
plot(landuse_pess)

#BAU 4.5
#Use dynamic_GFDL-ESM2G_rcp45_r1i1p1_seg_2046_2075
summary(pred_sites$seg_id)
stream_data_45 <- file.choose()
stream_data_45 <- read.csv(stream_data_45)
stream_data_45 <- stream_data_45 %>%
  filter(seg_id > 4455)

#Optimistic 2.6
#Use dynamic_GFDL-ESM2G_rcp26_r1i1p1_seg_2046_2075
stream_data_26 <- file.choose()
stream_data_26 <- read.csv(stream_data_26)
head(stream_data_26)
stream_data_26 <- stream_data_26 %>%
  filter(seg_id > 4455)

#Pessimistic 8.5
#Use dynamic_GFDL-ESM2G_rcp85_r1i1p1_seg_2046_2075
stream_data_85 <- file.choose()
stream_data_85 <- read.csv(stream_data_85)
stream_data_85 <- stream_data_85 %>%
  filter(seg_id > 4455)

#BAU########################### 
pred_sites_BAU <- nc_segments %>%
  left_join(stream_data_45, by = "seg_id")
head(pred_sites_BAU)

nlcd_extract_predict_BAU <- terra::extract(
  landuse_BAU,
  vect(pred_sites_BAU),
  fun = modal,        # most common land cover class within each polygon
  na.rm = TRUE,
  bind = FALSE
)

head(nlcd_extract_predict_BAU)
pred_sites_BAU$nlcd_value <- nlcd_extract_predict_BAU[, 2]  # column 1 is ID, column 2 is the value
pred_sites_BAU <- merge(pred_sites_BAU, nlcd_classes, by = "nlcd_value", all.x = TRUE)
head(pred_sites_BAU)

pred_sites_BAU <- pred_sites_BAU %>%
  mutate(
    nlcd_class = factor(case_when(
      nlcd_value %in% c(41, 42, 43)     ~ "forest",
      nlcd_value %in% c(21, 22, 23, 24) ~ "developed",
      nlcd_value %in% c(81, 82)         ~ "agriculture",
      nlcd_value %in% c(90, 95)         ~ "wetland",
      TRUE                              ~ "other"
    ), levels = c("forest", "agriculture", "developed", "wetland", "other"))
  ) %>%
  mutate(
    ma3     = scale_with_params(ma3,     hydro_means["ma3"],     hydro_sds["ma3"]),
    ml17    = scale_with_params(ml17,    hydro_means["ml17"],    hydro_sds["ml17"]),
    dh1     = scale_with_params(dh1,     hydro_means["dh1"],     hydro_sds["dh1"]),
    dl1     = scale_with_params(dl1,     hydro_means["dl1"],     hydro_sds["dl1"]),
    fh1     = scale_with_params(fh1,     hydro_means["fh1"],     hydro_sds["fh1"]),
    ra1     = scale_with_params(ra1,     hydro_means["ra1"],     hydro_sds["ra1"]),
    spr_mag = scale_with_params(spr_mag, hydro_means["spr_mag"], hydro_sds["spr_mag"]),
    sum_mag = scale_with_params(sum_mag, hydro_means["sum_mag"], hydro_sds["sum_mag"])
  )

# Design matrix — must match occ.formula exactly
X.0 <- model.matrix(
  ~ ma3 + ml17 + dh1 + dl1 + fh1 + ra1 + spr_mag + sum_mag + nlcd_class,
  data = st_drop_geometry(pred_sites_BAU)
)

# Prediction coordinates — one per segment
coords.0 <- pred_sites_BAU %>%
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(x, y) %>%
  as.matrix()

pred_out_BAU <- predict(
  out,
  X.0      = X.0,
  coords.0 = coords.0,
  verbose  = TRUE,
  n.report = 100
)

# Summarise
pred_sites_BAU$psi_mean <- apply(pred_out_BAU$psi.0.samples, 2, mean)
pred_sites_BAU$psi_sd   <- apply(pred_out_BAU$psi.0.samples, 2, sd)
pred_sites_BAU$psi_lo   <- apply(pred_out_BAU$psi.0.samples, 2, quantile, 0.025)
pred_sites_BAU$psi_hi   <- apply(pred_out_BAU$psi.0.samples, 2, quantile, 0.975)

# 9. MAP #####################

ggplot(pred_sites_BAU) +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(aes(color = psi_mean), size = 0.8) +
  scale_color_viridis_c(
    name   = "P(occupancy)",
    option = "plasma",
    limits = c(0, 1)
  ) +
  labs(
    title    = "Carolina Madtom predicted occupancy probability BAU projection",
  ) +
  theme_minimal()

ggplot(pred_sites) +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = Mitch_Madtom, color="red", size=1) +
  geom_sf(aes(color = psi_mean), size = 0.8) +
  scale_color_viridis_c(
    name   = "P(occupancy)",
    option = "plasma",
    limits = c(0, 1)
  ) +
  labs(
    title    = "Carolina Madtom predicted occupancy probability",
  ) +
  theme_minimal()

wake <- nc %>%
  dplyr::filter(NAME == "Wake")
wake <- st_transform(wake, st_crs(pred_sites_BAU))
MajorHydro <- st_transform(MajorHydro, st_crs(pred_sites_BAU))

pred_wake <- st_intersection(pred_sites_BAU, wake)
hydro_wake <- st_intersection(MajorHydro, wake)

ggplot(pred_wake) +
  geom_sf(data = wake, fill = NA) +
  geom_sf(data = hydro_wake, color = "lightblue") +
  geom_sf(aes(color = psi_mean), size = 0.8) +
  scale_color_viridis_c(
    name   = "P(occupancy)",
    option = "plasma",
    limits = c(0, 1)
  ) +
  labs(
    title = "Carolina Madtom occupancy",
    subtitle = "BAU"
  ) +
  theme_minimal()

#REPEAT FOR OPTIMISTIC SCENARIO

pred_sites_OPT <- nc_segments %>%
  left_join(stream_data_26, by = "seg_id")
head(pred_sites_OPT)

nlcd_extract_predict_OPT <- terra::extract(
  landuse_opt,
  vect(pred_sites_OPT),
  fun = modal,        # most common land cover class within each polygon
  na.rm = TRUE,
  bind = FALSE
)

head(nlcd_extract_predict_OPT)
pred_sites_OPT$nlcd_value <- nlcd_extract_predict_OPT[, 2]  # column 1 is ID, column 2 is the value
pred_sites_OPT <- merge(pred_sites_OPT, nlcd_classes, by = "nlcd_value", all.x = TRUE)
head(pred_sites_OPT)

pred_sites_OPT <- pred_sites_OPT %>%
  mutate(
    nlcd_class = factor(case_when(
      nlcd_value %in% c(41, 42, 43)     ~ "forest",
      nlcd_value %in% c(21, 22, 23, 24) ~ "developed",
      nlcd_value %in% c(81, 82)         ~ "agriculture",
      nlcd_value %in% c(90, 95)         ~ "wetland",
      TRUE                              ~ "other"
    ), levels = c("forest", "agriculture", "developed", "wetland", "other"))
  ) %>%
  mutate(
    ma3     = scale_with_params(ma3,     hydro_means["ma3"],     hydro_sds["ma3"]),
    ml17    = scale_with_params(ml17,    hydro_means["ml17"],    hydro_sds["ml17"]),
    dh1     = scale_with_params(dh1,     hydro_means["dh1"],     hydro_sds["dh1"]),
    dl1     = scale_with_params(dl1,     hydro_means["dl1"],     hydro_sds["dl1"]),
    fh1     = scale_with_params(fh1,     hydro_means["fh1"],     hydro_sds["fh1"]),
    ra1     = scale_with_params(ra1,     hydro_means["ra1"],     hydro_sds["ra1"]),
    spr_mag = scale_with_params(spr_mag, hydro_means["spr_mag"], hydro_sds["spr_mag"]),
    sum_mag = scale_with_params(sum_mag, hydro_means["sum_mag"], hydro_sds["sum_mag"])
  )

# Design matrix — must match occ.formula exactly
X.0 <- model.matrix(
  ~ ma3 + ml17 + dh1 + dl1 + fh1 + ra1 + spr_mag + sum_mag + nlcd_class,
  data = st_drop_geometry(pred_sites_OPT)
)

# Prediction coordinates — one per segment
coords.0 <- pred_sites_OPT %>%
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(x, y) %>%
  as.matrix()

pred_out_OPT <- predict(
  out,
  X.0      = X.0,
  coords.0 = coords.0,
  verbose  = TRUE,
  n.report = 100
)

# Summarise
pred_sites_OPT$psi_mean <- apply(pred_out_OPT$psi.0.samples, 2, mean)
pred_sites_OPT$psi_sd   <- apply(pred_out_OPT$psi.0.samples, 2, sd)
pred_sites_OPT$psi_lo   <- apply(pred_out_OPT$psi.0.samples, 2, quantile, 0.025)
pred_sites_OPT$psi_hi   <- apply(pred_out_OPT$psi.0.samples, 2, quantile, 0.975)

# 9. MAP #####################
ggplot(pred_sites_OPT) +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(aes(color = psi_mean), size = 0.8) +
  scale_color_viridis_c(
    name   = "P(occupancy)",
    option = "plasma",
    limits = c(0, 1)
  ) +
  labs(
    title    = "Carolina Madtom predicted occupancy probability",
  ) +
  theme_minimal()

ggplot(pred_sites_OPT) +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = Mitch_Madtom, color="red", size=1) +
  geom_sf(aes(color = psi_mean), size = 0.8) +
  scale_color_viridis_c(
    name   = "P(occupancy)",
    option = "plasma",
    limits = c(0, 1)
  ) +
  labs(
    title    = "Carolina Madtom predicted occupancy probability",
  ) +
  theme_minimal()

pred_wake_OPT <- st_intersection(pred_sites_OPT, wake)

ggplot(pred_wake_OPT) +
  geom_sf(data = wake, fill = NA) +
  geom_sf(data = hydro_wake, color = "lightblue") +
  geom_sf(aes(color = psi_mean), size = 0.8) +
  scale_color_viridis_c(
    name   = "P(occupancy)",
    option = "plasma",
    limits = c(0, 1)
  ) +
  labs(
    title = "Carolina Madtom occupancy",
    subtitle = "Optimistic"
  ) +
  theme_minimal()

#REPEAT FOR THE PESSIMISTIC SCENARIO

pred_sites_PESS <- nc_segments %>%
  left_join(stream_data_85, by = "seg_id")
head(pred_sites_PESS)

nlcd_extract_predict_PESS <- terra::extract(
  landuse_pess,
  vect(pred_sites_PESS),
  fun = modal,        # most common land cover class within each polygon
  na.rm = TRUE,
  bind = FALSE
)

head(nlcd_extract_predict_PESS)
pred_sites_PESS$nlcd_value <- nlcd_extract_predict_PESS[, 2]  # column 1 is ID, column 2 is the value
pred_sites_PESS <- merge(pred_sites_PESS, nlcd_classes, by = "nlcd_value", all.x = TRUE)
head(pred_sites_PESS)

pred_sites_PESS <- pred_sites_PESS %>%
  mutate(
    nlcd_class = factor(case_when(
      nlcd_value %in% c(41, 42, 43)     ~ "forest",
      nlcd_value %in% c(21, 22, 23, 24) ~ "developed",
      nlcd_value %in% c(81, 82)         ~ "agriculture",
      nlcd_value %in% c(90, 95)         ~ "wetland",
      TRUE                              ~ "other"
    ), levels = c("forest", "agriculture", "developed", "wetland", "other"))
  ) %>%
  mutate(
    ma3     = scale_with_params(ma3,     hydro_means["ma3"],     hydro_sds["ma3"]),
    ml17    = scale_with_params(ml17,    hydro_means["ml17"],    hydro_sds["ml17"]),
    dh1     = scale_with_params(dh1,     hydro_means["dh1"],     hydro_sds["dh1"]),
    dl1     = scale_with_params(dl1,     hydro_means["dl1"],     hydro_sds["dl1"]),
    fh1     = scale_with_params(fh1,     hydro_means["fh1"],     hydro_sds["fh1"]),
    ra1     = scale_with_params(ra1,     hydro_means["ra1"],     hydro_sds["ra1"]),
    spr_mag = scale_with_params(spr_mag, hydro_means["spr_mag"], hydro_sds["spr_mag"]),
    sum_mag = scale_with_params(sum_mag, hydro_means["sum_mag"], hydro_sds["sum_mag"])
  )

# Design matrix — must match occ.formula exactly
X.0 <- model.matrix(
  ~ ma3 + ml17 + dh1 + dl1 + fh1 + ra1 + spr_mag + sum_mag + nlcd_class,
  data = st_drop_geometry(pred_sites_PESS)
)

# Prediction coordinates — one per segment
coords.0 <- pred_sites_PESS %>%
  st_centroid() %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(x, y) %>%
  as.matrix()

pred_out_PESS <- predict(
  out,
  X.0      = X.0,
  coords.0 = coords.0,
  verbose  = TRUE,
  n.report = 100
)

# Summarise
pred_sites_PESS$psi_mean <- apply(pred_out_PESS$psi.0.samples, 2, mean)
pred_sites_PESS$psi_sd   <- apply(pred_out_PESS$psi.0.samples, 2, sd)
pred_sites_PESS$psi_lo   <- apply(pred_out_PESS$psi.0.samples, 2, quantile, 0.025)
pred_sites_PESS$psi_hi   <- apply(pred_out_PESS$psi.0.samples, 2, quantile, 0.975)

# 9. MAP #####################
ggplot(pred_sites_PESS) +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(aes(color = psi_mean), size = 0.8) +
  scale_color_viridis_c(
    name   = "P(occupancy)",
    option = "plasma",
    limits = c(0, 1)
  ) +
  labs(
    title    = "Carolina Madtom predicted occupancy probability",
  ) +
  theme_minimal()

ggplot(pred_sites_PESS) +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = Mitch_Madtom, color="red", size=1) +
  geom_sf(aes(color = psi_mean), size = 0.8) +
  scale_color_viridis_c(
    name   = "P(occupancy)",
    option = "plasma",
    limits = c(0, 1)
  ) +
  labs(
    title    = "Carolina Madtom predicted occupancy probability",
  ) +
  theme_minimal()


pred_wake_PESS <- st_intersection(pred_sites_PESS, wake)

ggplot(pred_wake_PESS) +
  geom_sf(data = wake, fill = NA) +
  geom_sf(data = hydro_wake, color = "lightblue") +
  geom_sf(aes(color = psi_mean), size = 0.8) +
  scale_color_viridis_c(
    name   = "P(occupancy)",
    option = "plasma",
    limits = c(0, 1)
  ) +
  labs(
    title = "Carolina Madtom occupancy",
    subtitle = "Pessimistic"
  ) +
  theme_minimal()


#plot new data
ggplot(pred_sites_PESS) +
  geom_sf(data = nc, fill = NA) +
  #geom_sf(data = MajorHydro, color="gray") +
  geom_sf(data = pred_sites_PESS, aes(color = dh1), size = 0.8) +
  scale_colour_distiller(palette = "RdBu")+
  labs(
    title    = "Pessimistic dh1",
  ) +
  theme_minimal()

ggplot(pred_sites_OPT) +
  geom_sf(data = nc, fill = NA) +
  #geom_sf(data = MajorHydro, color="gray") +
  geom_sf(data = pred_sites_OPT, aes(color = dh1), size = 0.8) +
  scale_colour_distiller(palette = "RdBu")+
  labs(
    title    = "Optimistic dh1",
  ) +
  theme_minimal()


#"ma3", "ml17", "dh1", "dl1",
#"fh1, "ra1", "spr_mag", "sum_mag"

summary(pred_sites_PESS$ra1)
summary(pred_sites_OPT$ra1)
summary(pred_sites_BAU$ra1)

summary(pred_sites_PESS$dh1)
summary(pred_sites_OPT$dh1)
summary(pred_sites_BAU$dh1)
#