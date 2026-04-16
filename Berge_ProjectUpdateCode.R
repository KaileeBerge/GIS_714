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
#va <- st_read("VA_Counties/VA_Counties.shp")
#md <- st_read("Maryland_County/Maryland_Physical_Boundaries_-_County_Boundaries_(Detailed).shp")
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

#Mitch_Madtom <- mitch_points %>%
 # filter(COM_NAME == "Carolina Madtom") %>%
  #drop_na(geometry)

ggplot() +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = Mitch_Madtom, color="red", size=1) +
  theme_minimal() +
  labs(title="Carolina Madtom Occurrences")

wc <- worldclim_country("USA", var="bio", res=10, path="worldclim_data")
wc <- setMinMax(wc)

precip <- worldclim_country("USA", var = "prec", res = 5, path = "worldclim_data")
temp_avg <- worldclim_country("USA", var = "tavg", res = 5, path = "worldclim_data")

landuse <- file.choose()
landuse_rast <- rast(landuse)
landuse_rast <- project(landuse_rast, crs(Mitch_Madtom))
plot(landuse_rast)

#legend<-pal_nlcd()
#legend
# Reference: https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description
codes <- c(0, 11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 51, 52, 71, 72, 73, 74, 81, 82, 90, 95)
labels <- c("Unclassified", "Open Water", "Perennial Ice/Snow", "Developed, Open",
            "Developed, Low", "Developed, Med", "Developed, High", "Barren Land",
            "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Dwarf Scrub",
            "Shrub/Scrub", "Grassland/Herbaceous", "Sedge/Herbaceous", "Lichens",
            "Moss", "Pasture/Hay", "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands")

nlcd_map <- data.frame(ID = codes, Cover = labels)

#Convert to factor using lookup table
nlcd_categorical <- as.factor(landuse_rast)
levels(nlcd_categorical) <- nlcd_map

cats(nlcd_categorical)
plot(nlcd_categorical)


# Crop to NC extent
nc_vect <- vect(nc)
plot(nc_vect)

#precip
precip_crop  <- crop(precip, nc_vect)
precip_nc <- mask(precip_crop, nc_vect)
minmax(precip_nc[[1]])
plot(precip_nc[[1]])

#temps
temp_crop  <- crop(temp_avg, nc_vect)
temp_nc <- mask(temp_crop, nc_vect)
minmax(temp_nc[[1]])
plot(temp_nc[[1]])

#get mean temp and total precip
bio_temp <- app(temp_nc, mean)
plot(bio_temp[[1]])
bio_precip <- app(precip_nc, mean)
plot(bio_precip[[1]])

#Adding in landuse or dev data
landuse_rast <- project(landuse_rast, crs(bio_temp))
#landuse_rast_aligned <- terra::resample(landuse_rast, bio_temp, method="near")
#plot(landuse_rast_aligned)
#env_vars <- c(bio_temp, bio_precip,landuse_rast_aligned)
#names(env_vars) <- c("mean_temp","annual_precip","landuse")


env_vars <- c(bio_temp, bio_precip)
names(env_vars) <- c("mean_temp","annual_precip")
head(env_vars)
Madtom_vect <- vect(Mitch_Madtom)
env_values <- terra::extract(env_vars, Madtom_vect)

head(env_values)
env_data <- cbind(
  Mitch_Madtom,
  env_values
)
head(env_data)
env_data <- env_data %>% drop_na()
head(env_data)
valid_rows <- complete.cases(st_drop_geometry(env_data))
env_data <- env_data[valid_rows, ]

#manually make land use data for the simulation 
env_data <- env_data %>%
  mutate(x_coord = st_coordinates(.)[,1])
env_data <- env_data %>%
  mutate(dev_level = 6 - ntile(x_coord, 5))
env_data <- env_data %>%
  mutate(dev_level = 6 - ntile(x_coord, 5))
env_data <- env_data %>%
  mutate(dev_level = factor(dev_level, levels = 1:5))

#plot the temp
ggplot(data = env_data) +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = Mitch_Madtom, color="red", size=1) +
  geom_sf(aes(color = mean_temp), size = 2) +   # points colored by mean_temp
  scale_color_viridis_c(option = "plasma") +    # nice continuous color scale
  labs(
    color = "Mean Temp (°C)"
  ) +
  theme_minimal()

#plot the precip again
ggplot(data = env_data) +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = Mitch_Madtom, color="red", size=1) +
  geom_sf(aes(color = annual_precip), size = 2) +   # points colored by mean_temp
  scale_color_viridis_c(option = "plasma") +    # nice continuous color scale
  labs(
    color = "Annual Precip. (mm)"
  ) +
  theme_minimal()

#plot the development
ggplot(data = env_data) +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = Mitch_Madtom, color="red", size=1) +
  geom_sf(aes(color = dev_level), size = 2) +   # points colored by mean_temp
  labs(
    color = "Developed"
  ) +
  theme_minimal()

#Fit the Spatial occupancy model with the simulated land cover data
coords <- st_coordinates(Mitch_Madtom)[1:nrow(env_data), ]
# Extract coords and add to env_data
env_data$X <- round(coords[,1], 5)  # round to 5 decimal places
env_data$Y <- round(coords[,2], 5)
env_data <- st_drop_geometry(env_data)


# Group by unique site (rounded coordinates)
site_data <- env_data %>%
  group_by(X, Y) %>%
  summarize(
    mean_temp = mean(mean_temp, na.rm = TRUE),
    annual_precip = mean(annual_precip, na.rm = TRUE),
    dev_level = first(dev_level),
    .groups = "drop"
  )

coords <- as.matrix(site_data[, c("X", "Y")])

#coords <- as.matrix(site_data %>% select(X, Y))
set.seed(123)

n_sites <- nrow(site_data)
n_visits <- 3

# True occupancy probability
psi <- plogis(
  -1 +
    2 * as.numeric(scale(site_data$mean_temp)) +
    4 * as.numeric(scale(site_data$annual_precip))+
    -8 * as.numeric(site_data$dev_level)
)

z <- rbinom(n_sites,1,psi)

# detection probability
p <- plogis(-0.3 + 0.5*as.numeric(scale(site_data$annual_precip)))

y <- matrix(0,n_sites,n_visits)

for(i in 1:n_sites){
  for(j in 1:n_visits){
    y[i,j] <- rbinom(1,1,z[i]*p[i])
  }
}

occ_covs <- data.frame(
  annual_precip = scale(site_data$annual_precip),
  mean_temp = scale(site_data$mean_temp),
  dev_level = as.numeric(site_data$dev_level)
)

det_covs <- list(
  precip = matrix(rep(scale(site_data$annual_precip), n_visits),
                  nrow = n_sites, ncol = n_visits)
)

data_list <- list(
  y = y,
  occ.covs = occ_covs,
  det.covs = det_covs,
  coords = coords
)

out_spatial <- spPGOcc(
  occ.formula = ~ mean_temp + annual_precip + dev_level,
  det.formula = ~ precip,
  data = data_list,
  NNGP = TRUE,
  n.neighbors = 10,
  n.batch = 800,
  batch.length = 25,
  verbose = TRUE
)

summary(out_spatial)


library(bayesplot)

posterior <- as.matrix(out_spatial$beta.samples)

mcmc_areas(
  posterior,
  prob = 0.95
)+
  scale_x_continuous(breaks = seq(-4, 4, by = 1))



#Make new prediction data for current state

#site data currently has the gradient we want to predict on. 
plot(site_data$X, site_data$Y)
site_data$dev_level <- as.numeric(as.character(site_data$dev_level))
grid_coords <- coords


#plot
X0 <- model.matrix(~ mean_temp + annual_precip + dev_level, data = site_data)
X0_df <- as.data.frame(cbind(X0, grid_coords))
X0_sf <- st_as_sf(
  X0_df,
  coords = c("X", "Y"),
  crs = st_crs(Mitch_Madtom)
  )


ggplot() +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  geom_sf(data = Mitch_Madtom, color="red", size=1) +
  geom_sf(data = X0_sf, aes(color = mean_temp), size = 2) +
  scale_color_viridis_c(name = "mean temp") +
  theme_minimal() +
  labs(title = "New Data Temp")

ggplot() +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  #geom_sf(data = Mitch_Madtom, color="red", size=1) +
  geom_sf(data = X0_sf, aes(color = annual_precip), size = 2) +
  scale_color_viridis_c(name = "precip") +
  theme_minimal() +
  labs(title = "Current Predicted Carolina Madtom Occupancy")


ggplot() +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  #geom_sf(data = Mitch_Madtom, color="red", size=1) +
  geom_sf(data = X0_sf, aes(color = dev_level), size = 2) +
  scale_color_viridis_c(name = "dev_level") +
  theme_minimal() +
  labs(title = "Current Predicted Carolina Madtom Occupancy")

X.0 <- site_data[,3:5] 
pred <- predict(
  out_spatial,
  X.0 = X0,
  coords.0 = grid_coords
)

psi_mean <- apply(pred$psi.0.samples, 2, mean)

pred_df <- data.frame(
  X = coords[,1],
  Y = coords[,2],
  psi = psi_mean)

pred_sf <- st_as_sf(
  pred_df,
  coords = c("X", "Y"),
  crs = st_crs(Mitch_Madtom)   # use same CRS as your sites
)

#mapping simulated occupancy probability
ggplot() +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  #geom_sf(data = Mitch_Madtom, color="red", size=1) +
  geom_sf(data = pred_sf, aes(color = psi), size = 2) +
  scale_color_viridis_c(name = "Occupancy Probability") +
  theme_minimal() +
  labs(title = "Current Predicted Carolina Madtom Occupancy")

#Fitting Current data to Spatial integrated SDM 
#use earlier data and 2016 Land use data

set.seed(400)

#Above was the single species spatial model for a SINGLE data source

#Below: Single species spatial model for Multipled data sources (integrated)

# Simulate Data
# Number of locations in each direction. This is the total region of interest
# where some sites may or may not have a data source. 
J.x <- 8
J.y <- 8
J.all <- J.x * J.y
# Number of data sources.
n.data <- 4
# Sites for each data source. 
J.obs <- sample(ceiling(0.2 * J.all):ceiling(0.5 * J.all), n.data, replace = TRUE)
# Replicates for each data source.
n.rep <- list()
for (i in 1:n.data) {
  n.rep[[i]] <- sample(1:4, size = J.obs[i], replace = TRUE)
}
# Occupancy covariates
beta <- c(0.5, 0.5)
p.occ <- length(beta)
# Detection covariates
alpha <- list()
alpha[[1]] <- runif(2, 0, 1)
alpha[[2]] <- runif(3, 0, 1)
alpha[[3]] <- runif(2, -1, 1)
alpha[[4]] <- runif(4, -1, 1)
p.det.long <- sapply(alpha, length)
p.det <- sum(p.det.long)
sigma.sq <- 2
phi <- 3 / .5
sp <- TRUE

# Simulate occupancy data from multiple data sources. 
dat <- simIntOcc(n.data = n.data, J.x = J.x, J.y = J.y, J.obs = J.obs, 
                 n.rep = n.rep, beta = beta, alpha = alpha, sp = sp, 
                 sigma.sq = sigma.sq, phi = phi, cov.model = 'exponential')

y <- dat$y
X <- dat$X.obs
X.p <- dat$X.p
sites <- dat$sites
X.0 <- dat$X.pred
psi.0 <- dat$psi.pred
coords <- as.matrix(dat$coords.obs)
coords.0 <- as.matrix(dat$coords.pred)

# Package all data into a list
occ.covs <- X[, 2, drop = FALSE]
colnames(occ.covs) <- c('occ.cov')
det.covs <- list()
# Add covariates one by one
det.covs[[1]] <- list(det.cov.1.1 = X.p[[1]][, , 2])
det.covs[[2]] <- list(det.cov.2.1 = X.p[[2]][, , 2], 
                      det.cov.2.2 = X.p[[2]][, , 3])
det.covs[[3]] <- list(det.cov.3.1 = X.p[[3]][, , 2])
det.covs[[4]] <- list(det.cov.4.1 = X.p[[4]][, , 2], 
                      det.cov.4.2 = X.p[[4]][, , 3], 
                      det.cov.4.3 = X.p[[4]][, , 4])
data.list <- list(y = y, 
                  occ.covs = occ.covs,
                  det.covs = det.covs, 
                  sites = sites, 
                  coords = coords)

J <- length(dat$z.obs)

# Initial values
inits.list <- list(alpha = list(0, 0, 0, 0), 
                   beta = 0, 
                   phi = 3 / .5, 
                   sigma.sq = 2, 
                   w = rep(0, J), 
                   z = rep(1, J))
# Priors
prior.list <- list(beta.normal = list(mean = 0, var = 2.72), 
                   alpha.normal = list(mean = list(0, 0, 0, 0), 
                                       var = list(2.72, 2.72, 2.72, 2.72)),
                   phi.unif = c(3/1, 3/.1), 
                   sigma.sq.ig = c(2, 2))
# Tuning
tuning.list <- list(phi = 0.3) 

# Number of batches
n.batch <- 2
# Batch length
batch.length <- 25

# Note that this is just a test case and more iterations/chains may need to 
# be run to ensure convergence.
out <- spIntPGOcc(occ.formula = ~ occ.cov, 
                  det.formula = list(f.1 = ~ det.cov.1.1, 
                                     f.2 = ~ det.cov.2.1 + det.cov.2.2, 
                                     f.3 = ~ det.cov.3.1, 
                                     f.4 = ~ det.cov.4.1 + det.cov.4.2 + det.cov.4.3), 
                  data = data.list,  
                  inits = inits.list, 
                  n.batch = n.batch, 
                  batch.length = batch.length, 
                  accept.rate = 0.43, 
                  priors = prior.list, 
                  cov.model = "exponential", 
                  tuning = tuning.list, 
                  n.omp.threads = 1, 
                  verbose = TRUE, 
                  NNGP = FALSE, 
                  n.report = 10, 
                  n.burn = 10, 
                  n.thin = 1)

summary(out)





#Predicting Future Scenarios#########

#Make future data to predict into the future. 
#use bioclim again 
#Business as usual scenario?
clim_fut <- geodata::cmip6_world(model='ACCESS-ESM1-5', ssp='245', time='2041-2060', var='bioc', download=F, res=10, path='data')

# Inspect the SpatRaster object:

future_nc <- crop(clim_fut, nc_vect)
future_nc <- mask(future_nc, nc_vect)
future_nc
names(future_nc)
plot(future_nc)


#cheat sheet for the variable codes
#https://www.worldclim.org/data/bioclim.html
#mean temp and annual precip

future_X0 <- future_nc[[c(1,12)]]
head(future_X0)

#note to kailee 
#in downloads folder landuse_2038
futures <- file.choose()
futures_rast <- rast(futures)
futures_rast <- project(futures_rast, crs(Mitch_Madtom))
plot(futures_rast)

#same as above, convert to factor w/ 
# Reference: https://www.mrlc.gov/data/legends/national-land-cover-database-class-legend-and-description
codes <- c(0, 11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 51, 52, 71, 72, 73, 74, 81, 82, 90, 95)
labels <- c("Unclassified", "Open Water", "Perennial Ice/Snow", "Developed, Open",
            "Developed, Low", "Developed, Med", "Developed, High", "Barren Land",
            "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Dwarf Scrub",
            "Shrub/Scrub", "Grassland/Herbaceous", "Sedge/Herbaceous", "Lichens",
            "Moss", "Pasture/Hay", "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands")

nlcd_map <- data.frame(ID = codes, Cover = labels)

#Convert to factor using lookup table
nlcd_categorical_future <- as.factor(futures_rast)
levels(nlcd_categorical_future) <- nlcd_map

cats(nlcd_categorical_future)
plot(nlcd_categorical_future)


future_X0_df <- as.data.frame(future_X0, xy =TRUE)
head(future_X0_df)


#RAN OUT OF TIME HERE. 
#prediction function from spOccupancy
future_pred <- predict(
  out_spatial,
  X.0 = future_X0_df,
  coords.0 = grid_coords
)
head(future_pred)

future_psi_mean <- apply(future_pred$psi.0.samples, 2, mean)

future_pred_df <- data.frame(
  X = future_X0_df$x,
  Y = future_X0_df$y,
  psi = future_psi_mean)

pred_sf <- st_as_sf(
  pred_df,
  coords = c("X", "Y"),
  crs = st_crs(Mitch_Madtom)   # use same CRS as your sites
)

#mapping simulated occupancy probability
ggplot() +
  geom_sf(data = nc, fill = NA) +
  geom_sf(data = MajorHydro, color="lightblue") +
  #geom_sf(data = Mitch_Madtom, color="red", size=1) +
  geom_sf(data = pred_sf, aes(color = psi), size = 2) +
  scale_color_viridis_c(name = "Occupancy Probability") +
  theme_minimal() +
  labs(title = "Current Predicted Carolina Madtom Occupancy")
