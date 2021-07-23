####################################################################################################
#######                 Global occurrences and predictors of activity of               #############
#######                       Glacier Lake Outburst Floods                             #############
#######                                                                                #############
#######                                  by Georg Veh                                  #############
#######                               V1.1: 12 Nov, 2020                               #############
#######                               V1.2: 11 Jan, 2021                               #############
#######                               V1.3: 02 Feb, 2021                               #############
#######                               V1.4: 01 Mar, 2021                               #############
#######                               V1.5: 08 Jul, 2021                               #############
####################################################################################################


########### Data grooming ##########################################################################

# Load the following packages, or use install.packages("nameofpackage"), if some of them
# are not pre-installed. In some cases you need to restart your R session.

require(readODS)
require(raster)
require(tidyverse)
require(sf)
require(tsibble)

# Set YOUR working directory folder where to find all files, necessary to run this script.
# Change the location appropriately.

setwd("C:/Users/local-admin/Desktop/Plots_GLOFs_global/")

# Open-office spreadsheet with GLOFs per region in separate sheets.

glof.file <- "Global_GLOF_database_2021_06_09.ods"

# Get names of the sheets and append 'Global' for the summary of all.

sheetnames <- list_ods_sheets(glof.file)
sheetnames <- c(sheetnames, "Global")

# Data on possible predictors of GLOF activity.

# I: Global gridded temperature and precipitation from the CRU TS 4.05 data set in netCDF format.
# Navigate to the sources below and unzip the data to your working directory.
# Source for tmp: https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/tmp/cru_ts4.05.1901.2020.tmp.dat.nc.gz
# Source for precip: https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.05/cruts.2103051243.v4.05/pre/cru_ts4.05.1901.2020.pre.dat.nc.gz

# Load the entire netCDF as a stack of rasters into memory.

cru.tmp <- raster::stack("cru_ts4.05.1901.2020.tmp.dat.nc")
cru.pre <- raster::stack("cru_ts4.05.1901.2020.pre.dat.nc")


# II: Glaciological measurements from the Fluctuations of Glaciers (FoG) Database.
# Navigate to the source below and unzip the data into its own folder in your working directory.
# Source: http://wgms.ch/downloads/DOI-WGMS-FoG-2020-08.zip ; DOI: 10.5904/wgms-fog-2020-08

# File with all glaciers in the WGMS database.

wgms <- read_csv("DOI-WGMS-FoG-2020-08/WGMS-FoG-2020-08-A-GLACIER.csv", 
                 col_names = T)

# WGMS glaciers with in-situ measured mass balances.

mb   <- read_csv("DOI-WGMS-FoG-2020-08/WGMS-FoG-2020-08-E-MASS-BALANCE-OVERVIEW.csv", 
                 col_names = T)

# WGMS glaciers with measured front variations.

front <- read_csv("DOI-WGMS-FoG-2020-08/WGMS-FoG-2020-08-C-FRONT-VARIATION.csv", 
                  col_names = T)

# WGMS glaciers with geodetic changes.

dch <- read_csv("DOI-WGMS-FoG-2020-08/WGMS-FoG-2020-08-D-CHANGE.csv", 
                col_names = T, guess_max = 1500)

# Shapefile with all regions of interest. 
# Navigate to the source below and unzip the data into its own folder in your working directory.
# https://github.com/geveh/ReportingBias/raw/main/Region_extents.zip

reg.pol <- st_read("Region_extents/Extent_pol.shp") 

# Convert all glaciers in the WGMS database to points.

wgms.points <-  wgms %>% 
  drop_na(c("LATITUDE", "LONGITUDE")) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Use plot(wgms.points) to show the global distribution of glaciers.

# Make the coordinate reference system of the glacier points identidcal to that of the regions.

wgms.merc <- st_transform(wgms.points, crs = projection(reg.pol))

# Find all unique entries of glaciers with available data of either glaciological,
# geodetic, or length change measurements.

uni.mb    <- unique(mb$WGMS_ID) 
uni.front <- unique(front$WGMS_ID)
uni.dch   <- unique(dch$WGMS_ID)

# Create a list for each region separately (items 1-7) and for all for all regions together (last item).
# At the end of following loop, each list element receives a table of
# the annual number of reported GLOFs, 
# the number of glaciological measurements, 
# and statistics of temperature and precipitation.

region.list <- list()

# Iterate over the names of the spreadsheet.

for(r in sheetnames) {
  
  # Distinguish whether to obtain data only for a specific region, or all regions together.
  # Load the corresponding sheet into memory and 
  # find all glaciers with glaciological surveys in that region (g.sub).
  
  if (r != "Global") { 
    
    data <- as_tibble(read_ods(glof.file, sheet = r))
    data <- data[-c(1:2), 1:33] 
    data$region <- r
    
    if (r == "Other") {
      tf <- (wgms.merc$GLACIER_SUBREGION_CODE %in% "CAU-01") | (wgms.merc$GLACIER_SUBREGION_CODE %in% "NZL")
    } else { tf <- st_intersects(reg.pol[reg.pol$Region == r, ], wgms.merc)  }
    
    g.sub <- wgms.merc[tf[[1]], ] }
  
  if (r == "Global") {
    
    # For the global assessment, iterate over all sheets and bind the outcome together.
    
    glof.list <- list()
    
    for (z in 1 : length(list_ods_sheets(glof.file))) {
      
      glofs.all <- read_ods(glof.file, sheet = z)
      glofs.all <- glofs.all[-c(1:2), 1:33 ]
      glofs.all$region <- list_ods_sheets(glof.file)[z]
      glof.list[[z]] <- glofs.all
      
    }
    
    data <- as_tibble(do.call(rbind, glof.list))
    
    tf <- st_intersects(reg.pol, wgms.merc)
    
    g.sub <- wgms.merc[unlist(tf), ]
    
  } 
  
  
  # Extract all years of measured mass balances in a given region.
  
  years.mb  <- sapply(uni.mb, function (y) {
    
    if (y %in% g.sub$WGMS_ID) {
      
      return( mb$Year[mb$WGMS_ID %in% y])
      
    }
  })
  
  # Extract all years of measured front variations in a given region.
  
  years.front  <- sapply(uni.front, function (y) {
    
    if (y %in% g.sub$WGMS_ID) {
      
      return( front$Year[front$WGMS_ID %in% y])
      
    }
  })
  
  # Extract all years of measured geoedetic mass balances in a given region.
  
  years.dch  <- sapply(uni.dch, function (y) {
    
    if (y %in% g.sub$WGMS_ID) {
      
      return( dch$YEAR[dch$WGMS_ID %in% y])
      
    }
  })
  
  # Generate a vector with the years of reported GLOF occurrences. 
  
  y <- as.numeric(str_sub(data$Date, 1, 4))
  
  # Generate an dataframe with the number of rows equal to a vector 
  # that starts with the year of the first observed GLOF (or in 1901, if none happened before 1901),
  # and that end in the year 2020.
  
  min.y <- min(y, na.rm = T)
  
  if(min.y < 1901) { all.years <- tibble(year = min.y : 2020) } else {  all.years <- tibble(year = 1901 : 2020)}
  
  # Pre-assign zero counts for GLOFs from all lake types, moraine-dammed lakes, ice-dammed lakes,
  # and unknown sources.
  
  all.years <- all.years %>%
    mutate(freq    = 0,
           moraine = 0,
           ice     = 0, 
           other   = 0,
           volc    = 0)
  
  # Extract the years of reported GLOF occurrences. 
  
  for (i in 1 : length(y)) {
    
    lake.type <- data$Lake_type[i]
    
    # Some years are NA because these GLOFs have no fixed date of occurrence, but a range of possible dates instead.
    # For example, some GLOFs were detected from satellite images and offer only the last image before and the
    # next image after the GLOF.
    # If there is NA, we first check whether there is a given range of dates. 
    # If so, we then randomly sample for the range of plausible years. 
    # Finally we increase the observed GLOF count for that dam type in that year by +1.
    
    if (is.na(y[i])) {
      
      min.date <- as.numeric(str_sub(data$Date_Min[i], 1, 4))
      max.date <- as.numeric(str_sub(data$Date_Max[i], 1, 4))
      
      if((!is.na(min.date)) & (!is.na(max.date))) {
        
        obs.period <- min.date:max.date
        
        random.year <- sample(obs.period, size = 1)
        
        all.years$freq[grep(pattern = random.year, x = all.years$year)] <- all.years$freq[grep(pattern = random.year, x = all.years$year)] + 1
        
        if (length(grep("^moraine$", lake.type)) == 1) {
          
          all.years$moraine[grep(pattern = random.year, x = all.years$year)] <- all.years$moraine[grep(pattern = random.year, x = all.years$year)] + 1
          
        } else if (length(grep("^ice$", lake.type)) == 1) {
          
          all.years$ice[grep(pattern = random.year, x = all.years$year)] <- all.years$ice[grep(pattern = random.year, x = all.years$year)] + 1
          
        } else if (length(grep("volc", lake.type)) == 1) {
          
          all.years$volc[grep(pattern = random.year, x = all.years$year)] <- all.years$volc[grep(pattern = random.year, x = all.years$year)] + 1
          
        } else {
          
          all.years$other[grep(pattern = random.year, x = all.years$year)] <- all.years$other[grep(pattern = random.year, x = all.years$year)] + 1
          
        }
        
        y[i] <- random.year
        
      } 
      
    } else { 
      
      # If there an exact year for the GLOF, we just add that year to the existing count.
      
      all.years$freq[grep(pattern = y[i], x = all.years$year)] <- all.years$freq[grep(pattern = y[i], x = all.years$year)] + 1
      
      if (length(grep("^moraine$", lake.type)) == 1) {
        
        all.years$moraine[grep(pattern = y[i], x = all.years$year)] <-all.years$moraine[grep(pattern = y[i], x = all.years$year)] + 1        
        
      } else if (length(grep("^ice$", lake.type)) == 1) {
        
        all.years$ice[grep(pattern = y[i], x = all.years$year)] <- all.years$ice[grep(pattern = y[i], x = all.years$year)] + 1        
        
      } else if (length(grep("volc", lake.type)) == 1) {
        
        all.years$volc[grep(pattern = y[i], x = all.years$year)] <- all.years$volc[grep(pattern = y[i], x = all.years$year)] + 1
        
      } else {
        
        all.years$other[grep(pattern = y[i], x = all.years$year)] <- all.years$other[grep(pattern = y[i], x = all.years$year)] + 1        
      }
      
    }
  }
  
  # Append the year column to the data.frame
  
  data <- data %>% 
    mutate(rounded_year = y)
  
  y[is.na(y)] <- 0
  
  # Now add the data of glacier surveys per year
  
  all.years <- all.years  %>%
    mutate(mb_meas    = 0,
           front_meas = 0,
           dch_meas   = 0)
  
  # For each type of glaciological surveys,
  # - count the number of measurements per year,
  # - delete all measurements that were done before the first GLOF was reported,
  # - match the year of GLOF with the glaciologcial measurement, and 
  # - add that to the dataframe of all.years
  
  t.mb <- table(unlist(years.mb))
  t.mb <- t.mb[!(as.numeric(names(t.mb)) < min(all.years$year, na.rm = T))]
  m.mb <- match(as.numeric(names(t.mb)), all.years$year)
  all.years$mb_meas[m.mb] <- as.numeric(t.mb)
  
  t.front <- table(unlist(years.front))
  t.front <- t.front[!(as.numeric(names(t.front)) < min(all.years$year, na.rm = T))]
  m.front <- match(as.numeric(names(t.front)), all.years$year)
  all.years$front_meas[m.front] <- as.numeric(t.front)
  
  t.dch <- table(unlist(years.dch))
  t.dch <- t.dch[!(as.numeric(names(t.dch)) < min(all.years$year, na.rm = T))]
  m.dch <- match(as.numeric(names(t.dch)), all.years$year)
  all.years$dch_meas[m.dch] <- as.numeric(t.dch)
  
  all.years <- all.years %>%
    rowwise(year) %>%
    mutate(all_meas = sum(c(mb_meas, front_meas, dch_meas)),
           mb_and_front = sum(c(mb_meas, front_meas)))
  
  
  glofs.1901 <- all.years %>% 
    filter(year >= 1901 & year <= 2020) %>%
    add_column(region = r) %>%
    add_column(year_scale = as.numeric(scale(.$year)))
  
  # Select from the inital data.frame all GLOFs that happened between 1901 and 2020 and
  # that are of non-volcanic origin.
  
  glofs.1901.all <- data[(y >= 1901) & (y <= 2020) & (!grepl("volc", data$Lake_type)), ]
  
  #### Obtain temperature and precipitation from all GLOF source locations. 
  
  # First, convert all GLOFs with dates and coordinates to a point shapefile. 
  
  glof.points <-  glofs.1901.all %>% 
    distinct(Latitude, Longitude) %>% 
    drop_na(c("Latitude", "Longitude")) %>% 
    st_as_sf(coords = c( "Longitude", "Latitude"), crs = 4326)
  
  # Extract temperatures from all grid cells in the stacked layers of temperature. 
  # Each year has 12 layers (i.e. one per month) for the period 1901 to 2020.
  
  temp.ts <- raster::extract(cru.tmp, glof.points)
  
  # There are duplicate time series, because several GLOFs might have happened in a grid cell.
  # Remove these entries, and convert to a tibble, in which the rows are the months and 
  # the columns are all extracted time series of temperature.
  
  temp.p <- apply(temp.ts, 1, function (x) paste0(x, collapse = ","))
  temp.cells <- temp.ts[!duplicated(temp.p), ]
  temp.cells.tibble <- as_tibble(t(as.matrix(temp.cells)))
  
  # There is a special format for handling time-stamped entries in a tibbles, called the tsibble.
  
  temp.ts <- tsibble(temp.cells.tibble, 
                     ym = seq(as.Date("1901-01-01"), 
                              as.Date("2020-12-31"), 
                              by = "1 month"), 
                     index =  ym)
  
  # Calculate the annual mean air temperature and the 25th and 75th percentile. 
  # Could choose other quantiles as well.
  
  annual.temp <- temp.ts %>% 
    index_by(year = ~ year(.)) %>% 
    filter(year >= 1901 & year <= 2020) %>%
    summarise(temp_mean = mean(c_across(starts_with("V"))),
              temp_q25  = quantile(c_across(starts_with("V")), 0.25),
              temp_q75  = quantile(c_across(starts_with("V")), 0.75)) 
  
  # Append the temperature statistics to the original dataframe storing the GLOF 
  # counts per year.
  
  glofs.1901 <- left_join(glofs.1901, annual.temp, by = "year")
  
  # Now do the same for precipitation, just calculating the annual sum of precipitation
  # instead of the mean.
  
  pre.ts <- raster::extract(cru.pre, glof.points)
  
  pre.p <- apply(pre.ts, 1, function (x) paste0(x, collapse = ","))
  
  pre.cells <- pre.ts[!duplicated(pre.p), ]
  
  pre.cells.tibble <- as_tibble(t(as.matrix(pre.cells)))
  
  pre.ts <- tsibble(pre.cells.tibble, 
                    ym = seq(as.Date("1901-01-01"), 
                             as.Date("2020-12-31"), 
                             by = "1 month"), 
                    index =  ym)
  
  # Calculate the mean precipitation from all cells and then sum over the entire year.
  
  o <- pre.ts %>% rowwise() %>% mutate(avg = mean(c_across(starts_with("V"))))
  o <- tsibble(o, index = ym)
  annual.pre <- o %>%
    index_by(year = ~ year(.)) %>% 
    filter(year >= 1901 & year <= 2020) %>%
    summarise(pre_sum = sum(avg)) 
  
  # Append the temperature statistics to the original dataframe storing the GLOF 
  # counts per year.
  
  glofs.1901 <- left_join(glofs.1901, annual.pre, by = "year")
  
  # Finally, add this dataframe to the list generated before the loop to save the regional
  # statistics on GLOFs, temperature, precipitation, and glaciological research activity.
  
  region.list[[r]] <- glofs.1901
  
  
}

# Save GLOF counts, temperature, and glacier survey between 1901 and 2020 for each region.

saveRDS(object = region.list, file = "regional_glof_stats.rds")  

# Save GLOF table for all regions 1901-2018 (last iteration in the loop)

saveRDS(object = glofs.1901.all, file = "glofs_1901_2018.rds")  
