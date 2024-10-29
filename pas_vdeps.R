

## try to get vdep chart per fireshed for 4fri area per top 2 BpSs

# use rlandfire package?  

# how to visualize/quantify?

# dependencies ----

# load packages
library(foreign)
library(raster)
library(rlandfire)
library(scales)
library(sf)
library(terra)
library(tidyverse)

## shapefile of project areas from Travis Woolley ----
shp <- st_read("inputs/LANDFIRE_AZ_4FRI_PAs.shp") %>% 
  st_transform(crs = 5070) %>%
  st_union() %>%
  st_sf()

### check the shape ----
vect(shp)
# plot the shape
plot(shp)

## read in attribute tables (I had LANDFIRE ones hand.  Needed to add here as they do not come with downloaded tif files.  pas came from .gdb posted by Kerry Metlen)

bps_conus_atts <- read.csv("inputs/LF20_BPS_220.csv")
scls2020_conus_atts <- read.csv("inputs/LF20_SCla_220.csv")
scls2022_conus_atts <- read.csv("inputs/LF22_SCla_230.csv")
pas_atts <- read.csv("inputs/pas_atts.csv")
ref_con <- read_csv("inputs/ref_con_long.csv")



# get LANDFIRE data: bps, 2020 scls and 2022 scls ----

# ran on October 29, 2024 then commented out
    
aoi <- getAOI(shp)
          
          
# LFPS Parameters
products <-  c("200BPS", "220SCLASS", "230SCLASS")
projection <- 5070
resolution <- 30
          
          
# R specific arguments
save_file <- tempfile(fileext = ".zip")
          
# call API

ncal <- landfireAPI(products,
                     aoi,
                     projection,
                     resolution,
                     path = save_file)

# took just over 1 minute

# extract data to inputs folder and split rasters  ----

# get location of downloaded file, then manually cut paste to inputs dir
          # tempdir()

# process datasets ----

bps_scl <- rast("inputs/pas_lf_data.tif")

# "split" downloaded raster into separate layers
for(lyr in names(bps_scl)) assign(lyr, bps_scl[[lyr]])


# bps 

bps_4fri_pas <- US_200BPS %>%
  crop(shp) %>%
  mask(shp)

plot(bps_4fri_pas)


bps_conus_atts <- read.csv("inputs/LF20_BPS_220.csv")

# bps_aoi <-  bps_conus_r %>%
#   crop(shp) %>%
#   mask(shp)

levels(bps_4fri_pas)[[1]] <- bps_conus_atts
activeCat(bps_4fri_pas) <- "VALUE"


bps_4fri_pas_atts <- values(bps_4fri_pas, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(bps_4fri_pas)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) %>%
  arrange(desc(REL_PERCENT))


writeRaster(bps_4fri_pas, "outputs/bps_4fri_pas.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

write.dbf(bps_4fri_pas_atts, "outputs/bps_4fri_pas.tif.vat.dbf")

## write csv for fun
write.csv(bps_4fri_pas_atts, "outputs/bps_4fri_pas_attributes.csv")


bps10_4fri_pas <- bps_4fri_pas_atts %>%
  group_by(BPS_NAME) %>%
  summarize(ACRES = sum(ACRES),
            REL_PERCENT = sum(REL_PERCENT)) %>%
  arrange(desc(REL_PERCENT)) %>%
  subset(BPS_NAME != "Open Water" & BPS_NAME != "Barren-Rock/Sand/Clay") %>%
  distinct(BPS_NAME, .keep_all = TRUE) %>%
  top_n(n = 10, wt = REL_PERCENT) 


# 2020 Succession Class ----


sclas2020_4fri_pas <- US_220SCLASS %>%
  crop(shp) %>%
  mask(shp)

scls2020_conus_atts <- read.csv("inputs/LF20_SCla_220.csv")

levels(sclas2020_4fri_pas)[[1]] <- scls2020_conus_atts
activeCat(sclas2020_4fri_pas) <- "VALUE"


scls2020_4fri_pas_atts <- values(sclas2020_4fri_pas, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(sclas2020_4fri_pas)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) %>%
  arrange(desc(REL_PERCENT))


writeRaster(sclas2020_4fri_pas, "outputs/sclas2020_4fri_pas.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

write.dbf(scls2020_4fri_pas_atts, "outputs/sclas2020_4fri_pas.tif.vat.dbf")

## write csv for fun
write.csv(scls2020_4fri_pas_atts, "outputs/sclas2020_4fri_pas.csv")


# 2022 Succession Class ----


sclas2022_4fri_pas <- US_230SCLASS %>%
  crop(shp) %>%
  mask(shp)


scls2022_conus_atts <- read.csv("inputs/LF22_SCla_230.csv")



levels(sclas2022_4fri_pas)[[1]] <- scls2022_conus_atts
activeCat(sclas2022_4fri_pas) <- "VALUE"


scls2022_4fri_pas_atts <- values(sclas2022_4fri_pas, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(sclas2022_4fri_pas)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) %>%
  arrange(desc(REL_PERCENT))


writeRaster(sclas2022_4fri_pas, "outputs/sclas2022_4fri_pas.tif",
            gdal = c("COMPRESS=NONE", "TFW=YES"),
            datatype = "INT2S",
            overwrite = T)

write.dbf(scls2022_4fri_pas_atts, "outputs/sclas2022_4fri_pas.tif.vat.dbf")

## write csv for fun
write.csv(scls2022_4fri_pas_atts, "outputs/sclas2022_4fri_pas.csv")



# try to extract combined BpS/Slc raster that was downloaded ----

shp <- st_read("inputs/LANDFIRE_AZ_4FRI_PAs.shp") %>% 
  st_transform(crs = 5070) %>%
  st_sf()

bps_scl_fs_2020 <- terra::extract(bps_scl, shp, df = TRUE, ID = TRUE)   %>%
    group_by(ID, US_200BPS, US_220SCLASS) %>%
    summarize(count = n()) 

bps_scl_fs_2022 <- terra::extract(bps_scl, shp, df = TRUE, ID = TRUE)   %>%
  group_by(ID, US_200BPS, US_230SCLASS) %>%
  summarize(count = n()) 


# wrangle BPS-SCLASS data for 2020 and 2022; merge together for charts ----

## to report by fireshed need to groupby summarize by ID


# need to get rid of US_BPS200; groupby BPS_MODEL


# 2020

bps_scl_fs_2020_full <- bps_scl_fs_2020 %>%
  left_join(scls2020_conus_atts %>% 
              dplyr::select(VALUE, LABEL), 
            by = c("US_220SCLASS" = "VALUE")) %>%
  left_join(bps_conus_atts %>%
              dplyr::select(VALUE, BPS_MODEL),
            by = c("US_200BPS" = "VALUE")) %>%
  filter(!(LABEL %in% c('Barren or Sparse', 'Water'))) %>%
  unite(id_model_label, c("ID", "BPS_MODEL", "LABEL"), remove = FALSE) %>%
  group_by(id_model_label, ID, LABEL, BPS_MODEL) %>%
  summarize(count = sum(count), .groups = 'drop') %>%
  group_by(ID, BPS_MODEL) %>%
  mutate(total_count = sum(count)) %>%
  mutate(currentPercent = as.integer((count/total_count)*100)) %>%
  rename(count2020 = count,
         total_count2020 = total_count,
         current_percent2020 = currentPercent) %>%
  ungroup() %>%  
  dplyr::select(-c(ID, BPS_MODEL, LABEL))


write.csv(bps_scl_fs_2020, "bps_scl_pas_2020.csv")
write.csv(bps_scl_fs_2020_full, "bps_scl_pas_2020_full.csv")


# 2022
bps_scl_fs_2022_full <- bps_scl_fs_2022 %>%
  left_join(scls2022_conus_atts %>% 
              dplyr::select(VALUE, LABEL), 
            by = c("US_230SCLASS" = "VALUE")) %>%
  left_join(bps_conus_atts %>%
              dplyr::select(VALUE, BPS_MODEL),
            by = c("US_200BPS" = "VALUE")) %>%
  filter(!(LABEL %in% c('Barren or Sparse', 'Water'))) %>%
  unite(id_model_label, c("ID", "BPS_MODEL", "LABEL"), remove = FALSE) %>%
  group_by(id_model_label, ID, LABEL, BPS_MODEL) %>%
  summarize(count = sum(count), .groups = 'drop') %>%
  group_by(ID, BPS_MODEL) %>%
  mutate(total_count = sum(count)) %>%
  mutate(currentPercent = as.integer((count/total_count)*100)) %>%
  rename(count2022 = count,
         total_count2022 = total_count,
         current_percent2022 = currentPercent) %>%
  ungroup() %>%  
  select(-c(ID, BPS_MODEL, LABEL))

## ABOVE LOOKS OK-SOMETHING HAPPENS WITH THE SCLASS PERCENTS (AND COUNTS?) IN THE FINAL DF.  IT APPEARS TO BE WITH HOW PERCENTS ARE CALCULATED ----

## generate 'foundation' df for the full join--results is way too long with every bps-scl combo in every fs...will see what happens in join

fs_bps_list <- bps_4fri_pas_atts$BPS_MODEL

#subset ref_con to aoi
aoi_ref_con <- subset(ref_con, model_code %in% fs_bps_list)

# replicate aoi_ref_con, add "to_join" field

full_aoi_ref_con <- do.call(rbind, replicate(94, aoi_ref_con, simplify =  FALSE))

full_aoi_ref_con$ID <- rep(1:94, each = nrow(aoi_ref_con))

full_aoi_ref_con <- full_aoi_ref_con %>%
  unite(to_join, c("ID", "model_label"), remove = FALSE) %>%
  filter(ref_label != "Water")



final_df <- full_aoi_ref_con |>
  left_join(bps_scl_fs_2020_full, by = c("to_join" = "id_model_label"))

final_df <- final_df |>
  left_join(bps_scl_fs_2022_full, by = c("to_join" = "id_model_label"))

# remove groups that only have NA

final_df <- final_df |> 
  group_by(model_code, ID) |>
  filter(!all(is.na(total_count2020))) |>
  mutate_all(funs(replace_na(.,0))) |>
  mutate(change = current_percent2020 - current_percent2022,
         sign_change = (change > 0),
         bps_acres = round(max(total_count2020) * 0.222))


write.csv(final_df, file = "pas_final_df.csv")
