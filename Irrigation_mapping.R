library(readxl)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(tm)

# irrigation wateruse
WaterUse <- read_excel("N:/Water Information/Queries/WaterUseSummaryReport2.xlsx", 
                       sheet = "WaterUseDetail") %>% 
  filter(ConsentID != "WP123456T") # remove the test consent

# anything but 'irrigation' as primary use from local councils 
# Napier City Council, Wairoa District Council, Hastings District Council
# we will use AUTH ID to represent these properties
Napier_ID = WaterUse %>% 
  filter(AuthHolder == "Napier City Council") %>% 
  filter(PrimaryUse != "Irrigation") %>% 
  pull(ResourceConsentID)

Wairoa_ID = WaterUse %>% 
  filter(AuthHolder == "Wairoa District Council")  %>% 
  filter(PrimaryUse != "Irrigation") %>% 
  pull(ResourceConsentID)

Hastings_ID = WaterUse %>% 
  filter(AuthHolder == "Hastings District Council") %>% 
  filter(PrimaryUse != "Irrigation") %>% 
  pull(ResourceConsentID)

WaterUse = WaterUse %>% 
  filter(! (ResourceConsentID %in% c(Napier_ID, Wairoa_ID, Hastings_ID))) 

# fill the missing AUTHs
WaterUse$ResourceConsentID[which(WaterUse$ConsentID == "WP180108T")] = "AUTH-123565-01"
WaterUse$AuthHolder[which(WaterUse$ConsentID == "WP180108T")] = "Waikare Dairy Trust"

WaterUse$ResourceConsentID[which(WaterUse$ConsentID == "WP180109T")] = "AUTH-123499-01"
WaterUse$AuthHolder[which(WaterUse$ConsentID == "WP180109T")] = "Brylee Farm Limited"

WaterUse$ResourceConsentID[which(WaterUse$ConsentID == "WP180528t")] = "AUTH-123705-01"
WaterUse$AuthHolder[which(WaterUse$ConsentID == "WP180528t")] = "Maori Trustee"

WaterUse$ResourceConsentID[which(WaterUse$ConsentID == "WP180622T")] = "AUTH-124271-01"
WaterUse$AuthHolder[which(WaterUse$ConsentID == "WP180622T")] = "Te Hau Farming Company Limited"

WaterUse$ResourceConsentID[which(WaterUse$ConsentID == "WP180585Ta")] = "AUTH-123739-02"
WaterUse$AuthHolder[which(WaterUse$ConsentID == "WP180585Ta")] = "Wharerangi Corner Trust"

WaterUse19_20 = WaterUse %>% 
  filter(WaterYear == "2019-2020") %>% 
  select(ResourceConsentID, ConsentID, AuthHolder, `Total for Consent`) %>% 
  group_by(ResourceConsentID, ConsentID, AuthHolder) %>%
  summarise(sum_waterUse = sum(`Total for Consent`))

# consent layer
Consent = st_read(dsn = "N:/Gorden_Jiang/temp/Tukituki FEMPSubmissions2018.gdb",
                  layer = "trythis_allresourceconsentspolygons")
Consent = subset(Consent, Consent$AuthorisationActivityType == "Water Permit") %>% 
  select(AuthorisationIRISID, AuthorisationHistoricID, AuthorisationActivityType, AuthorisationHolderContactName)

# irrigation landuse layer
Irrigation = st_read(dsn = "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris", layer = "Irrigation_Layer_landuse")
HBboundary = st_read(dsn = "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris", layer = "HBboundary")
HBIrrigation <- Irrigation[HBboundary,] 

# join irrigation water use to consent layer
Consent_wateruse = Consent %>% 
  rename(ResourceConsentID = AuthorisationIRISID, 
         ConsentID = AuthorisationHistoricID, 
         AuthHolder = AuthorisationHolderContactName) %>% 
  select(ResourceConsentID) %>% 
  right_join(WaterUse19_20, by =  c("ResourceConsentID"))

Consent_wateruse_spt = Consent_wateruse %>% filter(nchar(Shape) == 6) # the AUTHs with missing geometry! 
# write.csv(Consent_wateruse_spt, "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris/Consent_wateruse_spt.csv")


# Grab more consent's data from Tim Farrier
Consent_sply = st_read(dsn = "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris/IrrigationAnalysis.gdb", 
                       layer = "IRISPolygonsPlusConsentWaterUse_210916")

Consent_spt = st_read(dsn = "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris/IrrigationAnalysis.gdb",
                      layer = "IRISPointsPlusConsentWaterUse_210916")

Consent_wateruse_spt2 = Consent_sply %>% 
  select(AuthorisationIRISID, AuthorisationHistoricID, AuthorisationHolderContactName, AuthorisationActivityType) %>% 
  rename(ResourceConsentID = AuthorisationIRISID, 
         ConsentID = AuthorisationHistoricID, 
         AuthHolder = AuthorisationHolderContactName
         ) %>% 
  select(ResourceConsentID) %>% 
  right_join(Consent_wateruse_spt %>% 
               st_drop_geometry(),
             by= c("ResourceConsentID")) %>% 
  filter(nchar(SHAPE) == 6) # the AUTHs with missing geometry! 

Consent_wateruse_spt_filled = Consent_spt %>% 
  select(AuthorisationIRISID, AuthorisationHistoricID, AuthorisationHolderContactName, AuthorisationActivityType) %>% 
  rename(ResourceConsentID = AuthorisationIRISID,
         ConsentID = AuthorisationHistoricID, 
         AuthHolder = AuthorisationHolderContactName) %>% 
  select(ResourceConsentID) %>% 
  right_join(Consent_wateruse_spt2 %>% 
               st_drop_geometry(),
             by= c("ResourceConsentID"))


############ Two existing consent polygons with water use data ###############

Consent_wateruse_sply1 = Consent_wateruse %>% 
  filter(nchar(Shape) > 6) %>% 
  rename(SHAPE = Shape) %>% 
  mutate(Source = "Consent polygon")

Consent_wateruse_sply2 = Consent_sply %>% 
  select(AuthorisationIRISID, AuthorisationHistoricID, AuthorisationHolderContactName, AuthorisationActivityType) %>% 
  rename(ResourceConsentID = AuthorisationIRISID, 
         ConsentID = AuthorisationHistoricID, 
         AuthHolder = AuthorisationHolderContactName
  ) %>% 
  select(ResourceConsentID) %>% 
  right_join(Consent_wateruse_spt %>% 
               st_drop_geometry(),
             by= c("ResourceConsentID")) %>% 
  filter(nchar(SHAPE) > 6) %>% 
  mutate(Source = "Consent polygon")

########### convert remaining consent points to polygons using property title ###########

# load property title
library(DBI)  #need to set up ODBC
library(odbc) 
con <- dbConnect(odbc::odbc(), "hbrc_sde_vector")  
AllProperty <- dbGetQuery(con, "SELECT *, [Shape].STAsText() as Geom from [sdeadmin].[HawkesBay_LINZ_Property_Titles_Including_Owners]")
dbDisconnect(con)

# load property title
AllProperty = st_as_sf(AllProperty, wkt = "Geom")

# dissolve property parcels based on owners name and share boundaries.
target_parcels_sp = as(AllProperty, 'Spatial')
target_parcels_sp = raster::buffer(target_parcels_sp, width = 0, dissolve = F)
polya = raster::aggregate(target_parcels_sp, dissolve = TRUE, by = "owners") #aggregate initial polygon output into single multi-part features based on names
polyb <- disaggregate(polya) #disaggregate multi-part polygon into separate features
polyb = st_as_sf(polyb) %>% st_set_crs(2193)

# test1 = st_read(dsn = "N:/Gorden_Jiang/GISTeam/210921/PropertyTitlesIncludingOwners.gdb", layer = "HawkesBay_Property_Titles_Including_Owners_210921") # from Tim Farrier's export

Consent_wateruse_sply3 = polyb %>% 
  st_join(Consent_wateruse_spt_filled, left = T) %>% # join consent information from point layer to the target parcel, then remove the parcels with no match in the consent.
  filter(!is.na(ResourceConsentID)) %>% 
  mutate(Source = "Consent points") %>% 
  rename(SHAPE = geometry) %>% 
  select(-owners)

# unique(Consent_wateruse_spt_filled$ResourceConsentID[!(Consent_wateruse_spt_filled$ResourceConsentID %in% Consent_wateruse_sply3$ResourceConsentID)])

############ merge existing consent polygons ###############

Consent_wateruse_sply = rbind(Consent_wateruse_sply1, Consent_wateruse_sply2, Consent_wateruse_sply3)

HBIrrigation_waterUse = HBIrrigation %>% 
  st_join(Consent_wateruse_sply, left = T) %>% 
  select(-Shape_Leng, -Shape_Area)

HBIrrigation_waterUse = as(HBIrrigation_waterUse, 'Spatial')
polyc = raster::aggregate(HBIrrigation_waterUse, dissolve = TRUE, 
                          by = c("ResourceConsentID","Landuse","sum_waterUse")) # dissolve again to aggregate individual polygon into multi-polygon as Monique suggests
polyc$Area = raster::area(polyc)/10000
HBIrrigation_waterUse = st_as_sf(polyc) %>% st_set_crs(2193)

HBIrrigation_waterUse = HBIrrigation_waterUse %>% 
  group_by(ResourceConsentID) %>% 
  mutate(Area_Prop = Area / sum(Area)) %>% 
  mutate(waterUse = sum_waterUse * Area_Prop) %>%  # calculate water use based on the areal proportions of each land use type
  select(-sum_waterUse,-Area,-Area_Prop)

# write.csv(HBIrrigation_waterUse, "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris/HBIrrigation_waterUse.csv")

mapview::mapviewOptions(fgb = FALSE)
mp = mapview::mapview(HBIrrigation_waterUse, zcol = "waterUse", layer.name = "Sum water use", 
                 col.regions = function (n, alpha = 1, begin = 0, end = 1, direction = 1) {
                   colors <- RColorBrewer::brewer.pal(9, "YlOrRd")
                   if (direction < 0) colors <- rev(colors)
                   colorRampPalette(colors, alpha = alpha)(n)
                   }, at = c(0, 20000, 50000, 150000, 12000000)
)

# mapview::mapshot(mp, url = "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris/HBIrrigation_waterUse.html")

# st_write(Consent_wateruse19_20, "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris/Consent_wateruse19_20.shp")

# summary of hectares based on landuse
HBIrrigation %>% 
  group_by(Landuse) %>% 
  summarise(Hectare = sum(area_ha))

# summary of wateruse based on landuse
HBIrrigation_waterUse  %>% 
  group_by(Landuse) %>% 
  summarise(sum_waterUse = sum(waterUse, na.rm = T)/1000000)

############ check the missing IDs (IDs exist in Jo's Water Use data but not in the produced map) #############

ID_missed = WaterUse19_20$ResourceConsentID[!(WaterUse19_20$ResourceConsentID %in% HBIrrigation_waterUse$ResourceConsentID)]
# write.csv(ID_missed, "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris/ID_missed.csv")

Consent_wateruse_sply_IDmissed = Consent_wateruse_sply %>% filter(ResourceConsentID %in% ID_missed)
Consent_wateruse_spt_filled_IDmissed = Consent_wateruse_spt_filled %>% filter(ResourceConsentID %in% ID_missed)

Consent_wateruse_spt_filled_IDmissed_sply = polyb %>% 
  st_join(Consent_wateruse_spt_filled_IDmissed, left = T) %>% # join consent information from point layer to the target parcel, then remove the parcels with no match in the consent.
  filter(!is.na(ResourceConsentID)) %>% 
  rename(SHAPE = geometry) %>% 
  select(-owners)
  
Consent_wateruse_sply_noLanduse = rbind(Consent_wateruse_spt_filled_IDmissed_sply, 
                                        Consent_wateruse_sply_IDmissed %>% select(-Source))

Consent_wateruse_sply_noLanduse = as(Consent_wateruse_sply_noLanduse, 'Spatial')
polyd = raster::aggregate(Consent_wateruse_sply_noLanduse, dissolve = TRUE, by = c("ResourceConsentID","sum_waterUse")) # dissolve again to aggregate individual polygon into multi-polygon as Monique suggests

sum(polyd$sum_waterUse)/1000000 # sum of water use for outside the mapped irrigated area
sum(raster::area(polyd)/10000) # sum of area for outside the mapped irrigated area

############ Water use per region Tukituki, TANK, Mohaka, Wairoa, Porangahau #############



############### Irrigation mapping for Twyford (separate) #########################

Volumes1920 = read_csv("N:/Gorden_Jiang/Irrigation Analysis_Tim Norris/Volumes1920.csv")

Twyford_volume = Volumes1920 %>% 
  filter(str_detect(`Site Name`, "140429") == T) %>% 
  mutate(waterUse = 
           rowSums(
             data.frame(as.numeric(`Compliance Volume`), as.numeric(`Water Meter`)), 
             na.rm = T))

Twyford_Global_Consent = read_excel("N:/Gorden_Jiang/Irrigation Analysis_Tim Norris/Twyford Global Consent Telemetry Aduit 2020-2021.xlsx", 
                                    sheet = "2019-2020")[1:58,]

Bores = Twyford_volume %>% 
  rename(`Meter Name` = `Site Name`) %>% 
  left_join(Twyford_Global_Consent, by = "Meter Name")

Twyford_Global_Consent$`Meter Name`[!(unique(Twyford_Global_Consent$`Meter Name`) %in% unique(Twyford_volume$`Site Name`) ) ]
# "140429M6"  "140429M27" "140429M44" "140429M47" "140429M49" "140429M59"

Bores = st_as_sf(Bores, coords = c("Easting","Northing"), crs = 27200)
