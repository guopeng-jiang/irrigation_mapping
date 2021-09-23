library(readxl)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(tm)

# irrigation wateruse
WaterUse <- read_excel("N:/Water Information/Queries/WaterUseSummaryReport2.xlsx", 
                       sheet = "WaterUseDetail") %>% 
  filter(ConsentID != "WP123456T") # a test consent

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

# anything but irrigation for primary use from local councils. 
# WaterUse19_20 

# consent layer
Consent = st_read(dsn = "N:/Gorden_Jiang/temp/Tukituki FEMPSubmissions2018.gdb",
                  layer = "trythis_allresourceconsentspolygons")
Consent = subset(Consent, Consent$AuthorisationActivityType == "Water Permit") %>% 
  select(AuthorisationIRISID, AuthorisationHistoricID, AuthorisationActivityType, AuthorisationHolderContactName)

# irrigation landuse layer
Irrigation = st_read(dsn = "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris", layer = "Irrigation_Layer_landuse")
HBboundary = st_read(dsn = "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris", layer = "HBboundary")
HBIrrigation <- Irrigation[HBboundary,] 

# join irrigation wateruse to consent layer
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
# join consent information from point layer to the target parcel, then remove the parcels with no match in the consent.

polyb = st_as_sf(polyb) %>% st_set_crs(2193)

# test1 = st_read(dsn = "N:/Gorden_Jiang/GISTeam/210921/PropertyTitlesIncludingOwners.gdb", layer = "HawkesBay_Property_Titles_Including_Owners_210921") # from Tim Farrier's export

Consent_wateruse_sply3 = polyb %>% 
  st_join(Consent_wateruse_spt_filled, left = T) %>% 
  filter(!is.na(ResourceConsentID)) %>% 
  mutate(Source = "Consent points") %>% 
  rename(SHAPE = geometry) %>% 
  select(-owners)

############ merge existing consent polygons ###############

Consent_wateruse_sply = rbind(Consent_wateruse_sply1, Consent_wateruse_sply2, Consent_wateruse_sply3)

HBIrrigation_waterUse = HBIrrigation %>% 
  st_join(Consent_wateruse_sply, left = T) %>% 
  select(-Shape_Leng, -Shape_Area)
# write.csv(HBIrrigation_waterUse, "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris/HBIrrigation_waterUse.csv")

# check the missing IDs
ID_missed = WaterUse19_20$ResourceConsentID[!(WaterUse19_20$ResourceConsentID %in% HBIrrigation_waterUse$ResourceConsentID)]
# write.csv(ID_missed, "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris/ID_missed.csv")

Consent_wateruse_sply_IDmissed = Consent_wateruse_sply %>% filter(ResourceConsentID %in% ID_missed)
Consent_wateruse_spt_filled_IDmissed = Consent_wateruse_spt_filled %>% filter(ResourceConsentID %in% ID_missed)

mapview::mapviewOptions(fgb = FALSE)
mp = mapview::mapview(HBIrrigation_waterUse, zcol = "sum_waterUse", layer.name = "Sum water use", 
                 col.regions = function (n, alpha = 1, begin = 0, end = 1, direction = 1) {
                   colors <- RColorBrewer::brewer.pal(9, "YlOrRd")
                   if (direction < 0) colors <- rev(colors)
                   colorRampPalette(colors, alpha = alpha)(n)
                   }, at = c(0, 20000, 50000, 150000, 12000000)
)

mapview::mapshot(mp, url = "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris/HBIrrigation_waterUse.html")

# st_write(Consent_wateruse19_20, "N:/Gorden_Jiang/Irrigation Analysis_Tim Norris/Consent_wateruse19_20.shp")

# summary of hectares based on landuse
HBIrrigation %>% 
  group_by(Landuse) %>% 
  summarise(Hectare = sum(area_ha))

# summary of wateruse based on landuse
HBIrrigation_waterUse  %>% 
  group_by(Landuse) %>% 
  summarise(sum_waterUse = sum(sum_waterUse))



