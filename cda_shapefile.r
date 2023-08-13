
setwd("~/Desktop")
library(sf)
library(tidyverse)
library(Hmisc)
library(viridis)
library(data.table)
library(xml2)
library(httr)


# Download and unzip CDA shapefile
# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000b21a_e.zip

# Read in shapefile with sf package
cda <- st_read("lda_000b21a_e/lda_000b21a_e.shp")

# Download local area boundary file from Vancouver

# Read in shapefile with sf package
van <- st_read("~/Desktop/local-area-boundary/local-area-boundary.shp")

# Convert CRS to the same as cda
van <- st_transform(van, st_crs(cda))

# Crop CDA to include only Vancouver
cda_van <- st_crop(cda, st_bbox(van))

saveRDS(cda_van, "~/Library/Mobile Documents/com~apple~CloudDocs/cda_van_sf.rds")




intersect_pct <- st_intersection(van, cda_van) %>%
  mutate(intersect_area = st_area(.)) %>%
  tibble() %>%
  dplyr::select(name, DAUID, intersect_area, -geometry) 

# See which DAUIDs are duplicated. This will be dissemination areas
# that span across 2 or more Vancouver neighbourhoods


duped_DAUIDs <- unique(intersect_pct$DAUID[duplicated(intersect_pct$DAUID)])

# There is some CDAs that overlap the borders of some of the Vancouver neighbourhoods.
# The majority of these overlaps appear to be very small. This is something we can
# confirm.
ggplot(cda_van[cda_van$DAUID %in% duped_DAUIDs, ]) +
  geom_sf(data = van, colour = "red", alpha = 0, size = 2) +
  geom_sf(alpha = 0.5, fill = "orange") +
  theme(legend.position = "none")
  


# To get a sense of how much the CDAs overlap across different Vancouver neighbourhoods, we'll
# get the areas of all dissemination areas, and then divide by the CDA-Vancouver neighbourhood
# intersections.

# First, get the total area of each CDA
cda_van <- mutate(cda_van, DAUID_area = st_area(cda_van))

# Next, merge in the intersections
nc <- merge(cda_van, intersect_pct, by = "DAUID", all.x = TRUE)

nc <- left_join(cda_van, intersect_pct)


# An intial glance at the data shows that there are a lot of NA values for 
# the neighbourhood names. When we plot these intersections that have NA values, 
# we can see that these are CDAs that were caught within the bounding box
# of Vancouver, but don't actually belong to Vancouver (i.e. CDAs at UBC; and 
# in North Van, Richmond, and Burnaby)
ggplot(nc[is.na(nc$name), ]) +
  geom_sf(data = van, colour = "red", alpha = 0, size = 2) +
  geom_sf(alpha = 0.5, fill = "orange") +
  theme(legend.position = "none")


# With this in mind, we can remove those areas.
nc <- nc[!is.na(nc$name), ]


# Now we'll take a look at how much each CDA overlaps each Vancouver neighbourhood

nc$overlap <- as.numeric(nc$intersect_area/nc$DAUID_area)

total_overlap <- sum(nc$overlap >= 1)/length(nc$overlap)

# By looking at the intersections between the CDAs and Vancouver neighbourhoods,
# we find that 57% of all CDAs are completely nested within a Vancouver neighbourhood.
# This does not seem ideal, so let's dig deeper.

# First, we'll cut our `overlap` variable into 10% buckets:
nc$overlap_buckets <- Hmisc::cut2(nc$overlap, cuts = seq(0, 1, 0.1))

# Next, we'll count how many overlaps are in each 10% bucket:
count(nc, overlap_buckets) %>%
  mutate(proportion = n/sum(n)) %>%
  ggplot(aes(overlap_buckets, proportion)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 1)) +
  geom_text(aes(label = glue("{round(proportion*100)}%\n(N = {n})")),
            vjust = 0)

# So three quarters of our intersections have an overlap of 90% or more. While
# 20% have an overlap of 10% or less. All things considered, this is a number we
# can live with. 


nc <- nc %>%
  mutate(overlap_cat = case_when(overlap_buckets %in% "[0.9,1.0]" ~ "90% or more",
                                 overlap_buckets %in% "[0.0,0.1)" ~ "Less than 10%",
                                 TRUE ~ "10% to 90%")) 



ggplot(nc, aes(fill = overlap_cat)) +
  geom_sf(data = van, colour = "red", alpha = 0, size = 2, fill = NA) +
  geom_sf(alpha = 0.5) +
  theme(legend.position = "bottom")

# Indeed, though the CDAs don't line up directly with the Vancouver Neighbourhoods,
# it appears that the vast majority CDA-neighbourhood intersections that fall into 
# the "Less than 10%" are due to very minor overlaps. 

# For instance, let's look just at the CDAs that have at least some overlap with Kensington-Cedar Cottage:
nc %>%
  filter(name %in% "Kensington-Cedar Cottage") %>%
  ggplot(aes(fill = overlap_cat)) +
  geom_sf(alpha = 0.5) +
  geom_sf(data = van[van$name %in% "Kensington-Cedar Cottage", ], colour = "red", alpha = 0, size = 2, fill = NA) +
  theme(legend.position = "bottom")

# Most CDA-neighbourhood intersections are completely contained within Kensington-Cedar Cottage. 
# There are quite a few "Less than 10%" intersections where the borders don't line up perfectly,
# but we can say with some confidence that these are almost entirely not in Kensington-Cedar Cottage.
# There are a handful of cases where CDAs overlap significantly between two neighbourhoods, and
# this is something we'll have to live with. 


# With all this in mind, we can now associate each CDA with the Vancouver Neighbourhood it belongs to
# the most:


# Create some colours to map to the neighbourhoods:

# Randomization
viridis::plasma()

set.seed(1234)
neighbourhood_colours <- sample(c(viridis(9),
                                  inferno(9),
                                  plasma(9)),
                                length(unique(nc$name)),
                                replace = FALSE)


neighbourhood_colours <- setNames(neighbourhood_colours,
                                  unique(nc$name))

nc <- nc %>%
  group_by(DAUID) %>%
  slice_max(overlap) %>%
  ungroup()

nc$name_colour <- recode(nc$name, !!!neighbourhood_colours)

ggplot(nc, aes(fill = name_colour)) +
  geom_sf(alpha = 1) +
  geom_sf(data = van, colour = "red", alpha = 0, size = 1, fill = NA) +
  scale_fill_identity()

# There are a few CDAs on the periphery that are not within the boundaries of Vancouver.
# Three of these don't conern us:
# - The ones close to UBC (on the South-West of the map) are fine because areas are 
# mostly Pacific Spirit Park, where people do not live in (at least as far as the Census
# is concerned). 
# - Stanley Park (where people also do not live, Census-wise)

# Two are some areas that concern us, and will need to be removed:
# - A large chunk of Sea Island (YVR) that lightly touches Vancouver, and thus is included
# in the map. There *are* people who live here (in Burkeville) that are not Vancouverites,
# so let's remove this CDA.
# - Mitchell Island (Richmond) and a chunk of Bridgeport (also Richmond). These are mostly
# industrial areas, so few inhabitants, but let's remove.
# - A bunch of Burnaby CDAs along Boundary Road. 

nc %>%
  filter(name == "Sunset") %>%
  arrange(desc(DAUID_area)) %>%
  select(DAUID)


burnaby_cdas <- c("59150305", "59150314", "59153309", "59153310", 
                  "59153311", "59151350", "59151384", "59151232", 
                  "59151237", "59151312", "59153633")

sea_island_cdas <- "59153602"
mitchell_island_cdas <- "59151197"

nc %>%
  filter(!(DAUID %in% c(sea_island_cdas, # Sea Island
                        mitchell_island_cdas, # Mitchell Island/Bridgeport
                        burnaby_cdas))) %>%
  ggplot(aes(fill = name_colour)) +
  geom_sf(alpha = 1) +
  geom_sf(data = van, colour = "red", alpha = 0, size = 1, fill = NA) +
  scale_fill_identity()

vancouver_DGUIDs <- nc %>%
  filter(!(DAUID %in% c(sea_island_cdas, # Sea Island
                        mitchell_island_cdas, # Mitchell Island/Bridgeport
                        burnaby_cdas)))




# %%% Get the data for census dissemination areas %%% ----

# Now that we've mapped CDAs to Vancouver neighbourhoods, we can finally
# look at 2021 Census characteristics. This is a large (3.64) file because
# it has all census characteristics for all census divisions (CDs), 
# census subdivisions (CSDs) and dissemination areas (DAs) for all of BC.
# We just need the DA data for Vancouver. Luckily, we've identified
# Vancouver's DAs above and can narrow down the data to include just these.

# Run this only once because it takes so long
# df <- fread("98-401-X2021006_BC_CB_eng_CSV/98-401-X2021006_English_CSV_data_BritishColumbia.csv")
# # Some names are duplicated so we make them unique
# names(df) <- make.unique(names(df))
# # Just need DAs in Vancouver
# df <- df[df$GEO_LEVEL == "Dissemination area" & df$DGUID %in% vancouver_DGUIDs$DGUID, ]

saveRDS(df, "~/Library/Mobile Documents/com~apple~CloudDocs/vancouver_cda_census_profile_2021.rds")


  
df[df$CHARACTERISTIC_NAME == "Population density per square kilometre", ]


0 to 4 years
5 to 9 years
10 to 14 years
15 to 19 years
20 to 24 years
25 to 29 years
30 to 34 years
35 to 39 years
40 to 44 years
45 to 49 years
50 to 54 years
55 to 59 years
60 to 64 years
65 to 69 years
70 to 74 years
75 to 79 years
80 to 84 years
85 years and over
85 to 89 years
90 to 94 years
95 to 99 years
100 years and over
Average age of the population
Median age of the population


Single-detached house
Semi-detached house
Row house
Apartment or flat in a duplex
Apartment in a building that has fewer than five storeys
Apartment in a building that has five or more storeys
Other single-attached house
Movable dwelling
Total - Occupied private dwellings by structural type of dwelling - 100% data

Total - Private households by household size - 100% data
1 person
2 persons
3 persons
4 persons
5 or more persons

Total - Household type - 100% data
One-census-family households without additional persons
Couple-family households
With children
Without children
One-parent-family households
Multigenerational households
Multiple-census-family households
One-census-family households with additional persons
Two-or-more-person non-census-family households
One-person households

Total - Total income groups in 2020 for the population aged 15 years and over in private households - 100% data21	580,010
Without total income	16,285
With total income	563,730
Under $10,000 (including loss)	55,370
$10,000 to $19,999	61,180
$20,000 to $29,999	86,405
$30,000 to $39,999	67,875
$40,000 to $49,999	55,555
$50,000 to $59,999	46,055
$60,000 to $69,999	37,150
$70,000 to $79,999	30,195
$80,000 to $89,999	24,045
$90,000 to $99,999	19,545
$100,000 and over	80,345
$100,000 to $149,999	46,155
$150,000 and over	34,195

Total - After-tax income groups in 2020 for the population aged 15 years and over in private households - 100% data22	580,010
Without after-tax income	15,975
With after-tax income	564,035
Under $10,000 (including loss)	57,995
$10,000 to $19,999	63,245
$20,000 to $29,999	97,585
$30,000 to $39,999	77,280
$40,000 to $49,999	63,640
$50,000 to $59,999	50,160
$60,000 to $69,999	38,210
$70,000 to $79,999	29,320
$80,000 to $89,999	21,470
$90,000 to $99,999	14,965
$100,000 and over	50,165
$100,000 to $124,999	21,515
$125,000 and over	28,645

Total - Household total income groups in 2020 for private households - 100% data21	305,335
Under $5,000	7,260
$5,000 to $9,999	2,990
$10,000 to $14,999	4,025
$15,000 to $19,999	8,210
$20,000 to $24,999	14,595
$25,000 to $29,999	10,570
$30,000 to $34,999	10,515
$35,000 to $39,999	11,620
$40,000 to $44,999	10,450
$45,000 to $49,999	10,315
$50,000 to $59,999	20,760
$60,000 to $69,999	19,740
$70,000 to $79,999	18,775
$80,000 to $89,999	17,225
$90,000 to $99,999	15,485
$100,000 and over	122,810
$100,000 to $124,999	31,225
$125,000 to $149,999	23,270
$150,000 to $199,999	29,850
$200,000 and over	38,465
Total - Household after-tax income groups in 2020 for private households - 100% data22	305,335
Under $5,000	8,305
$5,000 to $9,999	3,055
$10,000 to $14,999	4,095
$15,000 to $19,999	8,565
$20,000 to $24,999	15,495
$25,000 to $29,999	12,065
$30,000 to $34,999	12,080
$35,000 to $39,999	13,130
$40,000 to $44,999	12,385
$45,000 to $49,999	12,390
$50,000 to $59,999	24,430
$60,000 to $69,999	22,815
$70,000 to $79,999	20,975
$80,000 to $89,999	18,315
$90,000 to $99,999	15,955
$100,000 and over	101,285
$100,000 to $124,999	31,545
$125,000 to $149,999	22,240
$150,000 and over	47,500



# ANNOYING CENSUS CHARACTERISTIC STRUCTURE ----





# URL to fetch the XML data of census characteristics
url <- "https://api.statcan.gc.ca/census-recensement/profile/sdmx/rest/dataflow/STC_CP/DF_DA?references=all"

# Make an HTTP request to fetch the XML data
response <- GET(url)





# Print the nested list
print(nested_list)

library(listviewer)


listviewer::jsonedit(nested_list)




# ORPHANAGE

DGUID_to_VAN <- vancouver_DGUIDs %>%
  tibble() %>%
  select(DGUID, name)

df <- merge(df, DGUID_to_VAN, by = "DGUID")

dfpop <- df[df$CHARACTERISTIC_NAME == "Population, 2021", ]

dfpop[ , .(group_sum = sum(C1_COUNT_TOTAL)), by = name]

# Get 2016 data
df16 <- fread("98-401-X2016044_BRITISH_COLUMBIA_eng_CSV (1)/98-401-X2016044_BRITISH_COLUMBIA_English_CSV_data.csv")

names(df16) <- make.unique(names(df16))
df1 <- df16[df16$GEO_LEVEL == 4 & df16$`GEO_CODE (POR)` %in% gsub("2021S0512", "", vancouver_DGUIDs$DGUID), ]

df1 <- rename(df1, 
              DGUID = `GEO_CODE (POR)`,
              C1_COUNT_TOTAL = `Dim: Sex (3): Member ID: [1]: Total - Sex`,
              CHARACTERISTIC_NAME = `DIM: Profile of Dissemination Areas (2247)`) %>%
  mutate(C1_COUNT_TOTAL = as.numeric(C1_COUNT_TOTAL))


df16pop <- DGUID_to_VAN %>%
  mutate(DGUID = as.numeric(gsub("2021S0512", "", DGUID))) %>%
  merge(df1, ., by = "DGUID") %>%
  filter(CHARACTERISTIC_NAME == "Population, 2016")

head(df16pop$CHARACTERISTIC_NAME)
unique(df1$CHARACTERISTIC_NAME) %>%
  grep("dwell", ., value = TRUE)

df16pop[ , .(group_sum = sum(C1_COUNT_TOTAL)), by = name]

df16dwellings <- DGUID_to_VAN %>%
  mutate(DGUID = as.numeric(gsub("2021S0512", "", DGUID))) %>%
  merge(df1, ., by = "DGUID") %>%
  filter(CHARACTERISTIC_NAME == "Total private dwellings")

df16dwellings[ , .(group_sum = sum(C1_COUNT_TOTAL)), ]

