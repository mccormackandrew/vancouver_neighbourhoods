Untitled
================

# Grouping dissemination areas into Vancouver neighbourhoods

## Why even do this?

Good question.

I want to find out about different characteristics of Vancouver’s
neighbourhoods with data from the [2021 Canadian
census](https://www12.statcan.gc.ca/census-recensement/index-eng.cfm).

The problem is that there is no “Vancouver Neighbourhoods” option in the
StatCan census profile data. However, I can access census profile
characteristics for each census dissemination area (DA), which are much
smaller than neighbourhoods. So **my goal here is to find out which DAs
belong to which Vancouver neighbourhoods, group** **these DAs together,
and aggregate census profile characteristics to the Vancouver**
**neighbourhood level.**

The City of Vancouver has very comprehensive and nicely designed
neighbourhood profiles (like [this
one](https://vancouver.ca/files/cov/social-indicators-profile-killarney.pdf)
for Killarney), but I can’t find any that are updated with new data from
2021 census. My assumption is that in order to get these data (for
2016), someone at City Hall either (a) had access to StatCan microdata
and was able to create custom geographies for Vancouver’s
neighbourhoods, or (b) they’re grouping census dissemination areas into
neighbourhoods. Realtors also have a lot of census information up their
sleeves, but this seems to typically be at the DA level, and I’m afraid
to contact them because then they might try to sell me a house. In any
case, all I have access to is census dissemination areas.

As we’ll see below the *vast majority* of DAs in the Vancouver area
neatly align with Vancouver’s neighbourhoods without overlapping,
allowing us to accurately estimate neighbourhood-level data
(e.g. population) by summing up the tract-level information.

Let’s get started.

## Loading packages

``` r
library(sf)
library(tidyverse)
library(Hmisc)
library(viridis)
library(data.table)
library(xml2)
library(httr)
library(glue)
library(ggtext)
library(kableExtra)

maptheme <- theme_void() +
  theme(text = element_text(family = "IBM Plex Sans"),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(face = "bold"))
```

## Reading in shapefiles

We’ll need a shapefile of all the census dissemination areas, as well as
a shapefile of Vancouver’s neighbourhoods, The former I’ve downloaded
from
[StatCan](https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21)
and the latter I’ve downloaded from the [City of Vancouver’s Open Data
Portal](https://opendata.vancouver.ca/explore/dataset/local-area-boundary/information/?disjunctive.name).

The DA shapefile is pretty big, so off the bat I narrow it down to only
include the bounding box of Vancouver (based off of the Vancouver
shapefile).

``` r
# Read in DA shapefile
cda <- st_read("lda_000b21a_e/lda_000b21a_e.shp")

# Read in YVR neighbourhoods shapefile
van <- st_read("local-area-boundary/local-area-boundary.shp")

# Convert YVR shafefile CRS to be the same as the cda CRS
van <- st_transform(van, st_crs(cda))

# Crop DA shapefile to include be within the borders of Vancouver
cda_van <- st_crop(cda, st_bbox(van))
```

Reading layer
`local-area-boundary' from data source`/Users/andrewmccormack/Library/Mobile
Documents/com<sub>apple</sub>CloudDocs/vandash_data/local-area-boundary/local-area-boundary.shp’
using driver \`ESRI Shapefile’ Simple feature collection with 22
features and 1 field Geometry type: POLYGON Dimension: XY Bounding box:
xmin: -123.2248 ymin: 49.19894 xmax: -123.0232 ymax: 49.29581 Geodetic
CRS: WGS 84

Let’s overlay both maps on top of one another take a quick look at what
we’re working with:

``` r
ggplot(cda_van) +
  geom_sf(aes(colour = "Census dissemination areas"),
          fill = "#f5f4f2") +
  geom_sf(data = van,
          aes(colour = "Vancouver neighbourhoods"),
          fill = NA,
          linewidth = 1) +
  scale_colour_manual(values = c("#D95E32", "black")) +
  ggtitle(str_wrap("Vancouver neighbourhoods overlayed on census dissemination areas", 40)) +
  maptheme
```

Discerning readers will immediately recognize this as Vancouver. The red
lines are Vancouver neighbourhoods, while the black lines mark the
borders of DAs.

It’s hard to tell at this point, but we can already tell we’ll be able
to come up with decent DA-to-neighbourhood groupings.

## Determining DA-Vancouver neighbourhood overlap

Next we’ll get the intersections of the DAs and Vancouver
neighbourhoods. That is we’ll find out which Vancouver neighbourhood(s)
each DA overlaps with. We’ll also calculate the area of each
intersection.

``` r
cda_intersections <- st_intersection(van, cda_van) %>%
  mutate(intersect_area = st_area(.)) %>%
  tibble() %>%
  dplyr::select(name, DAUID, intersect_area, -geometry)

# Take a quick look
cda_intersections %>%
  arrange(desc(DAUID)) %>%
  head() %>%
  mutate(intersect_area = round(intersect_area)) %>%
  kable(format = "markdown")
```

| name                     | DAUID    | intersect_area |
|:-------------------------|:---------|---------------:|
| Kensington-Cedar Cottage | 59154196 |     27 \[m^2\] |
| Renfrew-Collingwood      | 59154196 | 143291 \[m^2\] |
| Killarney                | 59154195 | 120619 \[m^2\] |
| Renfrew-Collingwood      | 59154195 |   2943 \[m^2\] |
| Victoria-Fraserview      | 59154195 |    427 \[m^2\] |
| Killarney                | 59154194 |    173 \[m^2\] |

Looking at a few rows of our intersections, we can see CD `59154196` (a
DA I’m sure we’re all familiar with) is mostly in Renfrew-Collignwood,
but not all of it — 27 square meters are in Kensington Cedar Cottage.
Nonetheless, based on the fact that 99.98% of CD `59154196` is in
Renfrew-Collingwood, we’ll go out on a limb and call this a
Renfrew-Collingwood CD.

Let’s apply this analysis more broadly and get the areas of all the DAs,
then divide each DA-Vancouver neighbourhood intersections by the DA’s
total area.

``` r
cda_intersections <- cda_intersections %>%
  mutate(prop_of_da = intersect_area/sum(intersect_area),
         .by = DAUID)

# Named vector to rename 1, 2, 3 into 0-10%, 10-20%, 20-30%, etc.
bucket_labels <- setNames(paste0(seq(0, 0.9, 0.1) * 100, "-", 
                seq(0, 0.9, 0.1) * 100 + 10, "%"),
                1:10)

# Create some labels for the intervals

# Group into 10% buckets
cda_intersections <- cda_intersections %>%
  mutate(prop_of_da_buckets = cut(prop_of_da, 
                                  breaks = seq(0, 1, 0.1), 
                                  include.lowest = TRUE))
```

``` r
cda_intersections %>%
  count(prop_of_da_buckets) %>%
  mutate(proportion = n/sum(n)) %>%
  ggplot(aes(prop_of_da_buckets, proportion)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 1)) +
  geom_richtext(aes(label = glue("**{round(proportion*100)}%**<br>(N = {n})")),
                family = "IBM Plex Sans",
                vjust = 0, fill = NA, label.color = NA) +
  theme_linedraw() +
  labs(x = "\nProportion in Vancouver neighbourhood",
       y = "Frequency",
       title = "Distribution of DA-neighbourhood intersections in neighbourhoods") +
  theme(text = element_text(family = "IBM Plex Sans"),
          axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold"))
```

As we can see, for 75% of the DA-neighbourhood intersections, 90% or
more of the area of those intersections belong to one neighbourhood. In
most cases, I think this is due to the fact that the Vancouver Shapefile
and Census DA Shapefile don’t line up *exactly* after transforming the
CRS, which results in tiny little overaps.

To illlustrate further, let’s look at CDs that span more than one
neighbourhood. From the map below, it looks like most (but not all) of
the overlap is right along the border of the neighbourhoods:

``` r
# See which DAUIDs are duplicated. This will be dissemination areas
# that span across 2 or more Vancouver neighbourhoods
duped_DAUIDs <- unique(cda_intersections$DAUID[duplicated(cda_intersections$DAUID)])

# There is some DAs that overlap the borders of some of the Vancouver neighbourhoods.
# The majority of these overlaps appear to be very small. This is something we can
# confirm.
ggplot(cda_van[cda_van$DAUID %in% duped_DAUIDs, ]) +
  geom_sf(data = van, 
          aes(colour = "Vancouver neighbourhoods"),
          fill = NA, 
          linewidth = 1.1) +
  geom_sf(aes(colour = "Census dissemination areas"),
          alpha = 0.5, fill = NA, linewidth = 0.5) +
  scale_colour_manual(values = c("#D95E32", "black")) +
  theme(legend.position = "none") +
  ggtitle(str_wrap("DAs that overlap multiple neighbourhoods", 40)) +
  theme_void() +
  maptheme
```

### Removing non-Vancouver DAs

If we merge together our DA-neighbourhood intersections with our
original DA data, we’ll find there are some `NA` values for the
neighbourhoods:

``` r
# Next, merge in the intersections
cda_intersections <- left_join(cda_van, 
                                      cda_intersections,
                                      by = "DAUID")


kable(head(cda_intersections), format = "markdown")
```

| DAUID    | DGUID             | LANDAREA | PRUID | name | intersect_area | prop_of_da | prop_of_da_buckets | geometry                     |
|:---------|:------------------|---------:|:------|:-----|---------------:|-----------:|:-------------------|:-----------------------------|
| 59150071 | 2021S051259150071 |   1.5073 | 59    | NA   |     NA \[m^2\] |   NA \[1\] | NA                 | POLYGON ((4022065 2010531, … |
| 59150073 | 2021S051259150073 |   0.2620 | 59    | NA   |     NA \[m^2\] |   NA \[1\] | NA                 | POLYGON ((4022672 2010487, … |
| 59150074 | 2021S051259150074 |   0.0658 | 59    | NA   |     NA \[m^2\] |   NA \[1\] | NA                 | POLYGON ((4022698 2010531, … |
| 59150076 | 2021S051259150076 |   0.3286 | 59    | NA   |     NA \[m^2\] |   NA \[1\] | NA                 | POLYGON ((4022977 2010531, … |
| 59150077 | 2021S051259150077 |   0.1138 | 59    | NA   |     NA \[m^2\] |   NA \[1\] | NA                 | POLYGON ((4023360 2010531, … |
| 59150078 | 2021S051259150078 |   0.4834 | 59    | NA   |     NA \[m^2\] |   NA \[1\] | NA                 | POLYGON ((4023684 2010371, … |

When we plot these intersections that have NA values, we can see that
these are CDAs that were caught within the bounding box of Vancouver,
but don’t actually belong to Vancouver (i.e. CDAs at UBC; and in North
Van, Richmond, and Burnaby).

``` r
ggplot(cda_intersections[is.na(cda_intersections$name), ]) +
  geom_sf(data = van, 
          aes(colour = "Vancouver neighbourhoods"),
          fill = NA, 
          linewidth = 1) +
  geom_sf(aes(colour = str_wrap("Census dissemination areas that are not actually in Vancouver", 30)),
          alpha = 0.5, fill = NA, linewidth = 0.5) +
  scale_colour_manual(values = c("#53777A", "black")) +
  theme(legend.position = "none") +
  ggtitle(str_wrap("DAs that are not in Vancouver", 40)) +
  theme_void() +
  maptheme
```

We can remove these non-Vancouver DAs.

``` r
cda_intersections <- cda_intersections[!is.na(cda_intersections$name), ]
```

### Associating each DA with a Vancouver neighbourhood

At this point, we can now associate each CDA with the Vancouver
Neighbourhood it belongs to the most:

``` r
cda_intersections <- cda_intersections %>%
  group_by(DAUID) %>%
  slice_max(prop_of_da) %>%
  ungroup()
```

Now that we’ve associated each DA with one neighbourhood, we can see if
the neighbourhoods we’ve cobbled together from the DAs look anything
like the actual Vancouver neighbourhoods.

Because there are so many neighbourhoods, I have to get creative with
the colours here:

``` r
neighbourhood_colours <- sample(c(viridis(9),
                                  inferno(9),
                                  plasma(9)),
                                length(unique(cda_intersections$name)),
                                replace = FALSE)

neighbourhood_colours <- setNames(neighbourhood_colours,
                                  unique(cda_intersections$name))

cda_intersections$name_colour <- recode(cda_intersections$name, !!!neighbourhood_colours)

ggplot(cda_intersections, aes(fill = name_colour)) +
  geom_sf(alpha = 1) +
  geom_sf(data = van, colour = "red", alpha = 0, linewidth = 1, fill = NA) +
  scale_fill_identity() +
  ggtitle(str_wrap("Vancouver neighbourhoods constructed from DAs", 40)) +
  maptheme
```

Not perfect, but looks pretty good overall!

There are a few CDAs on the periphery that are not within the boundaries
of Vancouver. Three of these don’t conern us: - The ones close to UBC
(on the South-West of the map) are fine because areas are mostly Pacific
Spirit Park, where people do not live in (at least as far as the Census
is concerned). - Stanley Park (where people also do not live,
Census-wise)

There are some areas that concern us, and will need to be removed: - A
large chunk of Sea Island (YVR) that lightly touches Vancouver, and thus
is included in the map. There *are* people who live here (in Burkeville)
that are not Vancouverites, so let’s remove this CDA. - Mitchell Island
(Richmond) and a chunk of Bridgeport (also Richmond). These are mostly
industrial areas, so few inhabitants, but let’s remove. - A bunch of
Burnaby CDAs along Boundary Road.

``` r
# Manually identify
burnaby_cdas <- c("59150305", "59150314", "59153309", "59153310", 
                  "59153311", "59151350", "59151384", "59151232", 
                  "59151237", "59151312", "59153633")

sea_island_cdas <- "59153602"
mitchell_island_cdas <- "59151197"

cda_intersections %>%
  filter(!(DAUID %in% c(sea_island_cdas, # Sea Island
                        mitchell_island_cdas, # Mitchell Island/Bridgeport
                        burnaby_cdas))) %>%
  ggplot(aes(fill = name_colour)) +
  geom_sf(alpha = 1) +
  ggtitle(str_wrap("Vancouver neighbourhoods constructed from DAs", 40)) +
  labs(subtitle = "Non-Vancouver periphery DAs removed") +
  geom_sf(data = van, colour = "red", alpha = 0, size = 1, fill = NA) +
  scale_fill_identity() +
  maptheme
```

### Saving our DA to Vancouver neighbourhood mapping

Now that we’re more or less satisfied with how we’ve mapped the DAs to
Vancouver neighbourhoods, we can save this mapping and then use it to
aggregate the DA Census data.

``` r
da_to_neighbourhood <- cda_intersections %>%
  tibble() %>%
  select(DAUID, DGUID, name, -geometry)

kable(head(da_to_neighbourhood), format = "markdown")
```

| DAUID    | DGUID             | name             |
|:---------|:------------------|:-----------------|
| 59150305 | 2021S051259150305 | Hastings-Sunrise |
| 59150307 | 2021S051259150307 | Hastings-Sunrise |
| 59150308 | 2021S051259150308 | Hastings-Sunrise |
| 59150309 | 2021S051259150309 | Hastings-Sunrise |
| 59150310 | 2021S051259150310 | Hastings-Sunrise |
| 59150311 | 2021S051259150311 | Hastings-Sunrise |

``` r
saveRDS(da_to_neighbourhood, "da_to_neighbourhood.rds")
```
