#############################################################################
#
#  11_dashboard_dataset.R
#
# Purpose: 
# Merge all datasets created (excluding similarity) into one master dataset, making
# the deliverables easier to produce. 
#
# Outputs:
# - county_final.rds
#
#############################################################################

library(tidyverse)
library(sf)
library(arrow)

#############################################################################
# Download All Datasets
#############################################################################

county_pca <- readRDS("data/finished/county_pca.rds") # contains all the same columns from county_dataset.rds, alongside PCs. it will be used as the baseline
county_pca_clusters <- readRDS("data/finished/county_pca_clusters.rds")
county_hierarchical_clusters <- readRDS("data/finished/county_hierarchical_clusters.rds")
county_gmm_clusters <- readRDS("data/finished/county_gmm_clusters.rds")
county_uniqueness <- readRDS("data/finished/county_uniqueness.rds")

#############################################################################
# Create Final Dataset 
#############################################################################

county_master_dataset <- county_pca |> 
  left_join(
    county_pca_clusters |> st_drop_geometry() |> 
      select(GEOID, pca_cluster = cluster13),
    by = "GEOID"
  ) |> 
  left_join(
    county_hierarchical_clusters |> st_drop_geometry() |> 
    select(GEOID, hc_cluster = cluster13),
    by = "GEOID"
  ) |> 
  left_join(
    county_gmm_clusters |> st_drop_geometry() |> 
      select(GEOID, gmm_cluster, gmm_uncertainty),
    by = "GEOID"
  ) |> 
  left_join(
    county_uniqueness |> select(-gmm_uncertainty),
    by = "GEOID"
  )

county_master_dataset <- county_master_dataset |> 
  mutate(pca_cluster_name = case_when(
    pca_cluster == 1 ~ "Frigid Retirement Communities",
    pca_cluster == 2 ~ "Rural Appalachia / Lower Midwest",
    pca_cluster == 3 ~ "Stable Urban Megacities",
    pca_cluster == 4 ~ "Extraction Counties",
    pca_cluster == 5 ~ "Northern Rural America",
    pca_cluster == 6 ~ "Midwest Suburbia",
    pca_cluster == 7 ~ "Western Highlands",
    pca_cluster == 8 ~ "East Coast Aging Counties",
    pca_cluster == 9 ~ "Sun Belt Metros",
    pca_cluster == 10 ~ "Underserved Southern Communities",
    pca_cluster == 11 ~ "Industrial Belt Cities",
    pca_cluster == 12 ~ "Border Counties",
    pca_cluster == 13 ~ "Growing Urban Centers",
    TRUE ~ NA_character_
  ),
  hc_cluster_name = case_when(
    hc_cluster == 1 ~ "Growing Affluent Suburbs",
    hc_cluster == 2 ~ "Mid Sized Metro Areas",
    hc_cluster == 3 ~ "Underserved Southern Communities",
    hc_cluster == 4 ~ "Rural Mid-east America",
    hc_cluster == 5 ~ "Urban Cores",
    hc_cluster == 6 ~ "Highland Regional Hubs",
    hc_cluster == 7 ~ "Established Older Suburbs",
    hc_cluster == 8 ~ "Resource Rich Economies",
    hc_cluster == 9 ~ "Midwestern Agricultural Areas",
    hc_cluster == 10 ~ "Frigid Retirement Communities",
    hc_cluster == 11 ~ "The Great Frontier",
    hc_cluster == 12 ~ "Rapidly Growing Border Counties",
    hc_cluster == 13 ~ "Downtown New York",
    TRUE ~ NA_character_
  ),

gmm_cluster_name = case_when(
  gmm_cluster == 1 ~ "Southern Working-Class America",
  gmm_cluster == 2 ~ "Affluent Coastal and Rugged Counties",
  gmm_cluster == 3 ~ "Older Industrial Heartland Counties",
  gmm_cluster == 4 ~ "Growing East / Midwest Suburban Hubs",
  gmm_cluster == 5 ~ "Appalachian Rural America",
  gmm_cluster == 6 ~ "Historically Black Southern Counties",
  gmm_cluster == 7 ~ "Upper Midwest Agricultural Centers",
  gmm_cluster == 8 ~ "Mid Sized Metro Suburbs",
  gmm_cluster == 9 ~ "Western Rural Recreation Areas",
  gmm_cluster == 10 ~ "Global Urban Centers",
  gmm_cluster == 11 ~ "Extraction Plains Counties",
  gmm_cluster == 12 ~ "Midwest Small Towns",
  gmm_cluster == 13 ~ "Rich Urban Centers",
  gmm_cluster == 14 ~ "Retirement Suburbs",
  TRUE ~ NA_character_
))


saveRDS(county_master_dataset, "data/final/county_master_dataset.rds")

#############################################################################
# Adding Descriptions of Clusters to a Dataset
#############################################################################

pca_profiles <- tibble(
  cluster = 1:13,
  cluster_name = c(
    "Frigid Retirement Communities",
    "Rural Appalachia / Lower Midwest",
    "Stable Urban Megacities",
    "Extraction Counties",
    "Northern Rural America",
    "Midwest Suburbia",
    "Western Highlands",
    "East Coast Aging Counties",
    "Sun Belt Metros",
    "Underserved Southern Communities",
    "Industrial Belt Cities",
    "Border Counties",
    "Growing Urban Centers"
  ),
  description = c(
    "By far, the counties in this cluster are the oldest, with over 28% of the population on average being over 65. These counties are among the coldest in the country, with a few outliers in California and Arizona. These counties have a lot of veterans, not a lot of immigrants, are typically much more rural, typically vote republican, and have a lot of forests.", 
    "These counties, almost entirely located in the Appalachia region into states like Missouri and Arkansas, are extremely rural in nature. This cluster has the largest number of counties (605), is not very diverse, and has low diversity and income, with a higher poverty rate and reliance on SNAP. It has the highest rate of construction workers, and overwhelmingly votes republican.", 
    "This cluster contains just 30 counties, mostly only urban centers. Its population is relatively stable, with minor population growth. Cities like San Francisco, New York, Chicago, and Boston all fit into this category. They’re extremely dense, and rely a lot on public transportation, and vote democratic in not very close elections.",
    "These counties, prevalent across Texas and Oklahoma as well as parts of the west and Plains, are unique because of their high level of workforce in agriculture, their lower population densities, high immigration, and overall high extraction based economies. These counties barely get any precipitation, vote overwhelmingly republican and have a low unemployment rate.",
    "These counties, almost all in the Great Plains region up towards the Canadian border, are extremely rural, with a population density average of just ~3.8. These counties are predominantly white, have lower than average poverty rates, and have low costs of living. They’re cold, don’t get a lot of rain, and are pretty flat.",
    "These counties, mostly in the upper midwest, stretch through most of the major cities in the region. Its biggest counties are direct suburbs of major cities in the area, while others are more rural. These counties are typically colder than usual, predominantly white with lower than average poverty rates. Like other more rural counties, they have a pretty low cost of living, and vote typically republican in uncompetitive elections.",
    "With little exceptions, these counties are predominantly counties with the Rocky Mountains in the backdrop. They’re more diverse than some of the other clusters, but still less diverse than the average, while they make more money than the average, and they have much more density than average. They have pretty competitive elections, typically favoring the democrats. These counties have high elevation, ruggedness, forest coverage, but not a lot of precipitation.",
    "These counties, almost all in Florida or across the east coast, have a high median age. These counties are pretty densely populated compared to the first cluster, and have a lower than average poverty rate. It rains a lot in these counties, and is pretty hot too. These counties are also usually covered in bodies of water.",
    "This group of metropolitan areas have found themselves in this cluster, predominantly being smaller metro areas or immediate suburbs in the southern half of the US. These counties are not as dense as the other metro clusters, but are pretty diverse and have a rising income. They’re hot, find themselves voting republican more often than not, and are flat.",
    "These counties, mostly in the south, obviously tend to be hotter and flatter, as well as more diverse, less dense, and poorer. An average county in this cluster has just under 24k people, and this population has been declining. These counties are typically much more rural, have very high income inequality and poverty. These counties also see a lot of rain compared to the average and have lots more forest. These counties are far and away the least internet-accessible counties.",
    "Another cluster of metropolitan areas finds us around the US in typically more industrial areas such as Summit County OH (with Akron), Wayne county, MI (with Detroit), and others. These cities across the midwest and other portions of the country are much less populated, poorer, and less diverse than the other metro counties that have been clustered. Their populations have stayed relatively stable thanks to a youthful population, and these counties have very competitive elections.",
    "These counties, spread near the Mexican border as well as near the Canadian border, are young counties whose diversity comes with a 10% immigrant population. These counties have the highest reliance on SNAP and poverty rate in the entire country, and the lowest income of any cluster. This comes with the highest unemployment rate as well. These counties get very little precipitation and their temperature varies depending on the geography of the county (which border it is close to).",
    "This cluster has a rapidly growing population, and contains cities like LA, Miami, and the immediate suburbs to many of the counties in Cluster Three. These counties sacrifice a bit of density, public transit usage, and immigrants in favor of a slightly less educated, slightly less impoverished population, and a lower cost of living. This cluster also has significantly more counties (193)."
  ),
  cluster_color = c(
    "#8E6BBE",  # Frigid Retirement Communities
    "#3A7D44",  # Rural Appalachia / Lower Midwest
    "#2F6CB3",  # Stable Urban Megacities
    "#B5651D",  # Extraction Counties
    "#6BA368",  # Northern Rural America
    "#F4C542",  # Midwest Suburbia
    "#8C5A2B",  # Western Highlands
    "#A07CC5",  # East Coast Aging Counties
    "#4F9DED",  # Sun Belt Metros
    "#E67E22",  # Underserved Southern Communities
    "#7F8C8D",  # Industrial Belt Cities
    "#C0392B",  # Border Counties
    "#5DADE2"   # Growing Urban Centers
  ))
saveRDS(pca_profiles, "data/final/pca_cluster_types.rds")

hc_profiles <- tibble(
  cluster = 1:13,
  cluster_name = c(
    "Growing Affluent Suburbs",
    "Mid Sized Metro Areas",
    "Underserved Southern Communities",
    "Rural Mid-east America",
    "Urban Cores",
    "Highland Regional Hubs",
    "Established Older Suburbs",
    "Resource Rich Economies",
    "Midwestern Agricultural Areas",
    "Frigid Retirement Communities",
    "The Great Frontier",
    "Rapidly Growing Border Counties",
    "Downtown New York"
  ),
  description = c(
    "These counties are large population suburban counties of major metropolitan areas such as the suburbs of Dallas-Fort Worth, Minneapolis, Chicago, and Columbus. These counties are all over the central and eastern US, and are typically wealthy suburban counties. Their residents are more likely to be college graduates than the average American, and these counties have some of the lowest income inequality in the country. They have somewhat competitive elections but typically vote republican, and have around average diversity.",
    "These counties, with counties like Knox County, TN, Wayne County, MI, and Maricopa County, AZ, are mid to large sized metropolitan areas. They differ from the two other metro clusters in that they are not as dense or diverse. This diversity comes from their lack of immigrants. With these areas, these counties are quite similar to the ones in cluster one, except with more density and diversity. Alongside that, they are on average a bit less wealthy and a bit more impoverished, vote for the democratic party a bit more in more competitive elections.",
    "These counties, mostly in the south, obviously tend to be hotter and flatter, as well as more diverse, less dense, and poorer. An average county in this cluster has just under 25k people, and this population has been declining. These counties are typically much more rural, have very high income inequality and poverty. These counties also see a lot of rain compared to the average and have lots more forest. These counties are far and away the least internet-accessible counties.",
    "With lots of counties across Appalachia and the south, this stretch of counties tends to be poorer, less diverse, elevated, rugged, and forested. Like cluster three, the residents in these counties are more impoverished than usual, and there is high income inequality. Residents of this county typically vote Republican.",
    "This cluster features nearly every major city not in cluster 2 except for New York, which remarkably got its own cluster. Counties in this cluster, such as LA County, Cook County, and Harris County, are extremely densely populated with young diverse populations and typically make a lot of money. Cost of living is way higher in these counties, and they have a large proportion of college graduates. These counties typically vote democratic.",
    "These counties, such as Salt Lake County, UT and Buncombe County, NC, are rugged counties that contain cities or communities that are hubs in their region, such as Salt Lake City or Asheville. These counties are less densely populated than even suburban counties, and are low on diversity. These counties are typically cooler, potentially due to most of them being in the northwest or in either of the mountain ranges. These counties have pretty close elections, have older populations, smaller households, and a decently low poverty rate.",
    "These counties are typically suburbs, mere miles away from metropolitan centers. Some counties, like Cuyahoga, contain cities not already in other clusters. These counties are similar to those from Cluster Two, except their residents are older, a bit more richer, and less impoverished, with slightly less competitive elections favoring the republicans.",
    "These agricultural centers in Texas, Oklahoma, and a bit further west, are interesting counties with unique characteristics. They’re younger than average, are incredibly sparsely populated, and have a lot of immigrants. They’re not as highly educated, and have around average poverty. They tend to get the least rain of any counties, and are typically highly elevated with little forests.",
    "These counties across the midwest and Plains regions of the US feature low diversity, colder, flatter land. These counties have high rates of workers in construction compared to the country. They have high labor participation and low unemployment, with a decently low cost of living.",
    "These counties, such as Montezuma County, CO, or St. Louis County, MN, are freezing counties across the US, ranging from the PNW to Rockies to Upper Midwest to Northeast, these counties are much older than average, feature low diversity, and are relatively wealthy. Unlike other suburban counties, these counties usually have rising populations. These counties feature the most veterans of any cluster as well. These counties don’t get a lot of precipitation, and when they do it’s usually snowfall. Elections in these counties typically favor republicans.",
    "These counties, mostly in the Great plains region, are extremely rural, with a population density of only 2.8, meaning there are 2.8 people per square mile. These counties are cold and not diverse at all, and are old, but not as old as the counties in cluster ten. These counties have low reliance on SNAP, and extremely low costs of living. They get very little precipitation, have almost no forests, and overwhelmingly vote republican.",
    "These counties, across both the Canadian and Mexican borders, have youthful populations, large families, lots of immigrants and diversity, and varying temperatures depending on which side of the country you’re on. These counties vary wildly in their rural-urban split, with some counties like Clark County, NV holding Las Vegas, a metro center, and some being in the sparsely populated regions of Montana. These counties struggle with poverty and rely heavily on SNAP, and have quite competitive elections.",
    "With just three counties, this cluster is one of the most intriguing of any that any of the methods have spat out. It only contains Kings County, New York County, and Bronx County. Interestingly, Richmond County and Queens County (which contain the city’s other two burroughs), are not included. These counties are unique in that they’re the most densely populated, public transit oriented, diverse populations in the entire country. The hierarchical method decided that Downtown New York was so different from everything else in the country that it warranted its own mini cluster."
  ),
  cluster_color = c(
    "#F1C40F",  # Growing Affluent Suburbs
    "#5DADE2",  # Mid Sized Metro Areas
    "#E67E22",  # Underserved Southern Communities
    "#4E944F",  # Rural Mid-east America
    "#2F6CB3",  # Urban Cores
    "#8C5A2B",  # Highland Regional Hubs
    "#F7DC6F",  # Established Older Suburbs
    "#B5651D",  # Resource Rich Economies
    "#7CB342",  # Midwestern Agricultural Areas
    "#8E6BBE",  # Frigid Retirement Communities
    "#4F7942",  # The Great Frontier
    "#C0392B",  # Rapidly Growing Border Counties
    "#0B3C5D"   # Downtown New York
  )
  )
saveRDS(hc_profiles, "data/final/hc_cluster_types.rds")

gmm_profiles <- tibble(
  cluster = 1:14,
  cluster_name = c(
    "Southern Working-Class America",
    "Affluent Coastal and Rugged Counties",
    "Older Industrial Heartland Counties",
    "Growing East / Midwest Suburban Hubs",
    "Appalachian Rural America",
    "Historically Black Southern Counties",
    "Upper Midwest Agricultural Centers",
    "Mid Sized Metro Suburbs",
    "Western Rural Recreation Areas",
    "Global Urban Centers",
    "Extraction Plains Counties",
    "Midwest Small Towns",
    "Rich Urban Centers",
    "Retirement Suburbs"
  ),
  description = c(
    "A group of pretty diverse, hot, flat counties predominantly in the south, both rural and suburban. Typically votes republican by a large margin and gets quite a bit of rainfall. These counties have some of the lowest access to the internet.",
    "A group of counties with cool climate and close to either the coast or big bodies of water inland with a rugged landscape. Higher education is more prevalent, leading to greater average income and lower percentage of residents relying on SNAP. However, these counties have some of the highest cost of living, with the 2nd highest housing cost burden of any of the 14 clusters. These counties tend to be near urban centers as suburbs or exurbs.",
    "Older than usual counties across the inland mountainous areas of the eastern US. These counties typically have lower diversity than usual, more high school only educated residents, a republican dominated political sphere, and tend to be covered in forest and rain.",
    "These counties tend to be much younger and more diverse compared to their geographical neighbors as well as slightly hotter and flatter. They surround metropolitan areas such as Milwaukee, Detroit, and Springfield IL and have a lower than average poverty rate.",
    "A set of predominantly white counties mainly in the Appalachian regions of West Virginia down towards Tennessee, these counties tend to be older, have less adults in the workforce, be covered in forests, and overwhelmingly vote republican. This group of counties has the highest average of people whose highest form of education was high school.",
    "These counties have the lowest population stability, losing residents more than any other cluster. These counties tend to have diversity rates that match cities, owing to their high population of POC as only 2% of the population is foreign born. These counties have the highest poverty rate and SNAP percentage in the country, and some of the lowest average income. Homes are worth very little here, and only on average 73% of people have access to the internet. They tend to be pretty competitive politically, and are the flattest, hottest, and rainiest counties in the country.",
    "These counties are almost all located in Illinois, Iowa, Wisconsin, and Minnesota. They are cold and not very densely populated, owing to their massive size, not very diverse, and are almost as lacking of forested areas as cities, owing to their agricultural nature. They have the smallest household sizes of anyone in the country, and vote republican.",
    "As the name implies, these counties are mainly in the suburbs of metropolitan areas, and smaller metropolitan areas themselves, especially in the midwest and east coast. Lots of eastern PA and New Jersey fall into this cluster, and they have extremely high population density for a non urban group of counties. Its largest counties include cities like Fort Worth and Columbus and its surrounding areas. These counties are young and diverse and have competitive elections on average.",
    "These counties in the west have the lowest population density of any county group in the country with just over 9 people per sq. mile on average here. These counties tend to be older, the most rural by RUCC codes of any county cluster, and a larger than average percentage of the workforce in agriculture. They also feature rugged terrain, high elevation, and extremely republican favored elections.",
    "This cluster features a denser population with counties such as Miami-Dade, Kings County (NYC), and others. The cities in this cluster tend to be not growing in population that fast, if at all, and tend to be younger, with higher household sizes, and a less educated population. The poverty rate and SNAP rate in this cluster is double that of the other big metro cluster, with the residents of 10 making much less money. They have more competitive elections, while still predominantly voting democrat. It also includes some outlier counties across the US.",
    "These counties don’t get a lot of rain, and have interestingly the lowest mean commute time of any cluster. They have the highest agriculture percentage, making sense given their location in the plains. They have a very low unemployment rate and some of the most affordable communities in the county and vote overwhelmingly republican.",
    "These counties have low, but not super low, population sizes, super low diversity, and tend to be less educated. Despite this, they have pretty low income inequality compared to others, and rank pretty middle of the road in every other metric. They’re flat and vote republican, much like other small population county clusters.",
    "This cluster is much richer than the other urban counties, containing counties like LA County, Maricopa County, and Harris County. Their populations tend to be less impoverished, growing, and they are more populated on average (potentially owing to the fact the other cluster, which contains some outliers). They’re the most diverse group of counties in the country, are more educated, less impoverished, and have higher costs of living. Overall, these counties are very similar, with a few key differences.",
    "These counties are the oldest counties in the country, and mainly are either in the northern stretches of the Upper Midwest, or the most southern stretches on the Gulf Coast. These counties are flat, rainy/snowy, and either extremely cold or hot, and covered in water, whether the gulf coast or great lakes."
  ),
  cluster_color = c(
    "#D35400",  # Southern Working-Class America
    "#D4AF37",  # Affluent Coastal and Rugged Counties
    "#7F8C8D",  # Older Industrial Heartland Counties
    "#5DADE2",  # Growing East / Midwest Suburban Hubs
    "#3A7D44",  # Appalachian Rural America
    "#E67E22",  # Historically Black Southern Counties
    "#7CB342",  # Upper Midwest Agricultural Centers
    "#F4C542",  # Mid Sized Metro Suburbs
    "#8C5A2B",  # Western Rural Recreation Areas
    "#2F6CB3",  # Global Urban Centers
    "#B5651D",  # Extraction Plains Counties
    "#6BA368",  # Midwest Small Towns
    "#1B4F72",  # Rich Urban Centers
    "#A07CC5"   # Retirement Suburbs
  )
  )
saveRDS(gmm_profiles, "data/final/gmm_cluster_types.rds")

#############################################################################
# Adding the Cluster Name to the Profiles Datasets
#############################################################################

pca_cluster_profiles <- readRDS("data/final/pca_clusters_profiles.rds")
pca_cluster_types <- readRDS("data/final/pca_cluster_types.rds")
hc_cluster_profiles <- readRDS("data/final/hc_clusters_profiles.rds")
hc_cluster_types <- readRDS("data/final/hc_cluster_types.rds")
gmm_cluster_profiles <- readRDS("data/final/gmm_clusters_profiles.rds")
gmm_cluster_types <- readRDS("data/final/gmm_cluster_types.rds")

pca_cluster_profiles <- pca_cluster_profiles |>
  left_join(
    pca_cluster_types |>
      select(cluster, cluster_name, cluster_color),
    by = "cluster"
  )

hc_cluster_profiles <- hc_cluster_profiles |>
  left_join(
    hc_cluster_types |>
      select(cluster, cluster_name, cluster_color),
    by = "cluster"
  )

gmm_cluster_profiles <- gmm_cluster_profiles |>
  left_join(
    gmm_cluster_types |>
      select(cluster, cluster_name, cluster_color),
    by = "cluster"
  )

gmm_cluster_profiles <- gmm_cluster_profiles |> 
  rename(rep_name = county_name)

saveRDS(pca_cluster_profiles, "data/final/pca_clusters_profiles.rds")
saveRDS(hc_cluster_profiles, "data/final/hc_clusters_profiles.rds")
saveRDS(gmm_cluster_profiles, "data/final/gmm_clusters_profiles.rds")

gmm_largest_counties <- readRDS("data/final/gmm_largest_counties.rds")

gmm_largest_counties <- gmm_largest_counties |>
  group_by(gmm_cluster) |>
  slice_head(n = 3) |>
  ungroup()

saveRDS(gmm_largest_counties, "data/final/gmm_largest_counties.rds")
#############################################################################
# Making Similarity Easier
#############################################################################

similarity <- read_parquet("data/finished/county_similarity.parquet")

top_similar <- similarity |>
  filter(similarity_rank < 6)

least_similar <- similarity |> 
  filter(similarity_rank > 3103)

saveRDS(top_similar, "data/final/most_similar.rds")
saveRDS(least_similar, "data/final/least_similar.rds")

#############################################################################
# End of Script
#############################################################################