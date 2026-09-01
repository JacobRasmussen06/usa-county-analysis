# usa-county-analysis

📊 Explore the Interactive Dashboard featuring an interactive map: [Dashboard](https://jacobrasmussen06.shinyapps.io/us-county-explorer/)

📖 Read the Report: [PDF](https://jacobrasmussen06.github.io/usa-county-analysis/report/report.pdf) &nbsp; [HTML](https://jacobrasmussen06.github.io/usa-county-analysis/report/report.html)

🗃️ [GitHub Repository](https://github.com/JacobRasmussen06/usa-county-analysis)

🔗 [Live Pages](https://jacobrasmussen06.github.io/usa-county-analysis/)

# Exploring the Statistical Geography of the USA

Counties, or county-equivalents like Parishes, in the United States are similar and different on several different characteristics. Among these can include a county's demographics, economics, geography, or politics. Some counties, like the 5 New York boroughs, are obviously similar, and different from incredibly rural counties such as Loving County, Texas. However, how can counties be grouped into meaningful clusters? How are counties typically similar. How are counties typically different?

Using unsupervised learning techniques, this project aims to identify different communities, understand the major factors that separate U.S regions, and find statistically unique counties that do not fit the patterns of their neighbors. Three clustering methods: k-means clustering following PCA analysis, hierarchical clustering, and Gaussian mixture model based clustering were each used to identify groups of U.S. counties. A similarity and uniqueness engine based in Euclidean distance was created to find direct comparisons between counties and their neighbors, cluster partners, and the nation as a whole. 

## Project Highlights

- Geographical analysis
- Machine learning
- Clustering
- Several engineered features
- Custom visualizations
- Interactive Dashboard and Map
- 10,000 word technical report

## Structure

```text
app/          All modules, css, helpers, and more related to the dashboard

data/         Raw, cleaned (finished), and final data

docs/         Data dictionary and a devlog that was used to update progress during the scripts section of this project

figures/      All visualizations created from the scripts

report/        Report

scripts/       Data cleaning, analysis, clustering, engine building
```

## Key Features

- Created three different sets of clusters using different methods
- Analysis of every cluster
- Several engineered variables
- Large scale user-friendly interactive dashboard
- Similarity engine featuring a similarity score for every county pair
- Uniqueness engine showing the most and least unique counties
- And more!

## Results

This project successfully gave an answer to each research question: there are 13 synthesized archetypes of U.S counties: Midwest Suburbia, Great Plains Rural Areas, Frigid Retirement Communities, Underserved Southern America, Rural Appalachia, Extraction / Border Counties, The Rural Recreational West, Mid Size Metro Areas, Rugged Coastal Suburbs, Established Industrial Urban Cores, Global Super-cities, Affluent Suburbia, and Sunny Retirement Communities. Counties vary on each and every variable, but often counties are more similar if they share socioeconomic and environmental variables, and counties differ on their rurality, their demographics, and their wealth.

Ultimately, the results of this project suggest that while there is no single definitive way to divide the United States into similar groups of counties, several archetypes, similarity indicators, and differences emerge regardless of the methodology.

## Data

Several data sources were used during the course of this project. Data sources include:

-   American Community Survey (ACS) 2023 5-year estimates
-   TIGER/Line county geography
-   PRISM climate normals
-   USGS elevation data
-   NLCD/FIA land coverage data
-   County Health Rankings
-   County Presidential Election Results
-   Natural Earth Coastline
-   Rural-Urban Continuum Codes

## Technologies

This project was developed using R, from the scripts to the report to the dashboard. Several packages were prevalent throughout the project:
- R
- tidyverse
- sf
- shiny
- leaflet
- bslib
- rnaturalearth
- FedData
- tidycensus
- cluster
- mclust
- factoextra

## Reproducibility

1. Clone the repository.
2. Install required packages in R.
3. Run the scripts from the 'code/' directory.
4. Render the report from the 'reports/' directory.
5. Render the app locally from the 'app/' directory.

## Future Work

There are several avenues to expand the work completed in this project. Several more data sources and variables could be created, the data could be expanded to explore smaller units of size than counties, such as municipalities, further clustering using different methods may also be explored.

## About Me

Jacob Rasmussen, third year data science and statistics student at UW-Madison
