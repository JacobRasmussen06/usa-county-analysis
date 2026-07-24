### Week of July 13th, 2026

#### Goal

Start the project.

#### Progress

-   Created GitHub Repository

-   Set Research Question

-   Initialized R Project

-   Created folder and analysis structure

-   Created First ACS data pipeline.

-   Downloaded an initial county-level dataset.

-   Began engineering and obtaining the data for all variables decided on.

-   Created the finished dataset that will be used for (version 1 if there are more versions) of the clustering and analysis.

-   Cleaned the R Scripts to an initial usable state

-   Finished an initial data dictionary.

#### Decisions Made

-   US Counties will be the unit analyzed

    -   This decision was made for several factors. Firstly, there are over 3100 counties/county-equivalents in the United States, which is enough variation. There is easy accessible public data available for this unit, and it is easy to map and familiar to people who live in the United States.

-   Planned techniques to use: clustering, PCA, and anomaly detection

-   Made a table of all the variables going to be used in the project

### Week of July 20th, 2026

#### Goal

Explore the data, do analysis, and start the process towards finishing analysis and making a finished product.

#### Progress

-   Finished Exploratory Data Analysis

    -   Created visualizations (10 maps, 6 heatmaps, and 7 histogram/barcharts) that help illustrate this process

    -   Took notes along the way

-   Started and Ran Principal Component Analysis

    -   Finished PCA

        -   Developed the PCA and interpreted the loadings for each PC

        -   Created Scree Plot

        -   Created combined variance plot

        -   Created and interpreted PC1-3 loading plots

        -   Visualized P1-3 geographically with maps

-   Performed the first clustering using PCA

    -   Created several visualizations in both PCA and Clustering

-   Cleaned the EDA, PCA, and Clustering Scripts

#### Decisions

-   Decided not to include the CHR&R variables in the PCA dataset, as they were missing hundreds of counties' data and It was determined to be too big of a loss to not include 1/3 of the dataset.
-   Decided to scale the forest coverage variable to some counties using median forest cov for the region they're in because it was only like 40 counties missing
