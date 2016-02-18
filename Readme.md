---
title: "Optimal Taxi Distribution"
output:
  html_document:
    fig_width: 4
---
***
The primary question faced by the operator of a taxi network is how to distribute cabs to maximize revenue (or minimize cost). This is a type of a inventory control problem with stochastic components that may increase or decrease inventory at a given location. The cab network operator experiences stochastic demand for trips which distribute the cabs across a city according to the patterns of daily routine and is then faced with the question of how to best direct cabs throughout the day to optimize revenue. Or put simply, "where should I park my cab between trips?" and the closely related question, "how many cabs do I need at a minimum?" The problem also informs the regulator, "what is the effect of changing regulation on the efficiency of meeting ridership demand?"

New York City recently released information on all taxi trips throughout the city between 2009 and mid 2015. This information allows the modeling of the demand for taxi rides and their redistributive effect on cab stock in each neighborhood. With the judicious use of assumptions regarding the nature of cab revenue and costs as well as the application of other simplifying strategies it is possible to construct the optimization problem facing a cab system operator.

First, a look at the effect of demand on taxi distribution. The following are plots of net taxi trips into given spatial regions (tract or burough) over every 15 minute interval during a representative Monday from the entire dataset (straight averaging, no conditioning on holidays). The tract level modeling may appear execessive, but due to the depth of the data the vast majority of spatial heterogeneity here is signal, not noise. Note that to show variation the scale has been clipped in both images, so pay careful attention to the fully saturated regions.

![Tract Level Gif](https://i.imgur.com/FMpgKhh.gif) ![Burough Level Gif](https://i.imgur.com/vfKduED.gif)

### Basic Dynamic Stochastic Program
#### Dynamics
Here $x_t$ represents stock of cabs at any given location & time, $u_t$ represents a reallocation of cabs, and $w_t$ represents the distributive effect of the stochastically modeled demand seen in the above gifs. The time evolution of the system is then:

$$x_{t+1} = x_t + u_t + w_t$$

Subject to a host of other constraints, such as $\sum_{space}{x_t} \leq N_t$, where $N_t$ is the total number of cabs available to place on shift.

#### Cost
$$g_t = g(x_t, u_t, w_t)$$ This function is represented generically, as linear or quadratic for expediency. Redistribution choices $u_t$ may be linked with transportation times which may then be costed.

### Required assumptions:

1. Sole operator. There is no competition of interests modeled here. What's good for the system is good for the driver.

2. Fixed demand. There is an assumption that the pattern of demand does not change with changes to the distribution of cabs. That is, if demand exists for a pickup at point $A$ and the operator chooses to move the cab to point $B$ the choice does not influence the short-run demand for the cab, but does imply that the demand will potentially go unmet and be factored into the cost function.

3. In constructing the cost function, it is necessary to grossly simplify the costs associated with travel. Foregone revenue modeled linearly to represent empty cab time and value of lost trips are obvious choices.

4. The remainder of the cost function is the cost associated with the control $u_t$, which is the cost of reallocating cabs between regions. It is also suggested that this cost be time based, along with a transportation network simplification (see below). 

#### Proposed Solution Technique & Dimensional Simplification:
1. A first-order estimate may be obtained by taking the cost of redistribution to be zero. Cost becomes solely a function of $x_t$ and $w_t$ and total adjustment between periods occurs. That is, Since there is no cost associated with redistributing cabs, adjustment is total. This simplification completely unlinks the temporal dynamics in the solution. 

2. The first-order estimate provides a point towards which the system would proceed given no costs of adjustment. Adding back the costs of adjustment, the solution shifts to the point where the cost of partial adjustment meets the expected cost of disequilibrium (from said adjustment-cost-free point). Hence (1) provides a natural framework with which to recenter the space.

3. Constructing the expected cost of disequilibrium is the hard part, though not dissimiliar from the powers-of-two card game challenge problem for the Data Incubator Fellowship. The idea is to use backward recursion, temporally. However, here the horizon is infinite. There is no farthest branch of the tree. Fortunately it is possible to apply the discrete-time, infinite horizon Bellman's optimality equation; to be evaluated either analytically or markov sampling. This is a challengingly large space and deserves further pruning.

4. In the newly recentered space, the decision set $u_t$ is very large. Potentially a cab may be directed from any tract to any other tract as part of the cost minimization. Pruning this tree is absolutely necessary to pull the problem into the realm of tractibility. The control set is of spatial dimension $n^2$ where $n$ is the number of regions.

5. I propose the use of spatial hiearchy in the transfer of cabs from point-to-point and recursion to trim the control set. The space can be broken into different scales, a census tract level scale within a neighborhood scale involving a dozen census tracts within a borough scale involving a few dozen neighborhoods. The idea being that witin each group there is a cost minimizing distribution of cabs given the number of cabs available to that group. Then toward the root of the tree the tradeoffs between the broader groups are evaluated. 

### Data Processing So Far

Trip pickup and dropoff locations are paired to census tract and processed to flat files to tract to support a variety of calculations. Showcased in this page is the distribution of net trips into regions over time. Also calculable:
1. Trip time distributions based on pickup region.
2. Fare distributions based on pickup region.
3. Pickup-destination transfer matrix distributions.
4. Merging of weather covariates
5. The joint distributions of the above. Though admittedly the space is sparse in outter borough tracts.


### General Outline of Data Processing
1. Download the raw files for the Yellow Cab trips
If you've ever wondered what wget looks like in R...
`./nycDownload.R`

2. Clean the raw files of mislaid columns and characters.
`./nycRawClean.sh`

3. Pair the raw files census tract number and save to csv flat files by pickup/dropoff tract directories.
`./nycPipelineCsv.R`

4. Calculate transfer matrices and net trips.
`./nycCalculateNetTransfer.R`

5. Generate figures
`./nycAnalysis.R`

###To Do List

1. Develop transportation hiearchy
    * Highest level: Buroughs + airports
    * Next level: Neighborhoods, custom design
    * Lowest level: census tract or NTA neighborhood
    * Spatially smooth sparse neighborhoods
    
2. Develop cost model
    * Expected fare distribution
          * Value of lost fare
    * Transportation cost by hiearchy level
    
3. Solve trivial solution (no distribution cost)

4. Recenter data set about trivial civil, construct Bellman's equation

5. Sample/evaluate Bellman's equation

6. Data Scope
    * Add Green cab data
    * Add covariates
        * "events" (snow etc) from wunder
        * Holiday dummies

7. Deferred
    * Cleanup data processing
        * Remove row.names from pickup tract csvs
        * Remove redundant time columns
    * Data integrity
        * 10% of the 2010-09 yellow taxi data appears to contain duplicates on lat/lon, time, but not passenger details.
        * A large portion of 2010 contains apparent duplicates


8. Simple Web App
    + R-Shiny transfer matrix browser
        + Tract level maps, shading/intensity to indicate cost or probability of going to destination tract
        + Demonstrate effect of weather on net rides (coefficients or conditional means)
          + Binned coefficients
    + Demonstrate conditional means of regular temporal patterns
    + Time-series plot on various margins
    + Demonstrate any gross trends at low frequency

