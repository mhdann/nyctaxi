---
Taxi, NYC
---
***
Where can I find a cab for a fast pickup? Where should taxi cab drivers go to minimize costs and maximize revenue? These two related questions are the center of my project in examining the New York City taxi trip database. The end product of the project will be a map of pickup probabilities for people seeking cabs at any given time of day and a set of strategies for cab network operators to attempt to meet historic demand at minimal cost.

New York City recently released information on all taxi trips throughout the city between 2009 and mid 2015. This information allows the modeling of the demand for taxi rides and their redistributive effect on cab stock in each neighborhood. With the judicious use of assumptions regarding the nature of cab revenue and costs as well as the application of other simplifying strategies it is possible to construct the probability of cab pickup in any given area and the optimization problem facing a cab system operator. 

Net taxi trips into an area can be computed from the data set. This is a map of the redistributive effect of demand for rides on the cab network. 

The following are plots of net taxi trips into given spatial regions (tract or burough) over every 15 minute interval during a representative Monday from the entire dataset (straight averaging, no conditioning on holidays). The tract level modeling may appear execessive, but due to the depth of the data the vast majority of spatial heterogeneity here is signal, not noise. Note that to show variation the scale has been clipped in both images, so pay careful attention to the fully saturated regions.

### Net Cab Trips. Pickups - Dropoffs.

![Tract Level Gif](http://i.imgur.com/gzMAhgY.gif) ![Burough Level Gif](https://i.imgur.com/vfKduED.gif)

In one sense, the plots above represent sources and sinks of empty cabs. Flipping the domain allows us to see the problem in calculating pickup probabilities: we do not have the location of empty cabs. I suggest using a diffusion based transportation model to represent the location of empty cabs and allow the development of pickup probabilities.

### Transportation Graph
The plot below represents a simplified graph of the transportation network in NYC and NJ.
![Gridded Graph](http://i.imgur.com/zbXDdPN.png)

As a proof of concept an initial distribution of cabs is provided in this time frame:
![Gridded Transport Model Frame 2](https://i.imgur.com/1fZD0N3.png)

...and iterated through 100 steps to demonstrate the diffusion.
![Gridded Transport Model Frame 100](https://i.imgur.com/jVy2ZCp.png)

This is a proof of concept transportation model. Diffusion coefficients can either be assumed (as is above) or estimated from data, since transportation times are included in the data set. A third option might be to hit the Google maps API for driving time estimates between various points at various times, though it is computationally unlikely (hundreds of millions of queries required).

### Data Processing So Far

Trip pickup and dropoff locations are paired to census tract or grid location and processed to flat files to tract/grid to support a variety of calculations. Showcased in this page is the distribution of net trips into regions over time. Also calculable, some trivially from my current summary statistics:
1. Trip time distributions based on pickup region.
2. Fare distributions based on pickup region.
3. Pickup-destination transfer matrix distributions.
4. Merging of weather covariates
5. The joint distributions of the above. Though admittedly the space is sparse in outter borough tracts.
6. Transportation graph
7. Diffusion model/mechanics

