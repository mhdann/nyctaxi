###############################################################################
# Plots
# Temporary file - left over fragments from first run through.
###############################################################################

plot(density(t2015[clean == T & fare_rev_per_hour < 200 & fare_rev_per_hour > 3 & Weekday == "Monday" & pickup_hour == 6, log(fare_rev_per_hour)]))

# Plot fare density 2d, log-normal scale
xlims = c(-74.05,-73.75); ylims = c(40.6, 40.9)

my_theme = theme(panel.background = element_rect(fill = "black"),
                 panel.border = element_rect(colour = "black", fill=NA, size=2),
                 axis.title = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.ticks = element_blank(), 
                 axis.text = element_blank(), 
                 legend.position="bottom") 
n = 800
breaks = list(x = seq(xlims[1], xlims[2], length.out = n),
              y = seq(ylims[1], ylims[2], length.out = n))

monday = t2015[clean == T & trip_time > 1/60 & fare_rev_per_hour < 201 & fare_rev_per_hour > 29 & Weekday == "Monday", list(pickup_hour, pickup_longitude, pickup_latitude, fare_rev_per_hour, passenger_count)]
# Pick-up fare_rev_per_hour density, time sensitive for ride sharing

for(h in 0:23){
  p <- ggplot(data = monday[pickup_hour == h,],
              aes(x = pickup_longitude, y = pickup_latitude, z = fare_rev_per_hour)) + 
    stat_summary2d(fun = mean, breaks = breaks) + my_theme +  
    xlim(xlims) + ylim(ylims) + 
    scale_fill_gradient(name = "Fare Revenue per Hour", 
                        trans= "log", 
                        labels = c("50", "100"),
                        breaks = c(50, 100),
                        low = "black", high = "white") 
  # + coord_map(projection = "ortho", orientation = c(mean(ylims), mean(xlims), 0))
  p <- p + annotate(geom="text", x=breaks$x[n/2], y=breaks$y[n-20], label=paste0("Fare Revenue per Hour on Monday at ", sprintf("%02d:00",h)),
                    color="white")
  ggsave(filename = paste0("images/density_fare_rev_2015_Monday_",sprintf("%02d00",h),".png"),
         plot = p,
         width = 6, height = 5, dpi = 300)
}

#################### GIF MAKING CALL, IMAGEMAGICK ############
#  cd ./images
#  convert.exe -delay 50 -loop 0 *.png monday.gif
#################### GIF MAKING CALL, IMAGEMAGICK ############


# Time-series of the weekly pattern in prices
weekly = t2015[clean == T & trip_time > 1/60 & fare_rev_per_hour < 201 & fare_rev_per_hour > 29,
               list(fare_rev_per_hour = mean(fare_rev_per_hour)),
               by = list(Weekday, pickup_hour)]

w = ggplot(data = weekly, aes(x = pickup_hour, y = fare_rev_per_hour, group = Weekday, color = Weekday)) +
  geom_line() + theme_bw() + 
  labs(x = "Hour", y = "Fare Revenue per Hour")

ggsave(filename = paste0("ts_fare_rev_2015_weekday.png"),
       plot = w,
       width = 6, height = 5, dpi = 300)


# Monthly patterns 
monthly = t2015[clean == T & trip_time > 1/60 & fare_rev_per_hour < 201 & fare_rev_per_hour > 29,
                list(fare_rev_per_hour = mean(fare_rev_per_hour)),
                by = list(Month, pickup_hour)]




#
# smoothScatter(t2015[clean == T & pickup_day == 1 , list(pickup_longitude, pickup_latitude)][1:1000000,],
#              xlim = xlims, ylim = ylims)

# Check Passenger density
plot(density(t2015[clean == T & trip_time > 1/60 & fare_rev_per_hour < 200 & Weekday == "Monday" & pickup_hour == 6, passenger_count]))

for(h in 0:23){
  p <- ggplot(data = monday[pickup_hour == h,],
              aes(x = pickup_longitude, y = pickup_latitude, z = passenger_count)) + 
    stat_summary2d(fun = sum, breaks = breaks) + my_theme +  
    xlim(xlims) + ylim(ylims) + 
    scale_fill_gradient(name = "Passenger Density", 
                        trans= "log", 
                        low = "black", high = "white") 
  p <- p + annotate(geom="text", x=breaks$x[n/2], y=breaks$y[n-20], label=paste0("Passengers Coming/Arriving on Monday at ", sprintf("%02d:00",h)),
                    color="white")
  ggsave(filename = paste0("images/density_fare_rev_2015_Monday_",sprintf("%02d00",h),".png"),
         plot = p,
         width = 6, height = 5, dpi = 300)
}


# Plotting pickup/dropoff density
plotCity <- function(dt, title, alpha = 0.01, size = 0.001){
  ggplot(data = dt) + 
    geom_point(aes(x = pickup_longitude, y = pickup_latitude), 
               color = "white", size = size, alpha = alpha, ) + 
    geom_point(aes(x = dropoff_longitude, y = dropoff_latitude), 
               color = "white", size = size, alpha = alpha, ) + 
    my_theme +
    xlim(xlims) + ylim(ylims) + labs(title = title)
}

# monday = t2015[clean == T & trip_time > 1/60 & fare_rev_per_hour < 201 & fare_rev_per_hour > 29 & Weekday == "Monday", list(pickup_hour, pickup_longitude, pickup_latitude, pickup_hour, dropoff_longitude, dropoff_latitude)]
# plotCity(monday, title = "Monday Pickup & Dropoff Density")
