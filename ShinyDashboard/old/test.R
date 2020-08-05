library(ggmap)

map <- get_map(location = 'Brazil', zoom = 4)

?register_google

library(ggplot2)

ggplot(data = df, mapping = aes(df$long, df$lat)) + geom_point() + rotate()
