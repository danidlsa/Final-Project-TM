library(tidyverse)
library(ggridges)
library(fmsb)

data = read.csv("data/lists of songs/full_dataset.csv")

ggplot(data, aes(x=danceability, y=title, fill=as_factor(year))) +
  geom_density_ridges(alpha=.4)

summary_data = read.csv("data/lists of songs/summary.csv") %>%
  select(-X, -tempo) %>%
  remove_rownames %>% 
  column_to_rownames(var="title")

scaled = as_tibble(scale(summary_data))
rownames(scaled) = rownames(summary_data)


vtable::vtable(scaled)

max_min <- data.frame(
    danceability = c(2.5, -2.5), energy = c(2.5, -2.5), key = c(2.5, -2.5),
  loudness = c(2.5, -2.5), mode = c(2.5, -2.5), speechiness = c(2.5, -2.5),
  acousticness = c(2.5, -2.5), instrumentalness = c(2.5, -2.5), liveness = c(2.5, -2.5),
  valence=c(2.5,-2.5)
)
rownames(max_min) <- c("Max", "Min")

df <- rbind(max_min, scaled)



create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.3), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}


max_min = head(df, 2)
df_2019 = rbind(max_min, df[3:5,])
df_2020 = rbind(max_min, df[6:8,])
df_2021 = rbind(max_min, df[9:11,])
df_2022 = rbind(max_min, df[12:14,])

rn_all =c("Max", "Min", "AUS", "NZ", "USA")
rownames(df_2019) = rn_all

list_data = list(df_2019, df_2020, df_2021, df_2022)


# Define colors and titles
colors <- c("#00AFBB", "#E7B800", "#FC4E07")
titles <- c("2019", "2020", "2021", "2022")

# Reduce plot margin using par()
# Split the screen in 3 parts

pdf("dataviz/radial chart.pdf", width=6, height=7)
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))


for(i in 1:4){
  create_beautiful_radarchart(
    data = data.frame(list_data[i]), caxislabels = c(0, 5, 10, 15, 20),
    color = colors, title = titles[i]
  )
}

legend(
  x = "bottom", legend = rownames(df_2019[-c(1,2),]), horiz = T,
  bty = "n", pch = 20 , col = colors,
  text.col = "black", cex = 1, pt.cex = 1.5
)


par(op)
dev.off()

