library(FactoMineR)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
dataAlbums <- read_excel("./metal_dataset_albums.xlsx")
dataSongs <- read_excel("./metal_dataset_songs.xlsx")
songsFromCsv <- read.csv("./dataAcpExtract.csv")
dataSongs100 <- data.frame(songsFromCsv)[,-c(1)]

# Song distribution
years <- dataSongs$`Release Year`
h <- hist(years,
          main= title("Distribution of songs per 5-year time"),
          xlab="Years", ylab="Number of songs", col="mediumpurple"
)
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5),cex = 0.6)

# Averaging the 6 main features per year
# Data is tweaked. Here BPM is lowered so that the graph would show more details
dataMean <- dataSongs[,c(6,10:17)] %>% group_by(`Release Year`) %>% summarise(
  BPM=mean(`BPM`)-50,
  Dance=mean(`Dance`),
  Energy=mean(`Energy`),
  Valence=mean(`Valence`),
  Acoustic=mean(`Acoustic`),
  Popularity=mean(`Popularity`),
)

# Gathering data into one table to display it
dataGather <- dataMean %>% gather(key = `Audio Feature`, value = Score, -`Release Year`)
dataGather %>% ggplot(
  aes(x=`Release Year`, y=Score, group=`Audio Feature`, fill=`Audio Feature`)) +
  geom_line() + geom_smooth(method = lm) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size=14)
  ) +
  ggtitle("Mean audio feature per year") +
  facet_wrap(~`Audio Feature`)


# Principal Component Analysis
res.pca <- PCA(dataSongs100, quanti.sup=c(3), quali.sup=1)

# Correspondence Analysis
res.ca <- CA(
  table(dataAlbums$Origin, dataAlbums$`Sub Metal Genre`)
  )

# Distribution per country
albumsOrigin <- dataAlbums %>% 
  group_by(`Origin`) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Multiple Correspondence Analysis
# First, adapting the dataset, changing values (20, 51, 97...) to categories (low, medium, high)
dataMCA <- dataSongs100 %>% 
  mutate(energy = ifelse(`Energy` < 33,"low",ifelse(`Energy` < 66,"medium","high"))) %>%
  mutate(acoustic = ifelse(`Acoustic` < 33,"low",ifelse(`Acoustic` < 66,"medium","high"))) %>%
  mutate(dance = ifelse(`Dance` < 33,"low",ifelse(`Dance` < 66,"medium","high")))
# Keeping the essential
dataMCAlight <- dataMCA[,c(2,10:12)]
# Finally, plotting
res.mca <- MCA(dataMCAlight, quanti.sup = 1, graph = FALSE)
plot(res.mca, cex = 0.7, autoLab = "y", col.var = "black", col.ind = "pink1")

