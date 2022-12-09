library(ggplot2)

# Some data
data(mtcars)
mtcars$gear = factor(mtcars$gear, labels=c("w2", "word spp", "word2 spp"))

# change the relevant label to "italic non-italic"
lbs = brk = levels(mtcars$gear)
lbs[match("word spp", brk)] = expression(italic("word")~spp)
lbs[match("word2 spp", brk)] = expression(italic("word2")~spp)

ggplot(mtcars, aes(factor(am), mpg, fill=gear)) +
  geom_col(position="dodge") +
  scale_fill_discrete(breaks=brk, labels=lbs) +
  theme(legend.text  = element_text(face = "italic"))


# try with our data
blorp <- legume.2018.dat$Plant.Species
blorp2 <-c(
  "Astragalus purshii",
  "Astragalus sp.",
  "Lathyrus latifolius",
  "Lupinus caudatus",
  "Lupinus sericeus",
  "Medicago lupulina",
  "Melilotus officinalis",
  "Oxytropis sericea",
  "Trifolium campestre",   
  "Trifolium pratense",
  "Trifolium repens",
  "Trifolium sp.",        
  "Vicia americana",
  "Vicia sativa",
  "Vicia tetrasperma",
  "Vicia villosa",
  "Vigna"~italic(sp.)
)

y=expression('No. of'~italic(bacteria X)~'isolates with corresponding types')



# Fig 2 ######
# 2018 aphid abundance on plants
legume.2018.dat <- read.xlsx("./Data/legume data 2018.xlsx", 1, header=TRUE) %>% filter(Crop.type == "Non-crop")

#convert abundance to density
legume.2018.dat$Total.Aphid.Abudance <- log((legume.2018.dat$Total.Aphid.Abudance/legume.2018.dat$Total.Plant.Coverage)+1)


#aphid density fig for 2018 non-crop legume transects
aphid.hosts.fig <- ggplot(legume.2018.dat, aes(x=Total.Aphid.Abudance, 
                                               y=reorder(Plant.Species,-desc(Total.Aphid.Abudance)),
                                               fill=PEMV.Presence)) +
  geom_bar(stat="identity", width=0.6, position="dodge") +
  theme_bw(base_size = 16) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Legume species from transects", x="Aphids per meter sampled (log scale)", fill="PEMV Present") + 
  scale_fill_grey() +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(axis.text.y = element_text(face = "italic")) +
  theme(legend.position=c(0.8,0.2))
aphid.hosts.fig