library(tidyverse)
library(ggthemes)
library(glue)
library(readxl)
library(docstring)

library(dplyr)
library(tidyr)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(brms)
library(ggstance)

theme_set(theme_light())

# Theme Overrides
theme_update(axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "darkgreen"),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             plot.subtitle = element_text(face = "bold", size = 8, colour = "darkred"),
             legend.title = element_text(size = 12, color = "darkred", face = "bold"),
             legend.position = "right", legend.title.align=0.5,
             panel.border = element_rect(linetype = "solid", 
                                         colour = "lightgray"), 
             plot.margin = unit(c( 0.1, 0.1, 0.1, 0.1), "inches"))


read_excel(data.sheet, sheet = "skills") %>%
  arrange(desc(level)) %>%
  ggplot(aes(reorder(skill, level), level)) +
  geom_bar(aes(fill = level), stat = "identity", alpha = .8) +
  coord_flip() +
  xlab("") + ylab("Proficiency") +
  ggtitle("", subtitle = "By Years Experience") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_text(size = 22, face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "#f7fbff", color = NA), # bg of the plot
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


data.sheet <- read_excel("positions.xlsx")

datasets <- "positions.xlsx"

positions <- read_excel(data.sheet, sheet = "positions")
projects <- read_excel(data.sheet, sheet = "projects")

theme_gar <- theme_bw() + 
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14, colour="black"),
        legend.key.width = unit(1.5,"cm"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        strip.text = element_text(size=20, face="bold", colour="white"),
        strip.background = element_rect(colour="black", fill="grey40"))

library(beepr)
library(WRS)

set.seed(21)
nsim <- 10000 # population size
nkde <- 500 # sample size for KDE
gseq <- seq(0,1,0.1) # sequence of g parameters
xseq <- seq(-5,7,0.01) # points at which to compute KDE
gh.kde <- matrix(0, nrow = length(xseq), ncol = length(gseq)) # KDE results to plot in next figure
mean.g <- vector(mode = "numeric", length = length(gseq))
tmean10.g <- vector(mode = "numeric", length = length(gseq))
tmean20.g <- vector(mode = "numeric", length = length(gseq))
md.g <- vector(mode = "numeric", length = length(gseq))
for(G in 1:length(gseq)){
  print(paste("gseq =",G,"/",length(gseq)))
  beep(2)
  set.seed(7)
  samp <- ghdist(nsim, g = gseq[G], h = 0)
  gh.kde[,G] <- akerd(ghdist(nkde, g = gseq[G], h = 0), 
                      pts = xseq, pyhat = TRUE, plotit = FALSE)
  mean.g[G] <- mean(samp) # population mean
  tmean10.g[G] <- mean(samp, trim = 0.1) # population 10% trimmed mean
  tmean20.g[G] <- mean(samp, trim = 0.2) # population 20% trimmed mean
  md.g[G] <- median(samp) # population median
}

# make data frame
df <- tibble(x = rep(xseq, length(gseq)),
             y = as.vector(gh.kde),
             g = factor(rep(gseq, each = length(xseq))))

# make plot
ggplot(df, aes(x = x, y = y, colour = g)) + theme_gar +
  geom_line(size = 1) +
  scale_colour_viridis_d(end = 0.9) +
  theme(legend.title = element_text(size=16, face="bold"),
        legend.position = c(.8, .55)) +
  coord_cartesian(xlim = c(-4, 6)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = seq(-4, 15, 1)) +
  labs(x = "Observations", y = "Density") +
  guides(colour = guide_legend(override.aes = list(size=3)))

library(tidybayes)

data.frame(alpha = seq(5, 100, length.out = 10)) %>%
  ggplot(aes(y = "", dist = "beta", arg1 = alpha, arg2 = 10, slab_color = alpha)) +
  # show.legend = TRUE is currently necessary here due to a bug in ggplot
  stat_dist_halfeyeh(show_interval = FALSE, fill = NA, show.legend = TRUE) +
  coord_cartesian(expand = FALSE) +
  # you can set color scales on the "slab_color" aesthetic by passing 
  # `guide` and `aesthetics` parameters to existing color scale functions:
  scale_color_viridis_c(guide = "colorbar2", aesthetics = "slab_color") +
  labs(
    title = "stat_dist_halfeyeh(show_interval = FALSE, fill = NA)",
    x = "Beta(alpha,10) distribution",
    y = NULL
  ) 

dist_df %>%
  ggplot(aes(x = group, dist = "norm", arg1 = mean, arg2 = sd, fill = subgroup)) +
  stat_dist_eye(position = "dodge") +
  ggtitle("stat_dist_eye(position = 'dodge')")

library(tidybayes)

data.frame(alpha = seq(5, 100, length.out = 10)) %>%
  ggplot(aes(y = alpha, dist = "beta", arg1 = alpha, arg2 = 10)) +
  stat_dist_halfeyeh() +
  labs(
    title = "stat_dist_halfeyeh()",
    x = "Beta(alpha,10) distribution"
  )

plotb <- function(s1=1, s2=1, ncp=0) {
  g=ggplot()+xlab("")
  df=expand.grid(s1=s1, s2=s2, ncp=ncp)
  n=nrow(df)
  for(i in 1:n){
    x=df[i,]
    g=g+stat_function(
      aes(c(0, 1)), color=hcl.colors(n)[i],
      fun=dbeta, args=list(x$s1, x$s2, x$ncp)
    )
  }
}

plotb(5, seq(1, 6, by = 2), seq(1, 20, by = 1))

seq(5, 100, length.out = 10)
