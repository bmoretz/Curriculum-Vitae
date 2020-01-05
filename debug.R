library(tidyverse)
library(ggthemes)
library(glue)
library(readxl)
library(docstring)

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

