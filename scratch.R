library(tidyverse)
library(ggthemes)
library(glue)
library(readxl)
library(docstring)
library(anytime)

select <- dplyr::select

theme_set(theme_light())

# Theme Overrides
theme_update(axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "darkgreen"),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             plot.subtitle = element_text(face = "bold", size = 8, colour = "darkred"),
             panel.background = element_rect(fill = "transparent"),
             plot.background = element_rect(fill = theme.colors$sidebar, color = NA), # bg of the plot to match the sidebar.
             legend.title = element_text(size = 12, color = "darkred", face = "bold"),
             legend.position = "right", legend.title.align=0.5,
             panel.border = element_rect(linetype = "solid", 
                                         colour = "lightgray"), 
             plot.margin = unit(c( 0.1, 0.1, 0.1, 0.1), "inches"))

data.sheet <- "background.xlsx"

positions <- read_excel(data.sheet, sheet = "positions") %>%
  filter(in_resume == T) %>%
  select(-in_resume)

projects <- read_excel(data.sheet, sheet = "projects") %>%
  filter(in_resume == T) %>%
  select(-in_resume)

# experience <- 


history <- inner_join(positions, projects, by = "institution") %>%
  mutate(id = 1:n(),
         start_date = format(as.Date(start, origin = "1899-12-30"), "%b '%y"),
         end_date = ifelse(is.na(end), "Present", format(as.Date(end, origin = "1899-12-30"), "%b '%y"))) %>%
  arrange(!is.na(end), desc(end)) %>%
  select(-c(start, end)) %>%
  mutate_at(vars(contains('detail')), as.character) %>%
  pivot_longer(
    starts_with('detail'),
    names_to = 'detail_num',
    values_to = 'detail',
  ) %>%
  filter(!is.na(detail)) %>%
  group_by(name) %>%
  mutate(
    details = list(detail),
    no_detail = is.na(first(detail))
  ) %>%
  ungroup() %>% 
  filter(detail_num == 'detail_1') %>% 
  mutate(
    timeline = ifelse(
      is.na(start_date) | start_date == end_date,
      end_date,
      glue('{end_date} - {start_date}')
    ),
    detail_bullets = ifelse(
      no_detail,
      ' ',
      map_chr(details, ~paste('-', ., collapse = '\n'))
    )
  ) %>%
  strip_links_from_cols(c('title', 'detail_bullets')) %>% 
  mutate_all(~ifelse(is.na(.), 'N/A', .))

details <- history %>%
  group_by(institution) %>%
    group_map( ~ {
      
      title <- unique(.x$title)
      loc <- unique(.x$loc)
      timeline <- unique(.x$timeline)
    
      projects <- as_tibble(.x) %>%
        group_by('name') %>%
        group_map( ~ {
          
          project <- as_tibble(.x) %>%
            glue_data("**{name}**
                      {overview}
                      {detail_bullets}
                      Technology: {technology}")
          
          project
          
        }) %>%
        unlist()
      
      position <- paste( paste0("### ", title), 
                         .y, 
                         loc, 
                         timeline,
                         sep = "\n\n")
      
      position <- cat(position, unlist(projects), sep = "\n\n")
      
      position
    })

cat(unlist(details), sep = "")

## Logo Plot
sigma <- matrix( c(1,.5,.5,1), 2, 2)
x <- data.frame(rmvt(n = 10000, sigma, df = 2, delta = rep(0, nrow(sigma)) )) 

x_cdf <- cbind(pt(x[,1],df=2),pt(x[,2], df=2)) 
z1 <- x_cdf[,1] 
z2 <- x_cdf[,2] 

z1 <- qgev( x_cdf[,1], loc=0.0004, scale=0.0009, shape=-0.3)  
z2 <- qbeta( x_cdf[,2], shape1=4.0, shape2=2.8 )
x <- data.frame(z1,z2)

pmain <- ggplot(x, aes(z1,z2)) +
  geom_point(alpha = .65, col = "darkgrey") +
  labs(x = "", y = "") +
  stat_density_2d(aes(fill = stat(level), alpha = ..level..), geom = "polygon") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank())

xbox <- axis_canvas(pmain, axis = "x") +
  geom_histogram(data = x, aes(z1, y = ..density.., fill = ..count..), size = .2, bins = 50, alpha = .65) +
  geom_density(data = x, aes(z1), col = "darkgrey", alpha = .55) +
  theme(legend.position = "none")

ybox <- axis_canvas(pmain, axis = "y", coord_flip = T) +
  geom_histogram(data = x, aes(z2, y = ..density.., fill = ..count..), size = .2, bins = 50, alpha = .65) +
  geom_density(data = x, aes(z2), col = "darkgrey", alpha = .55) +
  coord_flip() +
  theme(legend.position = "none")

suppressWarnings({
  p1 <- insert_xaxis_grob(pmain, xbox, grid::unit(.2, "null"), position = "top")
  p2 <- insert_yaxis_grob(p1, ybox, grid::unit(.2, "null"), position = "right")
})

ggdraw(p2)
