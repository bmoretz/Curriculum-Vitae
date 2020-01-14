print_education <- function(data.sheet) {
  #' This function parses the education data from the
  #' information data sheet.
  #'
  #' Reads education information in the data sheet
  #' and returns rmd format.
  #'
  #' @param data.sheet the information data sheet (excel file)
  read_excel(data.sheet, sheet = "education") %>%
    filter(in_resume == T) %>%
    arrange(desc(end)) %>%
    mutate(id = 1:n(),
      start_date = format(as.Date(start, origin = "1899-12-30"), "%b '%y"),
      end_date = format(as.Date(end, origin = "1899-12-30"), "%b '%y")
    ) %>%
    mutate_at(vars(contains('desc')), as.character) %>%
    pivot_longer(
      starts_with('description'),
      names_to = 'description_num',
      values_to = 'description',
    ) %>%
    group_by(id) %>%
    filter(!is.na(description)) %>%
    mutate(
      descriptions = list(description),
      no_descriptions = is.na(first(description))
    ) %>%
    ungroup() %>%
    filter(description_num == 'description_1') %>% 
    mutate(
      timeline = ifelse(
        is.na(start) | start == end,
        end,
        glue('{end_date} - {start_date}')),
      
      description_bullets = ifelse(
        no_descriptions,
        ' ',
        map_chr(descriptions, ~paste('-', ., collapse = '\n'))
      )
    ) %>%
    mutate_all(~ifelse(is.na(.), 'N/A', .)) %>%  
    glue_data(
      "### {title}",
      "\n\n",
      "{institution}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{timeline}", 
      "\n\n",
      "{description_bullets}",
      "\n\n\n",
    )
}

print_professional <- function(data.sheet) {
  #' This function parses the professional experience data from the
  #' information data sheet, which are in tabs positions & projects.
  #'
  #' Each position can be associated with n projects, which are
  #' summarized and formatted in the prof exp section.
  #'
  #' @param data.sheet the information data sheet (excel file)  
  positions <- read_excel(data.sheet, sheet = "positions") %>%
    filter(in_resume == T) %>%
    select(-in_resume)
  
  projects <- read_excel(data.sheet, sheet = "projects") %>%
    filter(in_resume == T) %>%
    select(-in_resume)
  
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
      timeline =glue('{end_date} - {start_date}'),
      detail_bullets = ifelse(
        no_detail,
        ' ',
        map_chr(details, ~paste('-', ., collapse = '\n'))
      )
    ) %>%
    mutate_all(~ifelse(is.na(.), 'N/A', .))

  history %>%
    group_by(institution) %>%
    group_map( ~ {
      
      title <- unique(.x$title)
      loc <- unique(.x$loc)
      timeline <- unique(.x$timeline)
      summary <- unique(.x$summary)
      
      projects <- as_tibble(.x) %>%
        mutate(overview = ifelse(is.na(overview), "", paste0(overview, "\n"))) %>%
        group_by('name') %>%
        group_map( ~ {
          
          project <- as_tibble(.x) %>%
            glue_data("**{name}**\n
                      {overview}
                      {detail_bullets}\n
                      ***\n
                      Technology: {technology}")
          
          project
          
        }) %>%
        unlist()
      
      position <- paste( paste0("\n### ", title), 
                         .y, 
                         loc, 
                         timeline,
                         ifelse(summary == 'N/A', "", paste0(summary)),
                         sep = "\n\n")
      
      position <- cat(position, unlist(projects), sep = "\n\n")
      
      position
    }) %>%
    unlist() %>%
    cat()
}

skill_chart <- function(data.sheet, sidebar.col) {
  #' This function parses the language skill data from
  #' the information data sheet.
  #'
  #' Reads language skills information in the data sheet
  #' and returns a ggplot plot.
  #'
  #' @param data.sheet the information data sheet (excel file)
  read_excel(data.sheet, sheet = "skills") %>%
    arrange(desc(level)) %>%
    ggplot(aes(reorder(skill, level), level)) +
    geom_bar(aes(fill = level), stat = "identity", alpha = .8) +
    coord_flip() +
    xlab("") + ylab("") +
    ggtitle("", subtitle = "By Years Experience") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 16, face = "bold"),
          axis.text.y = element_text(size = 22, face = "bold"),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.subtitle = element_text(face = "bold", size = 22))
}

print_certs <- function(data.sheet) {
  read_excel(data.sheet, sheet = "certifications") %>%
    filter(in_resume == T) %>%
    mutate(date = format(as.Date(when, origin = "1899-12-30"), "%b '%y")) %>%
    glue_data(
      "### {name}",
      "\n\n",
      "[{type}]({link})",
      "\n\n",
      "{institution}",
      "\n\n",
      "{date}\n\n"
    )
}
