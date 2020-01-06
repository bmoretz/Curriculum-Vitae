# Regex to locate links in text
find_link <- regex("
  \\[   # Grab opening square bracket
  .+?   # Find smallest internal text as possible
  \\]   # Closing square bracket
  \\(   # Opening parenthesis
  .+?   # Link text, again as small as possible
  \\)   # Closing parenthesis
  ",
                   comments = TRUE)

# Function that removes links from text and replaces them with superscripts that are 
# referenced in an end-of-document list. 
sanitize_links <- function(text){
  if(PDF_EXPORT){
    str_extract_all(text, find_link) %>% 
      pluck(1) %>% 
      walk(function(link_from_text){
        title <- link_from_text %>% str_extract('\\[.+\\]') %>% str_remove_all('\\[|\\]') 
        link <- link_from_text %>% str_extract('\\(.+\\)') %>% str_remove_all('\\(|\\)')
        
        # add link to links array
        links <<- c(links, link)
        
        # Build replacement text
        new_text <- glue('{title}<sup>{length(links)}</sup>')
        
        # Replace text
        text <<- text %>% str_replace(fixed(link_from_text), new_text)
      })
  }
  text
}

# Take entire positions dataframe and removes the links 
# in descending order so links for the same position are
# right next to eachother in number. 
strip_links_from_cols <- function(data, cols_to_strip){
  for(i in 1:nrow(data)){
    for(col in cols_to_strip){
      data[i, col] <- sanitize_links(data[i, col])
    }
  }
  data
}

# Take a position dataframe and the section id desired
# and prints the section to markdown. 
print_section <- function(position_data, section_id) {
  position_data %>% 
    filter(section == section_id) %>% 
    arrange(desc(end)) %>% 
    mutate(id = 1:n()) %>% 
    pivot_longer(
      starts_with('description'),
      names_to = 'description_num',
      values_to = 'description'
    ) %>% 
    filter(!is.na(description) | description_num == 'description_1') %>%
    group_by(id) %>% 
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
        glue('{end} - {start}')
      ),
      description_bullets = ifelse(
        no_descriptions,
        ' ',
        map_chr(descriptions, ~paste('-', ., collapse = '\n'))
      )
    ) %>% 
    strip_links_from_cols(c('title', 'description_bullets')) %>% 
    mutate_all(~ifelse(is.na(.), 'N/A', .)) %>% 
    glue_data(
      "### {title}",
      "\n\n",
      "{loc}",
      "\n\n",
      "{institution}",
      "\n\n",
      "{timeline}", 
      "\n\n",
      "{description_bullets}",
      "\n\n\n",
    )
}

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
    mutate(id = 1:n()) %>%
    mutate(start_date = format(as.Date(start, origin = "1899-12-30"), "%b '%y"),
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
    strip_links_from_cols(c('title', 'description_bullets')) %>% 
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

skill_chart <- function(data.sheet, sidebar.col) {
  #' This function parses the language skill data from
  #' the information data sheet.
  #'
  #' Reads language skills information in the data sheet
  #' and returns a ggplot plot.
  #'
  #' @param data.sheet the information data sheet (excel file)
  #' 
  read_excel(data.sheet, sheet = "skills") %>%
    arrange(desc(level)) %>%
    ggplot(aes(reorder(skill, level), level)) +
    geom_bar(aes(fill = level), stat = "identity", alpha = .8) +
    coord_flip() +
    xlab("") + ylab("Proficiency") +
    ggtitle("", subtitle = "By Years Experience") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 16, face = "bold"),
          axis.text.y = element_text(size = 22, face = "bold"),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}
