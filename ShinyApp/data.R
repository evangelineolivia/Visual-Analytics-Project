pacman::p_load(tidyverse, jsonlite,
               tidygraph, ggraph, igraph, plotly, visNetwork,ggtext, ggiraph,patchwork,
               kableExtra, showtext,lubridate, DT)
library(dplyr)

#Load Data
data <- fromJSON("MC1_graph.json")  

# Custom Theme
font_add_google("Montserrat", "montserrat")
theme <- list(
  font = 'montserrat',
  size=14,
  background = element_rect(fill = "#FEFCF3",color = NA),
  title = element_text(
    size = 16,
    face = "bold",
    color = "black"),
  fill = "#96b3c2",
  panel = element_rect(fill = "#FEFCF3", color = NA),
  grid = element_line(color = "#E6DCD0")
)

edge_colors <- c(
  "PerformerOf" = "#E69F00",   
  "ComposerOf"  = "#56B4E9",   
  "LyricistOf"  = "#009E73",   
  "ProducerOf"  = "#F0E442",   
  "MemberOf"    = "#D55E00",
  
  "CoverOf" = "#E69F00",
  "InStyleOf" = "#56B4E9",
  "InterpolatesFrom" = "#009E73",
  "LyricalReferenceTo" = "#F0E442",
  "DirectlySamples" = "#D55E00"
)

node_colors <- c(
  "Person" = "#176BA0",
  "Song"   = "#F1948A",  
  "Album"  = "#C0392B",  
  "MusicalGroup" = "#9DF0E2",
  "RecordLabel" = "#CFF9F8"
)

edges <- as_tibble(data$links)
nodes <- as_tibble(data$nodes)

colnames(nodes)[colnames(nodes) == "Node Type"] <- "type"
colnames(edges)[colnames(edges) == "Edge Type"] <- "relation"

id_map <- tibble(id = nodes$id,
                 index = seq_len(
                   nrow(nodes)))
edges <- edges %>%
  left_join(id_map, by = c("source" = "id")) %>%
  rename(from = index) %>%
  left_join(id_map, by = c("target" = "id")) %>%
  rename(to = index)

edges <- edges %>%
  filter(!is.na(from), !is.na(to))
graph <- tbl_graph(nodes = nodes, edges = edges, directed = data$directed)

# Sailor Shift's Career Explorer

nodes <- graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  mutate(row_id = row_number())

edges <- graph %>%
  activate(edges) %>%
  as_tibble()

sailor_id <- nodes %>%
  filter(name == "Sailor Shift") %>%
  pull(row_id)

group_id <- edges %>%
  filter(from == sailor_id, relation == "MemberOf") %>%
  pull(to)

# Overview

## Active Years
sailor_releases <- edges %>%
  filter(from == sailor_id, relation == "PerformerOf") %>%
  pull(to)

group_releases <- edges %>%
  filter(from == group_id, relation == "PerformerOf") %>%
  pull(to)

all <- union(sailor_releases, group_releases)

release_years <- nodes %>%
  filter(row_id %in% all,
         (
           (type == "Song" & single == TRUE) |
             (type == "Album")
         )) %>%  
  mutate(release_year = as.integer(release_date)) %>%
  filter(!is.na(release_year)) %>%
  pull(release_year) %>%
  unique() %>%
  sort()

activeyears <- paste0(min(release_years), "–", max(release_years))

## Total Releases

total_releases <- nodes %>%
  filter(
    row_id %in% all,
    (
      (type == "Song" & single == TRUE) |
        (type == "Album")
    )
  ) %>%
  nrow()

#### Collaborators

collab_roles <- c("PerformerOf", "ComposerOf", "LyricistOf", "ProducerOf")

sailor_songs <- graph %>%
  activate(edges) %>%
  filter(from == sailor_id, relation %in% collab_roles) %>%
  pull(to)

collab_edges <- graph %>%
  activate(edges) %>%
  filter(to %in% sailor_songs, relation %in% collab_roles, from != sailor_id) %>%
  pull(from) %>%
  unique()

collaborators <- length(collab_edges)

#### Creatives Role Played

roles <- graph %>%
  activate(edges) %>%
  as_tibble() %>%
  filter(from == sailor_id) %>%
  pull(relation) %>%
  unique()
roles<- length(roles)

#### Charted Songs or Albums

charted <- nodes %>%
  filter(
    row_id %in% all,
    (
      (type == "Song" & single == TRUE) |
        (type == "Album")
    ),
    notable == TRUE 
  )
charted_count <- nrow(charted)

#### Chart Success Ratio

ratio <- round((charted_count / total_releases) * 100)
ratio <- paste0(ratio,'%')

### Tab 2 : Career Timeline

#### Bar Plot
release_summary <- nodes %>%
  filter(
    row_id %in% all,
    type == "Song"| type == "Album"
  ) %>%
  mutate(
    year = as.integer(release_date)
  ) %>%
  filter(!is.na(year)) %>%
  count(year, type, name = "n")


release_summary_table <- nodes %>%
  filter(
    row_id %in% all,
    type == "Song" | type == "Album"
  ) %>%
  mutate(
    year = as.integer(release_date)
  )

### Tab 3

#### Table



## Oceanus Folk Influence Tracker

### Tab 1 : Overview

#### Total performers
oceanus_songs <- nodes %>% 
  filter(
    genre == "Oceanus Folk",
    type == "Song"| type == "Album"
  )

performer_edges <- edges %>%
  filter(relation == "PerformerOf", to %in% oceanus_songs$row_id)

performer_ids <- unique(performer_edges$from)

unique_performers <- nodes %>%
  filter(row_id %in% performer_ids, type %in% c("Person", "MusicalGroup")) %>% 
  select(`Name`=name,`Type`=type)

n_unique_performers <- nrow(unique_performers)
 
#### Total Release
oceanus_releases <- nodes %>%
  filter(
    genre == "Oceanus Folk",
    type == "Song" | type == "Album"
  ) %>%
  mutate(
    year = as.integer(release_date)
  ) %>%
  filter(!is.na(year)) %>%
  count(year, type, name = "n")
oceanus_release <- nrow(oceanus_releases)

#### Year Active
Year_active <- paste(min(oceanus_releases$year),"-",max(oceanus_releases$year))

### Tab 2 : Influence Timeline

