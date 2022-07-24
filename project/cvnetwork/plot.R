my_df <- NET$entries_data

my_df$end[1] <- "current"

my_df$start[3] <- "NA"

my_relevant_df <- my_df %>% dplyr::select(title, section, start, end)

current_year <- lubridate::year(lubridate::ymd(Sys.Date()))

my_relevant_df$start <- as.numeric(my_relevant_df$start)

my_relevant_df$end <- as.numeric(my_relevant_df$end)

my_relevant_df$filter <- interaction(is.na(my_relevant_df$start),is.na(my_relevant_df$end))

my_relevant_df <- my_relevant_df[!my_relevant_df$filter == "TRUE.TRUE",]

my_relevant_df <- my_relevant_df %>% dplyr::select(title, section, start, end)

my_relevant_df <- my_relevant_df %>% dplyr::mutate(start = ifelse(is.na(start), end, start))

my_relevant_df <- my_relevant_df %>% dplyr::mutate(end = ifelse(is.na(end), current_year, end))

edges <- my_relevant_df
edges <- edges %>% dplyr::mutate(id = dplyr::row_number())
edges <- edges %>% dplyr::select(id, start, end)

combination_indices <- function(n) {
  rep_counts <- (n:1) - 1
  dplyr::tibble(a = rep(1:n, times = rep_counts), b = purrr::flatten_int(purrr::map(rep_counts, 
                                                                                    ~{
                                                                                      tail(1:n, .x)
                                                                                    })))
}


edges <- edges %>% purrr::pmap_dfr(function(id, 
                         start, end) {
  dplyr::tibble(year = start:end, id = id)
}) 

edges <-  edges %>% dplyr::group_by(year) %>% tidyr::nest() %>% dplyr::rename(ids_for_year = data)

edges <- edges %>% purrr::pmap_dfr(function(year, ids_for_year) {
  combination_indices(nrow(ids_for_year)) %>% dplyr::transmute(year = year, 
                                                               from = ids_for_year$id[a], to = ids_for_year$id[b])
})

nodes <- my_relevant_df
nodes <- nodes %>% dplyr::mutate(id = dplyr::row_number())
nodes <- nodes %>% dplyr::select(id, title, section)

nodes <- nodes %>% dplyr::mutate(section = stringr::str_to_title(stringr::str_replace_all(section, "_", " ")))

sections <- unique(nodes$section)

colors <- brewer.pal(n = length(sections), "Paired")

nodes$color.background <- nodes$section

for (i in 1:length(sections)) {
  
  nodes[nodes$section == sections[i],]$color.background <- colors[i]
  
}

nodes$color.border <- "black"
nodes$highlight.background <- "black"
nodes$highlight.border <- "black"
nodes$label <- paste0(nodes$section, ":\n", ifelse(nchar(nodes$title) >20, paste0(substr(nodes$title,1,20), "..."), nodes$title))

years <- sort(unique(edges$year))

colors <- brewer.pal(n = length(years), "Greys")

edges$color <- as.character(edges$year)

for (i in 1:length(years)) {
  
  edges[edges$year == years[i],]$color <- colors[i]
  
}

library(visNetwork)
visNetwork(nodes, edges)%>%
  visIgraphLayout(layout = "layout_nicely")%>%
  visOptions(highlightNearest = list(hover = T))



visSave(graph = plot,file = "plot.html",selfcontained = T)
