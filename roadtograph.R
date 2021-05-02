roadtograph <- function(road){
  edges <- road %>%
    mutate(edgeID = c(1:n()))
  
  #create nodes at the start and end point of each edge(tibble class)
  nodes <- edges %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(edgeID = L1) %>%
    group_by(edgeID) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(start_end = rep(c('start', 'end'), times = n()/2))
  
  nodes <- nodes %>%
    mutate(xy = paste(X, Y))
  nodes <- nodes %>% mutate(nodeID = group_indices(nodes, factor(xy, levels = unique(xy))))
  
  nodes = nodes[-5]
  
  #combine the node indices with the edges
  source_nodes <- nodes %>%
    filter(start_end == 'start') %>%
    pull(nodeID)
  
  target_nodes <- nodes %>%
    filter(start_end == 'end') %>%
    pull(nodeID)
  
  edges = edges %>%
    mutate(from = source_nodes, to = target_nodes)
  
  #remove duplicate nodes(tibble to sf)
  nodes <- nodes %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    dplyr::select(-c(edgeID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
  #convert to tbl_graph(sf line and sf point to one tbl_graph class)
  ## columns containing the indices of the source and target nodes should either be the first two columns of the sf object, or be named ‘to’ and ‘from’, repectively
  ## inside the tbl_graph function, these columns are converted into a two-column matrix.
  graph = tidygraph::tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)
  
  comp = components(graph)
  graph = graph %>% activate(nodes) %>% mutate(comp = comp$membership)
}
