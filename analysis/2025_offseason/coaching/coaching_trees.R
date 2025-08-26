library(tidyverse)
library(dplyr)
library(igraph)
dotenv::load_dot_env()
data_path <- Sys.getenv("LOCAL_DATA_PATH")

coaches <- read_csv(file.path(data_path, "yearly_coaching_history.csv")) %>% 
  mutate(coach_id = gsub("/executives/", "", coach_id))

coaching_history <- read_csv(file.path(data_path, "coaching_histories.csv"))

hc_tenures <- coaching_history %>%
  filter(Level == "NFL") %>%
  group_by(coach_id) %>%
  summarize(hc_seasons = sum(Role == "Head Coach"))

coaching_tree <- read_csv(file.path(data_path, "coaching_trees.csv")) %>% 
  separate_rows(Role, sep = ", ") %>% 
  separate_rows(Role, sep = "/") %>% 
  unique() %>% 
  group_by(coach_id, type, Name) %>%
  summarise(Role = paste(Role, collapse = " / "), .groups = "drop") %>% 
  ungroup() %>% 
  select(Role, coach_id, type, Name) %>% arrange(Name)

coach_ids <- coaches %>% 
  select(coach, coach_id) %>% 
  unique()

coaching_tree_2 <- coaching_tree %>% 
  left_join(coach_ids) %>% 
  left_join(coach_ids %>% select(Name = coach, tree_coach_id = coach_id)) %>% 
  group_by(Name, coach_id, type) %>% 
  mutate(count = n()) %>% 
  filter(Name != "Jim Mora") %>% 
  ungroup() %>% 
  filter(type == 'Employed') %>% 
  select(coach_id, coach_name = coach, tree_coach_id, tree_coach_name = Name, tree_coach_role = Role)


# 
# coach_stats <- coaching_tree_2 %>%
#   group_by(coach_id, coach_name) %>%
#   summarize(count = n_distinct(tree_coach_id)) %>%
#   arrange(desc(count)) %>%
#   left_join(hc_tenures) %>%
#   filter(hc_seasons >= 5, count >= 6)

edges <- coaching_tree_2 %>%
  select(coach_id, tree_coach_id) %>%
  filter(!is.na(tree_coach_id))

# Create directed graph
g <- graph_from_data_frame(edges, directed = TRUE)


all_tree_data <- data.frame()
root_coaches <- setdiff(coaching_tree_2$coach_id, coaching_tree_2$tree_coach_id)
for(lvl in c(1:7)){
  # Coaches who are never listed as tree_coach_id (they started a coaching tree)
  print(paste("Start of iteration", lvl, ":"))

  # Use BFS to get coaching trees
  coaching_trees <- list()

  for (root in root_coaches) {
    # Get all descendants of the root coach
    subtree <- subcomponent(g, root, mode = "out")
    coaching_trees[[root]] <- names(subtree)
  }


  # Convert to a data frame for easy manipulation
  tree_df <- stack(coaching_trees) %>%
    rename(coach_id = values, tree_root = ind)

  # Compute shortest paths from each root coach to other coaches
  branch_levels <- data.frame()  # Ensure it's initialized properly

  for (root in root_coaches) {
    # Compute shortest paths from root to all other nodes
    dist_matrix <- distances(g, v = root, mode = "out")

    # Convert matrix to a long format
    dist_df <- as.data.frame(as.table(dist_matrix))
    colnames(dist_df) <- c("tree_root", "coach_id", "level")

    # Keep only meaningful results (excluding root itself if needed)
    dist_df <- dist_df %>% filter(level < Inf)  # Remove unreachable nodes

    # Append to branch_levels
    branch_levels <- rbind(branch_levels, dist_df)
  }


  branch_levels <- branch_levels %>% filter(level == 1) %>% mutate(tree_level = lvl)
  
  all_tree_data <- rbind(all_tree_data, branch_levels) %>% unique()

  lvl_1_coaches <- branch_levels %>% left_join(hc_tenures) %>% filter(hc_seasons >=5) %>% select(coach_id) %>% pluck(1)
  # child_coaches <- as.character(branch_levels$coach_id) %>% unique()
  # if(lvl == 1){
  #   new_coaches <- coaching_tree_2 %>% filter(!(coach_id %in% root_coaches))
  # }
  new_coaches <- coaching_tree_2 %>% filter(!(coach_id %in% root_coaches) & (coach_id %in% lvl_1_coaches))
  print(paste0(lvl, " done"))
  root_coaches <- setdiff(new_coaches$coach_id, new_coaches$tree_coach_id)
}
                                
dupes <- all_tree_data %>% 
  group_by(tree_root, coach_id) %>% 
  filter(row_number()<=1) %>% 
  ungroup() %>% 
  group_by(coach_id) %>% 
  mutate(count = n())

coach_levels <- all_tree_data %>% 
  select(tree_root, tree_level) %>% 
  unique() %>% 
  left_join(coach_ids,
            by = c("tree_root" = "coach_id")) %>% 
  group_by(tree_root) %>% 
  mutate(count = n()) %>% 
  arrange(desc(count), tree_root)# %>% 
  # ungroup() %>% 
  # mutate(tree_level_above = tree_level -1) %>% 
  # left_join(all_tree_data %>% select(tree_root = coach_id, tree_coach = tree_root, tree_level_above = tree_level) %>% unique(),
  #           by = c("tree_root" = "tree_root",
  #                  "tree_level_above" = "tree_level_above"))



# all_tree_data <- data.frame()
# root_coaches <- setdiff(coaching_tree_2$coach_id, coaching_tree_2$tree_coach_id)
# for(lvl in c(1:7)){
#   # Coaches who are never listed as tree_coach_id (they started a coaching tree)
#   print(paste("Start of iteration", lvl, ":"))
#   
#   # Use BFS to get coaching trees
#   coaching_trees <- list()
#   
#   for (root in root_coaches) {
#     # Get all descendants of the root coach
#     subtree <- subcomponent(g, root, mode = "out")
#     coaching_trees[[root]] <- names(subtree)
#   }
#   
#   
#   # Convert to a data frame for easy manipulation
#   tree_df <- stack(coaching_trees) %>%
#     rename(coach_id = values, tree_root = ind)
#   
#   # Compute shortest paths from each root coach to other coaches
#   branch_levels <- data.frame()  # Ensure it's initialized properly
#   
#   for (root in root_coaches) {
#     # Compute shortest paths from root to all other nodes
#     dist_matrix <- distances(g, v = root, mode = "out")
#     
#     # Convert matrix to a long format
#     dist_df <- as.data.frame(as.table(dist_matrix))
#     colnames(dist_df) <- c("tree_root", "coach_id", "level")
#     
#     # Keep only meaningful results (excluding root itself if needed)
#     dist_df <- dist_df %>% filter(level < Inf)  # Remove unreachable nodes
#     
#     # Append to branch_levels
#     branch_levels <- rbind(branch_levels, dist_df)
#   }
#   
#   
#   branch_levels <- branch_levels %>% filter(level == 1) %>% mutate(tree_level = lvl)
#   all_tree_data <- rbind(all_tree_data, branch_levels)
#   
#   child_coaches <- as.character(branch_levels$coach_id) %>% unique()
#   new_coaches <- coaching_tree_2 %>% filter(coach_id %in% child_coaches)
#   print(paste0(lvl, " done"))
#   root_coaches <- setdiff(new_coaches$coach_id, new_coaches$tree_coach_id)
# }


coaches_df <- read_csv(file.path(data_path, "coaching_trees.csv")) %>% 
  separate_rows(Role, sep = ", ") %>% 
  separate_rows(Role, sep = "/") %>% 
  unique() %>% 
  group_by(coach_id, type, Name) %>%
  summarise(Role = paste(Role, collapse = " / "), .groups = "drop") %>% 
  ungroup() %>% 
  select(Role, coach_id, type, Name) %>% arrange(Name)

create_coaching_graph <- function(df) {
  # For "Employed" relationships: the coach (Name) employs the coach_id
  employed_edges <- df %>%
    filter(type == "Employed") %>%
    select(from = Name, to = coach_id)
  
  # For "Worked For" relationships: the coach_id worked for the coach (Name)
  worked_for_edges <- df %>%
    filter(type == "Worked For") %>%
    select(from = coach_id, to = Name)
  
  # Combine edges
  all_edges <- bind_rows(employed_edges, worked_for_edges)
  
  # Create a graph
  g <- graph_from_data_frame(all_edges, directed = TRUE)
  
  return(g)
}

get_coaching_tree <- function(g, root_name) {
  # Get downstream coaches (those who worked under this coach or their descendants)
  # We use BFS (breadth-first search) to traverse the graph
  descendants <- bfs(g, root_name, mode = "out", unreachable = FALSE)$order
  descendants <- names(descendants)[!is.na(descendants)]
  
  return(descendants)
}


# Function to identify the most prominent coaches (potential tree roots)
identify_tree_roots <- function(g) {
  # A tree root is likely to have many outgoing connections and few incoming
  # or be at the "top" of the hierarchy with no incoming connections
  
  # Compute in-degree and out-degree for each node
  in_deg <- degree(g, mode = "in")
  out_deg <- degree(g, mode = "out")
  
  # Potential roots are nodes with high out-degree and low in-degree
  potential_roots <- names(out_deg)[out_deg > 0 & (in_deg == 0 | in_deg < out_deg)]
  
  return(potential_roots)
}

# Function to determine which tree each coach belongs to
assign_coaches_to_trees <- function(g, roots) {
  # For each coach, find which tree(s) they belong to
  all_vertices <- V(g)$name
  coach_trees <- list()
  
  for (coach in all_vertices) {
    coach_trees[[coach]] <- character(0)
    
    for (root in roots) {
      tree_members <- get_coaching_tree(g, root)
      if (coach %in% tree_members) {
        coach_trees[[coach]] <- c(coach_trees[[coach]], root)
      }
    }
  }
  
  # Convert to data frame
  result <- data.frame(
    coach = names(coach_trees),
    tree_root = sapply(coach_trees, function(x) paste(x, collapse = ", ")),
    stringsAsFactors = FALSE
  )
  
  return(result)
}

get_tree_members <- function(g, root) {
  tree_members <- get_coaching_tree(g, root)
  
  # Remove the root from its own tree members
  tree_members <- tree_members[tree_members != root]
  
  return(data.frame(
    tree_root = root,
    tree_member = tree_members,
    stringsAsFactors = FALSE
  ))
}

# Create the graph
coaching_graph <- create_coaching_graph(coaches_df)

# Identify potential roots of coaching trees
root_coaches <- identify_tree_roots(coaching_graph)
cat("Identified potential coaching tree roots:", paste(root_coaches, collapse=", "), "\n\n")

# Assign coaches to their respective trees
coach_tree_assignments <- assign_coaches_to_trees(coaching_graph, root_coaches)
print("Coach Tree Assignments:")
print(coach_tree_assignments)

# For each identified tree root, list all members of their tree
all_trees <- lapply(root_coaches, function(root) {
  get_tree_members(coaching_graph, root)
})

all_trees_df <- do.call(rbind, all_trees)
print("\nCoaching Tree Members:")
print(all_trees_df)

# Optional: Visualize the coaching graph
# if (requireNamespace("ggraph", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
#   library(ggraph)
#   library(ggplot2)
#   
#   ggraph(coaching_graph, layout = "fr") +
#     geom_edge_link(arrow = arrow(length = unit(2, "mm")), 
#                    end_cap = circle(2, "mm")) +
#     geom_node_point(size = 3) +
#     geom_node_text(aes(label = name), repel = TRUE) +
#     theme_graph() +
#     labs(title = "NFL Coaching Relationships")
# }

# Creating more comprehensive analysis functions for a larger dataset
# Function to find the path between coaches
find_coaching_path <- function(g, from_coach, to_coach) {
  path <- shortest_paths(g, from = from_coach, to = to_coach, 
                         output = "vpath")$vpath[[1]]
  path_names <- names(path)
  
  return(path_names)
}

# Function to calculate tree size
calculate_tree_size <- function(g, roots) {
  tree_sizes <- sapply(roots, function(root) {
    length(get_coaching_tree(g, root)) - 1  # Subtract 1 to exclude the root itself
  })
  
  return(data.frame(
    root = roots,
    tree_size = tree_sizes,
    stringsAsFactors = FALSE
  ))
}

# Example of additional analysis
# (Uncomment and run if you have a substantial dataset)
tree_sizes <- calculate_tree_size(coaching_graph, root_coaches)
# print("Coaching Tree Sizes:")
# print(tree_sizes[order(-tree_sizes$tree_size), ])
