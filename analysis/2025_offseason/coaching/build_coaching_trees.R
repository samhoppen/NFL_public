library(tidyverse)
library(dplyr)
library(igraph)
library(ggplot2)
library(ggraph)
library(tidygraph)

coaches <- read_csv("~/Documents/Data/yearly_coaching_history.csv") %>% 
  mutate(coach_id = gsub("/executives/", "", coach_id))

coaching_history <- read_csv("~/Documents/Data/coaching_histories.csv")
coaching_tree <- read_csv("~/Documents/Data/coaching_trees.csv")

coaching_tenures <- coaching_history %>% 
  filter(Level == "NFL") %>% 
  group_by(coach_id) %>% 
  summarize(hc_seasons = sum(Role == "Head Coach"))


coaching_tree_v2 <- coaching_tree %>% 
  separate_rows(Role, sep = ", ") %>% 
  separate_rows(Role, sep = "/") %>% 
  unique() %>% 
  group_by() %>%
  group_by(coach_id, type, Name) %>%
  summarise(Role = paste(Role, collapse = " / "), .groups = "drop")

coaching_tree_v2 %>% filter(type == 'Employed') %>% head(20) %>% clipr::write_clip()

# split the coaching tree Role by comma-separated values
coach_ids <- coaches %>% 
  select(coach, coach_id) %>% 
  unique()

fin_coaches = coaching_tree_v2 %>% 
  left_join(coach_ids) %>% 
  left_join(coach_ids %>% select(Name = coach, tree_coach_id = coach_id)) %>% 
  group_by(Name, coach_id, type) %>% 
  mutate(count = n()) %>% 
  filter(Name != "Jim Mora") %>% 
  ungroup() %>% 
  filter(type == 'Employed') %>% 
  select(coach_id, coach_name = coach, tree_coach_id, tree_coach_name = Name, tree_coach_role = Role)

coach_stats <- fin_coaches %>% 
  group_by(coach_id, coach_name) %>% 
  summarize(count = n_distinct(tree_coach_id)) %>% 
  arrange(desc(count)) %>% 
  left_join(coaching_tenures) %>% 
  filter(hc_seasons >= 5, count >= 6)


# Load your dataframe (assuming it's named df)
edges <- fin_coaches %>%
  select(coach_id, tree_coach_id) %>%
  filter(!is.na(tree_coach_id))  # Ensure no NA values

# Create directed graph
g <- graph_from_data_frame(edges, directed = TRUE)

fin_tree <- data.frame()
for(i in c(1:length(g))){
  parent_coach <- names(g[[i]])  
  for(j in c(1:length(g[[i]][[1]]))){
    child_coach = g[[i]][[1]][[j]]$name
    temp_df <- data.frame(parent_coach_id = c(parent_coach),
                          child_coach_id = c(child_coach))
    
    fin_tree <- rbind(fin_tree, temp_df)
  }
  print(paste0(parent_coach, " done"))
  
}

# Coaches who are never listed as tree_coach_id (they started a coaching tree)
root_coaches <- setdiff(fin_coaches$coach_id, fin_coaches$tree_coach_id)

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

closest <- branch_levels %>% 
  group_by(coach_id) %>% 
  filter(level == min(level)) %>% 
  ungroup()

lvl_1 = branch_levels %>% 
  filter(level == 1)

lvl_2 = branch_levels %>% 
  filter(level == 2)

test = fin_tree %>% 
  left_join(closest,
            by = c("parent_coach_id" = "tree_root", 
                   "child_coach_id" = "coach_id")) %>% 
  filter(!is.na(level))

### NEXT LEVEL OF COACHING TREES ###
fin_coaches2 <- fin_coaches %>% filter(!(coach_id %in% root_coaches))
# Coaches who are never listed as tree_coach_id (they started a coaching tree)
root_coaches2 <- setdiff(fin_coaches2$coach_id, fin_coaches2$tree_coach_id)

# Use BFS to get coaching trees
coaching_trees2 <- list()

for (root in root_coaches2) {
  # Get all descendants of the root coach
  subtree <- subcomponent(g, root, mode = "out")
  coaching_trees2[[root]] <- names(subtree)
}


# Convert to a data frame for easy manipulation
tree_df2 <- stack(coaching_trees2) %>%
  rename(coach_id = values, tree_root = ind)

# Compute shortest paths from each root coach to other coaches
branch_levels2 <- data.frame()  # Ensure it's initialized properly

for (root in root_coaches2) {
  # Compute shortest paths from root to all other nodes
  dist_matrix2 <- distances(g, v = root, mode = "out")
  
  # Convert matrix to a long format
  dist_df2 <- as.data.frame(as.table(dist_matrix2))
  colnames(dist_df2) <- c("tree_root", "coach_id", "level")
  
  # Keep only meaningful results (excluding root itself if needed)
  dist_df2 <- dist_df2 %>% filter(level < Inf)  # Remove unreachable nodes
  
  # Append to branch_levels
  branch_levels2 <- rbind(branch_levels2, dist_df2)
}

closest2 <- branch_levels2 %>% 
  group_by(coach_id) %>% 
  filter(level == min(level)) %>% 
  ungroup()


### NEXT LEVEL OF COACHING TREES ###
fin_coaches3 <- fin_coaches2 %>% filter(!(coach_id %in% root_coaches2))
# Coaches who are never listed as tree_coach_id (they started a coaching tree)
root_coaches3 <- setdiff(fin_coaches3$coach_id, fin_coaches3$tree_coach_id)

# Use BFS to get coaching trees
coaching_trees3 <- list()

for (root in root_coaches3) {
  # Get all descendants of the root coach
  subtree <- subcomponent(g, root, mode = "out")
  coaching_trees3[[root]] <- names(subtree)
}


# Convert to a data frame for easy manipulation
tree_df3 <- stack(coaching_trees3) %>%
  rename(coach_id = values, tree_root = ind)

# Compute shortest paths from each root coach to other coaches
branch_levels3 <- data.frame()  # Ensure it's initialized properly

for (root in root_coaches3) {
  # Compute shortest paths from root to all other nodes
  dist_matrix3 <- distances(g, v = root, mode = "out")
  
  # Convert matrix to a long format
  dist_df3 <- as.data.frame(as.table(dist_matrix3))
  colnames(dist_df3) <- c("tree_root", "coach_id", "level")
  
  # Keep only meaningful results (excluding root itself if needed)
  dist_df3 <- dist_df3 %>% filter(level < Inf)  # Remove unreachable nodes
  
  # Append to branch_levels
  branch_levels3 <- rbind(branch_levels3, dist_df3)
}

closest3 <- branch_levels3 %>% 
  group_by(coach_id) %>% 
  filter(level == min(level)) %>% 
  ungroup()
