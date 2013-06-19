# Copyright (C) 2011 Andrew Ziem
# Licensed under the GNU General Public License version 2 or later <https://www.gnu.org/licenses/gpl-2.0.html>

# get node ID for left child
btree_left <- function(mytree, parent_id)
{
  nodes(mytree, parent_id)[[1]]$left$nodeID
}

# get right child
btree_right <- function(mytree, parent_id)
{
  nodes(mytree, parent_id)[[1]]$right$nodeID
}

# get prediction for this node
btree_prediction <- function(mytree, node_id)
{
  p <- nodes(mytree, node_id)[[1]]$prediction
  if (2 == length(p)) {
    return(p[2])
  }
  return (p)
  
}

# criteria for this node as a string
btree_criteria <- function(mytree, node_id, left)
{
  if (nodes(mytree, node_id)[[1]]$terminal)
  {
    return("(error: terminal node)");
  } 
  if (nodes(mytree, node_id)[[1]]$psplit$ordered)
  {
    sp <- nodes(mytree, node_id)[[1]]$psplit$splitpoint
    vn <- nodes(mytree, node_id)[[1]]$psplit$variableName
    if (left) {
      op <- '<='   
    } else {
      op <- '>'
    }
    return(paste(vn, op, sp))
  } else {
    psplit <- nodes(mytree, node_id)[[1]]$psplit
    if (left){
      l <- as.logical(psplit$splitpoint)
    } else {
      l <- as.logical(!psplit$splitpoint)
    }
    
    r <- paste(attr(psplit$splitpoint, 'levels')[l], sep='', collapse="','")
    return(paste(psplit$variableName, " in ('", r,"')", sep=''))
  }
}

walk_node <- function(mytree, node_id = 1, parent_criteria = character(0))
{
  if (nodes(mytree, node_id)[[1]]$terminal) {
    prediction <- btree_prediction(mytree, node_id)
    sprediction <- paste('else if', parent_criteria, 'then prediction =',prediction,';')
    return (sprediction)
  }
  
  left_node_id <- btree_left(mytree, node_id)
  right_node_id <- btree_right(mytree, node_id)
  
  if (is.null(left_node_id) != is.null(right_node_id)) {
    print('left node ID != right node id')
  }
  sprediction <- character(0)
  if (!is.null(left_node_id)) {
    new_criteria <- paste(parent_criteria, btree_criteria(mytree, node_id, T), sep=' and ')
    if (1 == node_id)
      new_criteria <- btree_criteria(mytree, node_id, T)
    sprediction <- walk_node(mytree, left_node_id, new_criteria)
  }
  if (!is.null(right_node_id)) {
    new_criteria <- paste(parent_criteria, btree_criteria(mytree, node_id, F), sep=' and ')
    if (1 == node_id)
      new_criteria <- btree_criteria(mytree, node_id, F)
    sprediction <- paste(sprediction, walk_node(mytree, right_node_id, new_criteria), sep='\n')
  }
  return(sprediction)
}

# demonstration
require(party)
airq <- airquality[complete.cases(airquality),]
airct <- ctree(Ozone ~ ., data = airq)
sprediction <- walk_node(airct)