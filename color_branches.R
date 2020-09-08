 color_branches <-  function (tree, k = NULL, h = NULL, col, groupLabels = NULL, 
                     warn = dendextend_options("warn"), ...) 
{
  old_labels <- labels(tree)
  labels_arent_unique <- !all_unique(old_labels)
  if (labels_arent_unique) {
    if (warn) 
      warning("Your tree labels are NOT unique!\n This may cause an un expected issue with the color of the branches.\n Hence, your labels were temporarily turned unique (and then fixed as they were before).")
    labels(tree) <- seq_along(old_labels)
  }
  if (missing(col)) 
    col <- rainbow_fun
  if (is.null(k) & is.null(h)) {
    if (warn) 
      warning("k (number of clusters) is missing, using the tree size as a default")
    k <- nleaves(tree)
  }
  if (!is.dendrogram(tree) && !is.hclust(tree)) 
    stop("tree needs to be either a dendrogram or an hclust object")
  g <- dendextend::cutree(tree, k = k, h = h, order_clusters_as_data = FALSE, 
                          sort_cluster_numbers = TRUE)
  if (is.hclust(tree)) 
    tree <- as.dendrogram(tree)
  k <- max(g)
  if (k == 0L) {
    if (warn) 
      warning("Tree has only one level - returning the dendrogram with no colors.")
    return(tree)
  }
  if (is.function(col)) {
    col <- col(k)
  }
  else {
    if (length(col) < k) {
      warning("Length of color vector was shorter than the number of clusters - color vector was recycled")
      col <- rep(col, length.out = k)
    }
    if (length(col) > k) {
      warning("Length of color vector was longer than the number of clusters - first k elements are used")
      col <- col[seq_len(k)]
    }
  }
  if (!is.null(groupLabels)) {
    if (length(groupLabels) == 1) {
      if (is.function(groupLabels)) 
        groupLabels = groupLabels(seq.int(length.out = k))
      else if (is.logical(groupLabels)) {
        if (groupLabels) 
          groupLabels = seq.int(length.out = k)
        else groupLabels = NULL
      }
    }
    if (!is.null(groupLabels) && length(groupLabels) != k) 
      stop("Must give same number of group labels as clusters")
  }
  addcol <- function(dend_node, col) {
    if (is.null(attr(dend_node, "edgePar"))) {
      attr(dend_node, "edgePar") <- list(col = col)
    }
    else {
      attr(dend_node, "edgePar")[["col"]] <- col
    }
    unclass(dend_node)
  }
  descendTree <- function(sd) {
    groupsinsubtree = unique(g[labels(sd)])
    if (length(groupsinsubtree) > 1) {
      for (i in seq(sd)) sd[[i]] <- descendTree(sd[[i]])
    }
    else {
      sd = dendrapply(sd, addcol, col[groupsinsubtree])
      if (!is.null(groupLabels)) {
        attr(sd, "edgetext") = groupLabels[groupsinsubtree]
        attr(sd, "edgePar") = c(attr(sd, "edgePar"), 
                                list(p.border = col[groupsinsubtree]))
      }
    }
    unclass(sd)
  }
  if (!is.character(labels(tree))) 
    labels(tree) <- as.character(labels(tree))
  tree <- descendTree(tree)
  class(tree) <- "dendrogram"
  if (labels_arent_unique) 
    labels(tree) <- old_labels
  tree
}
# <environment: namespace:dendextend>
  