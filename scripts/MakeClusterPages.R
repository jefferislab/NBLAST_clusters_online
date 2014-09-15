source("CommonStartup.R")

all_clusters <- apresdf$cluster
valid_clusters <- unique(all_clusters[apresdf$exemplar %in% exnotol])

for(cluster in valid_clusters) {
  # Pull out exemplar and other neurons in this cluster
  cluster_neurons <- apresdf[apresdf$cluster == cluster, 'item']
  exemplar <- unique(as.character(apresdf[apresdf$cluster == cluster, 'exemplar']))
  supercluster_num <- which(sapply(exemplars_by_supercluster, function(x) exemplar %in% x))
  exemplar_neuron <- fc_neuron(exemplar)
  others <- setdiff(cluster_neurons, exemplar)

  # Find similar exemplars
  exemplar_scores <- fc_nblast(exemplar, exemplars, scoremat=allbyallmem, normalisation='norm')
  # Take either the top 5 or all those > 0.5 (if more than 5)
  exemplar_scores <- sort(exemplar_scores, decreasing=TRUE)
  threshold <- exemplar_scores[7]
  if(threshold > 0.5) threshold <- 0.5
  similar_exemplars <- names(which(exemplar_scores > threshold))
  similar_exemplar_neurons <- fc_neuron(similar_exemplars)
  similar_exemplar_superclusters <- sapply(similar_exemplars, function(x) which(sapply(exemplars_by_supercluster, function(y) x %in% y)))
  similar_cluster_ids <- sapply(similar_exemplars, function(x) apresdf$cluster[which(apresdf$exemplar == x)[1]])
  # Build a data frame for xtable to display in the brewed document
  similar_exemplars_df <- data.frame(cluster=similar_cluster_ids, exemplar=similar_exemplar_neurons, supercluster=similar_exemplar_superclusters, norm_score=exemplar_scores[similar_exemplars])

  # Create directory for cluster and brew twice, once for PNG snapshot display, and once for WebGL display
  cluster_path <- file.path("..", "clusters", cluster)
  dir.create(cluster_path, recursive=TRUE)
  cluster_view <- '<a href= "webgl.html"><img src="snapshot.png" alt="Click for 3D rendering of cluster" title="Click image for 3D rendering of cluster" class="center-block img-responsive"></a><br /><button type="button" class="btn btn-lg btn-default active">2D</button><a href="webgl.html"><button type="button" class="btn btn-lg btn-default">3D</button></a><br />'
  brew(file.path("..", "templates", "cluster_template.html"), file.path(cluster_path, "index.html"))
  cluster_view <- '%WebGL%\n<br /><a href="index.html"><button type="button" class="btn btn-lg btn-default">2D</button></a><button type="button" class="btn btn-lg btn-default active">3D</button><br />'
  brew(file.path("..", "templates", "cluster_template.html"), file.path(cluster_path, "webgl.html"))

  # Make the WebGL scene and save into the brewed template
  clear3d()
  plot3d(dps[exemplar], col='black', lwd=2, soma=TRUE)
  plot3d(dps[others], soma=TRUE)
  plot3d(FCWB)
  frontalView()
  writeWebGL(dir=cluster_path, filename=file.path(cluster_path, 'webgl.html'), template=file.path(cluster_path, "webgl.html"))
  # Also save lower-resolution image for supercluster display page
  par3d(windowRect=c(150,150,150+384,150+288))
  Sys.sleep(0.5)
  rgl.snapshot(file.path(cluster_path, "smaller.png"))
  Sys.sleep(0.5)
  par3d(windowRect=c(150,150,150+1024,150+768))
}
