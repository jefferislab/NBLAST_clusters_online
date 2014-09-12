source("CommonStartup.R")

supercluster_snapshot_dir <- file.path("..", "images", "superclusters")
dir.create(supercluster_snapshot_dir, recursive=TRUE)

# Plot dendrogram
exnotolhcc <- colour_clusters(exnotolhc, k=num_superclusters, groupLabels=as.roman)
labels(exnotolhcc)=NULL
par(mar=c(0,2,0,0))
png(file.path(supercluster_snapshot_dir, "dendrogram.png"), width=1170)
plot(exnotolhcc)
abline(h=height_for_ngroups(exnotolhc, k=num_superclusters), lty='dashed')
dev.off()

for(supercluster in 1:num_superclusters) {
  supercluster_neurons <- subset(exnotolhc, k=num_superclusters, groups=supercluster)
  supercluster_clusters <- unique(apresdf[apresdf$item %in% supercluster_neurons, 'cluster'])

  # Create directory for supercluster and brew twice, once for PNG snapshot display, and once for WebGL display
  supercluster_path <- file.path("..", "superclusters", supercluster)
  dir.create(supercluster_path, recursive=TRUE)

  supercluster_view <- '<a href= "webgl.html"><img src="snapshot.png" alt="Click for 3D rendering of supercluster" title="Click image for 3D rendering of supercluster" class="center-block img-responsive"></a><br /><button type="button" class="btn btn-lg btn-default active">2D</button><a href="webgl.html"><button type="button" class="btn btn-lg btn-default">3D</button></a><br />'
  brew(file.path("..", "templates", "supercluster_template.html"), file.path(supercluster_path, "index.html"))
  supercluster_view <- '%WebGL%\n<br /><a href="index.html"><button type="button" class="btn btn-lg btn-default">2D</button></a><button type="button" class="btn btn-lg btn-default active">3D</button><br />'
  brew(file.path("..", "templates", "supercluster_template.html"), file.path(supercluster_path, "webgl.html"))

  # Make the WebGL scene and save into the brewed template
  clear3d()
  plot3d(dps[supercluster_neurons], soma=TRUE)
  plot3d(FCWB)
  frontalView()
  writeWebGL(dir=supercluster_path, filename=file.path(supercluster_path, 'webgl.html'), template=file.path(supercluster_path, "webgl.html"))

  # Also save a lower resolution snapshot for the landing page
  clear3d()
  plot3d(dps[supercluster_neurons], soma=TRUE, col=rainbow(num_superclusters)[supercluster])
  plot3d(FCWB)
  frontalView()
  par3d(windowRect=c(150,150,150+384,150+288))
  Sys.sleep(0.5)
  rgl.snapshot(file.path(supercluster_snapshot_dir, paste0(supercluster, ".png")))
  Sys.sleep(0.5)
  par3d(windowRect=c(150,150,150+1024,150+768))
}

# Make landing page
brew(file.path("..", "templates", "index_template.html"), file.path("..", "index.html"))
