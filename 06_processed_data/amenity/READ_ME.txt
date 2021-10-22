building_points_for analyses_2020-09-11: point buildings of the SNP and the surroundings. To use
			                    to calculate predictors for the GLM. Not heavy.
					    Use to plot after cropping as well.
					    Some points that could have been relevant may have been
		                            deleted. But considering that point buildings are smh 
                                            redundant with polygon buildings, and that they are far
                                            less numerous, I don't think it matters. 
                                            Since I can't remember where I donwloaded this file (not
                                            from Geofabrik) I'll use it as it is now.

			
buildings_polygons_for_analyses_2021-05-25: polygons buildings of the SNP and the surrounding. I only
                                           kept buildings that were not too far from the roads that 
                                           are considered to calculate the predictors for the GLM, so 
                                           as to lower the computational burden.

buildings_polygons_for_analyses_2021-05-25_bis_bis: same thing as "buildings_polygons_for_analyses_2021-05-25"
                                                   except many more buildings were kept. Not useful a priori, but
                                                   I kept it in case I need to include more buildings in 
	                                           the calculation of the predictor, as this file would allow me
                                                   not to start again from the super large shapefile that contains
                                                   all the buildings of Tanzania (super long to load, and to 
                                                   process).


buildings.polygons.cropped.65km.circle.09-11: polygon buildings cropped with the SNP border and 
						with the 65km radius circle centered on the visitor 
						center. Use this shapefile to plot the map.

visitor.center : the visitor center of the Serengeti. Center of the 65km wide circle

amenity_polygon_plot_2021-09: amenity for the plots. I cropped the tanzania amenity data using a square that 
                              corresponds to the bbox of the roads + a margin