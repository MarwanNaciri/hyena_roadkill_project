major_roads: primary and secondary roads that cross the SNP. 
		Cropped at the border of the SNP

snp.roads.snp_roads_for_analyses_25-05-2021: the roads that were not on the 2013 paper map were removed. 
					     Road portions were fused together so as to minimize the 
					     number of independent lines.
					     Towards the east of the SNP, a carcass was found on one
					     of the roads (let's call it road A) that is not on the paper map. 
					     This road was removed and is absent from this shapefile.
					     Towards the west of the SNP three roads absent from the OSM map
					     but present on the paper map were added because a few carcasses 
					     were found there.
                                             Roads that were outside of SNP were removed (even if they were
                                             within the 65km-radius circle)
					     This file was created in May 2021 to replace the existing one which
	                                     had roads outside of the Serengeti NP (i.e. in the neighbouring 
	                                     protected areas), that were very far from where the hyena project
	                                     operates, and where there is little chance that roadkills would
                                             have been detected. The existinf files are in the "OLD" folder.

snp_roads_according_to_FZS_map: 16-09-2020 this is the road network that corresponds best to the one in the 
                                Frankfurt Zoological Society map (Sarah's map, she sent me photos on whatsapp).
                                Some roads were missing in the OSM data, so I had to add them. To do that, we
	 	                I used satelitte images as a background on QGIS and added lines over the 
                                corresponding roads.
                                BUT: some roads were not on satelitte images, so I could not add them. I suppose
                                     this is why these roads were not picked-up by OSM
                                                            

snp_roads_according_to_FZS_map_cropped: same as above except cropped with the study-area polygon I built on 
                                        16-09-2020 (see in the "border" folder)

major_roads_cropped: same as above except only the main road and the secondary roads are kept.
				





	


