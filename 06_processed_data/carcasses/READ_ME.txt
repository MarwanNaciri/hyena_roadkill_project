3_hy.clan.members: file with the GPS coordinates, date, sex, age and ID of all clan members.
		For females, the standardized rank is also present.

4_hy.clan.members.F: file used for analysis on the female clan members. For the most part, this file
			is similar to hy_clan_members_F_complete.

5_hy.carcasses.per.month.counts.obs.VS.exp: file with the expected number of carcasses per month (i.e.
						N/12) and the observed number of carcasses found 
						during each month.

6_prey.levels.distances.territories: file with the prey level in the territories of the studied clans at the 
					time of death each roadkilled hyena + the distance from the center of each
					territory to the carcass. No correlation was found

7_hy.carcasse.date.death: file with the date of observation, date of death and estimated day of death.


8_hy.carcasse.date.death.processed: file with processed date of death: if a day of death was indicated,
					it was used (instead of the date of observation). If it was possible
					to estimated the date of death (e.g. at least a week), the estimated
					day of death was used. Otherwise, the date of observation was used. 
					
9_clan.size.per.clan.per.year: file used to calculate the average sex ratio and age distribution in the three 
				studied clans over the 30 years.

10_hy.carcasses.certainety: file with the certainty scores of each carcass (in terms of cause of death and GPS
				coordinates/location).

11_hy.carcasses.certainty_with_comments_SB: file with the certainty scores of each carcass and the comments of
						Sarah and her suggested rectifications. The rectifications
						were implemented.

12_hy.carcasses.certainty.formatted.spatial : file with all the known carcasses and the certainty score
						as determined on 13/08/2020 after implementing the 
						rectifications Sarah suggested. This file is used in the
						GLM analysis.

hy_clan_members_F_complete: table with all the female roadkill victims belonging to the studied
				clans. The prey level in their territory at the time of their death, 
				the distance to their territory when they died is indicated. 

table_01 : table with all spoted hyena carcasses suspected to be from roadkills.
		roadkills of other species, duplicates, and hyenas killed in other
		ways were removed.

table_Roadkill_Marwan:  Carcasses with very vague approximate locations or approximate locations 
			that were impossible for me to interprete. 

table_Roadkill_Marwan_ME: Indications from Marion about the carcasses mentionned above.

hy_carcasses: shapefile with all the hyena carcasses (exact + estimated GPS coordinates)
