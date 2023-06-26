# Point-Based-Tennis-Prediction

## Code

This directory contains various R, R Markdown, Excel, and Jupyter notebook files. The Jupyter notebooks are only used for aggregating and extracting data (see Data Lake section below), while main analysis was done with R. The Excel files mainly document results, but also contain some aggregatation functions and highlighting of cells. See descriptions below for which files contain code ran to obtain results in paper. If there are further inquries, please reach out here or through email.

### R & Excel

Among the R and R Markdown files, the 7 prediction notebooks, provided for reference, were the ones originally used for the study. Correspondingly, they are not very streamlined and in some cases contain outdated code. Overall, notebook 3 is the only notebook which is suitable for direct use. Notebook 1, including its 1m and 1n variants, can also be run for finalized results, but many of its accompanying comments are outdated.

| File | Description |
| ------ | -------------------------- |
| diff_p_results.xlsx | Records all experimental results pertaining to plugging different $p$ estimates into the baseline model. This includes overlays and merges. |
| case_patching.R | Contains the match win probability functions that take into account the different Grand Slam rules through the years. Given to and ran in all final versions of the files that use the model. |
| cat2_for_paper.Rmd | Consolidates and organises work done in prediction notebooks 2 and 5 on game-scenario-specific $p$'s. Results obtained here were directly included in the paper, with the exception of the plots which were replotted using ggplot2 in "figure_2_4_code.Rmd". | 
| figure_2_4_code.Rmd, figure_3_code.Rmd, figure_5_code.Rmd | Plots data using ggplot2 and Plotly. |
| breakpoint_q_experiment.Rmd | Obtains breakpoint - $q$ results in Appendix B. |
| yby_for_paper.Rmd | In "prediction_notebook_6" directory, streamlines the year-by-year experiments in Appendix B. |
| oaf_flips_players.Rmd | Examines players in the "oaf_flips" directory to obtain table in Appendix B. |
| distance_examine_atp.xlsx, distance_examine_wta.xlsx | Contains records of matches with long distance ran and the tournament results of the players. Aggregate results are derived in the spreadsheet and reported in Appendix C. |
| overlays.Rmd | Loads data obtained in notebooks 1 (including 1m and 1n) and 4, as well as certain sections of "scrap_paper" detailed below. Produces final results for overlays and merges. |
| scrap_paper.Rmd | Used as digital scrap paper for small experiments outside of the notebooks. All experiments are separated by level 1 headers, and some can only be run after certain parts of code in the notebooks are run. Some experiments were eventually separated into their own R Markdown files, such as "overlays" and "breakpoint_q_experiment. Among the ones still here, the sections "CO2", "CO2(ns)", and "CO(OAF)" were used to get results that went into "diff_p_results" and "overlays". These sections can be independently run. |
| prediction_notebook_1.Rmd, prediction_notebook_1m.Rmd, prediction_notebook_1n.Rmd | Notebooks used for the majority of experiments using different $p$ estimates. "1" is used for the 2019-2022 time period, while "1m" and "1n" are exact copies of "1", with "1m" accounting for 2015-2018 and "1n" for 2011-2014. |
| prediction_notebook_2.Rmd | Notebook used for initial experiments on game scenarios. Organized and re-ran in "cat2_for_paper". |
| prediction_notebook_3.Rmd | Notebook used for the 3 experiments on fatigue in Appendix C. The only one of the notebooks which is organized enough for direct reading. |
| prediction_notebook_4.Rmd | Initially used for experiments regarding CO2 and CO(OAF). All experiments in this file were redone later (mostly though "scrap_paper" and "overlays") and none of its results are directly used. |
| prediction_notebook_5.Rmd | Notebook used for initial drilldown into the plateaus of notebook 2. Organized and re-ran in "cat2_for_paper". |
| prediction_notebook_6 | Directory and notebook used for the first run of Appendix B's year-by-year experiments. Organized and re-ran in "yby_for_paper". |
| prediction_notebook_7 | Directory and notebook used for further experiments focusing on only one player, Denis Shapovalov, during a relatively short time frame. This resulted in too small of a sample size to draw conclusions and was subsequently not followed up on nor included in the paper. |

### Python / Jupyter Notebooks

With the excpetion of "point_by_point_merge_and_distill", all resulting files are stored under the "Players" directory.

| File | Description |
| ------ | -------------------------- |
| player_statistics_extraction | Extracts statistics from the files in the "Matches" directory. Namely service points served and won, receiving points received and won, and number of aces for each surface. List of names assigned to 'players' is generated using code under the "Names for Python Script" section of "scrap_paper.Rmd". |
| match_specific_h2h_co_extraction | Filters for and extracts H2H and CO statistics from files in the "Matches" directory. Also records surface-specific sample size for each match-up, though this was not used in the study. |
| match_specific_co_extraction_2, match_specific_co_extraction_2ns | Filters for and extracts CO2 statistics from files in the "Matches" directory. "ns" stands for non-surface-specific, referring to the approach of taking surface-specific CO lists, but then ignoring surface when taking data from matches containing those COs. The file without "ns" is correspondingly always surface-specific. |
| point_by_point_merge_and_distill | In preparation for point-by-point statistics extraction, merges match information in the matches files and points files, both in the "Point-by-Point" directory, using "match_id" as the key. Only score and point server data were kept in the resulting files, but more features can be specified in 'f_fields'. Resulting files are stored with "pbp" suffixes along with the point-by-point data. |
| point_by_point_player_extraction | Uses the results of the previous notebook to extract surface-specific game-scenario-specific/point-by-point statistics. |
| point_by_point_player_extraction_set5 | Same as above, but takes only from either 5th-set data or non-5th-set data. |
| breakpoint_q_player_extraction | Uses the "pbp" data from "merge_and_distill" to extract the breakpoint statistics needed in Appendix B. |
| accumulated_fatigue_factors_extraction | Creates columns for accumulated rallies played, points played, and distance ran after each point throughout a match. Results are stored in 3 separate files along with accompanying data such as match_id and time elapsed. Takes a very long time to run compared to all other notebooks. |

## Data Lake

(The directory "Data Lake" is called such since it was also used to store various data files that were ultimately not used for the study. The name cannot be easily changed as it is used in the majority of Python and R notebooks.)

"Experiments" contains the data sets used for testing. "Matches" contains records of matches played during 2011-2022 along with aggregate statistics for both players. "Players" contains the results of the various Jupyter Notebooks which is then fed to the model. "Point-by-Point" contains Grand Slam match information and point-by-point progressions for each of the matches.

The great majority of the data used in the study is obtained from [Jeff Sackman’s GitHub](https://github.com/JeffSackmann) and filtered/aggregated further using the Jupyter notebooks. The resulting data is provided directly, but can also be obtained with Jeff Sackmann’s data and the Jupyter notebooks which are also provided, However, one final process that was done manually (and reflected in the uploaded data) is correcting for names of certain players. The name formatting the point-by-point data sets use differ depending on tournament. For example, Alejandro Davidovich Fokina can have records under "Alejandro Davidovich Fokina", "A Davidovich Fokina", "A. Davidovich Fokina", "Alejandro Davidovich-Fokina" etc. Most of these names are dealt with using the "get.equiv.names" function when relevant matches come up in testing, but we used manual entry to deal with further cases including the ones listed below (non-exhaustive list meant to be representative of different cases):

- Stan Wawrinka: Had some records under names with first name Stanislas.
- Mackenzie McDonald: The first "D" in the last name was sometimes capitalized and sometimes not.
- Alex De Minaur: Same case as "McDonald", capitalization of the "D" in "De" was inconsistent.
- Carla Suárez Navarro, Garbiñe Muguruza, and Alizé Cornet: Some records used the accented characters (which were not always able to be read) while others substituted with the non-accented equivalent.
- Karolína Plíšková and Kristýna Plíšková: This was the only case we found where two players' first names shared the first letter, and the two players had the same last name as well. Consequently, records under "K. Plíšková" had to be filled in with full names before the data was fed to Python.
- Jan-Lennard Struff, Yen-Hsun Lu, Jo-Wilfried Tsonga: "get.equiv.names" did not have a rule to deal with players with hyphenated first names such as these three. A rule could be added, but for our study we dealt with them manually in the processed data.
- J. J. Wolf: A special case worth noting here where the abbreviation is consistently used. We did not do any additional processing in this case.

Any files with "atp_bpq" (for breakpoint - $q$) contain data scraped from [UltimateTennisStatististics](https://www.ultimatetennisstatistics.com/). Both this site and Sackmann’s data are licensed under [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-nc-sa/4.0/). According to its ShareAlike term, data provided here falls under the same license.

## Misc.

Even though the language breakdown describes the project as almost all Jupyter notebook, the majority of analysis is done in R and stored in the various .Rmd files. This issue is outlined here: https://github.com/github-linguist/linguist/issues/5208. Python is only used for preparing the data for analysis and extracting aggregate stats. The real language breakdown would be in the ballpark of 70% R and 30% Python.
