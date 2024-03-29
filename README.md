# Concept Mover's Distance: Reproduction Guide

[Dustin S. Stoltz](https://www.dustinstoltz.com) and [Marshall A. Taylor](https://www.marshalltaylor.net)

----
If you are interested in using Concept Mover's Distance, please use the following R package:

install.packages("[text2map](https://cran.r-project.org/package=text2map)")

And, see the [`CMDist` vignette](https://culturalcartography.gitlab.io/text2map/articles/CMDist-concept-movers-distance.html).

----

This is the original code and data to reproduce the measures, graphs, and plots for Stoltz and Taylor (2019) "Concept Mover's Distance," forthcoming in the _Journal of Computational Social Science_. A preprint is available on SocArxiv at [https://osf.io/preprints/socarxiv/5hc4z/](https://osf.io/preprints/socarxiv/5hc4z/).

In the paper, we propose a method for measuring a text's engagement with a focal concept  using  distributional  representations  of  the  meaning  of  words. In our approach, which we call __Concept Mover's Distance__, a document is measured by the minimum distance the words in the document need to travel to arrive at the position of a "pseudo document" consisting of only words denoting a focal concept.

To reproduce the figures in the paper, download all scripts and CSVs to a local folder, and load the packages in the 1_cmd_jcss_packages.R script. The remaining scripts are self-contained, and refer to the respective section of the paper. Some of the figures require downloading text from Project Gutenberg which may take some time. 

**Note that the `CMDist` function in this repository is the original, and is likely outdated.**

-------------------------------------------------------------------
