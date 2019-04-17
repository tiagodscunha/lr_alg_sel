# lr_alg_sel

Source code for Label Ranking approaches for Collaborative Filtering algorithm selection. It contains all experimental procedures used to empirically compare multiple Collaborative Filtering algorithm selection studies. The code can be used to replicate the experiments of the contributions presented in the following research papers:

- Cunha, T., Soares, C., & de Carvalho, A. C. P. L. F. (2018). A Label Ranking approach for selecting rankings of Collaborative Filtering algorithms. In ACM Symposium on Applied Computing (pp. 1393–1395).
- Cunha, T., Soares, C., & de Carvalho, A. C. P. L. F. (2018). Algorithm Selection for Collaborative Filtering: the influence of graph metafeatures and multicriteria metatargets. ArXiv E-Prints, 1–25. Retrieved from http://arxiv.org/abs/1807.09097

Furthermore, take into consideration that the metadata used is thoroughly explained in the following research papers:

- Cunha, T., Soares, C., & de Carvalho, A. C. P. L. F. (2016). Selecting Collaborative Filtering algorithms using Metalearning. In European Conference on Machine Learning and Knowledge Discovery in Databases (pp. 393–409).
- Cunha, T., Soares, C., & de Carvalho, A. C. P. L. F. (2017). Recommending Collaborative Filtering algorithms using subsampling landmarkers. In Discovery Science (pp. 189–203).
- Cunha, T., Soares, C., & de Carvalho, A. C. P. L. F. (2018). Metalearning and Recommender Systems: A literature review and empirical study on the algorithm selection problem for Collaborative Filtering. Information Sciences, 423, 128–144. 

The current code reflects an empirical comparison of such approaches to the scope of CF algorithm selection. The results reported here will be included in the PhD thesis. The code consists on a series of R scripts, aimed at evaluating the aforementioned frameworks on multiple evaluation scenarios. In order to simplify the experiments, the scripts represent individual experiments, each with a specific scope, aimed to be executed independently. In order to organize the code, a prefix number was used to identify the different steps in the meta-approaches:

- 1: create graph metafeatures (remaining are already pre-calculated and available in the respective folders as CSV files)
- 2: create multicriteria metatargets (proposed in https://arxiv.org/abs/1807.09097)
- 3: metalevel experiments evaluated with Kendall's tau (multiple variations exist, all dependent on the metafeatures and metatargets used)
- 4: script to create Kendall's tau graphs
- 5: impact on thee baselevel performance experiments (multiple variations exist, all dependent on the metafeatures and metatargets used)
- 6: script to create impact on thee baselevel performance graphics
- 7: Label Ranking metafeature importance procedure
- 8: script to assess correlations between regular and multicriteria metatargets
- 9: Critical Difference diagrams for statistical validation of results
- 10: Experiments to obtain detailed evaluation assessment to use in Critical Difference diagrams

The remaining items in this repository can be organized as follows:

Folders:
- labelrankingforests-master: source code with Label Ranking algorithms implementation and evaluation and tuning procedures (source code adapted from https://github.com/rebelosa/labelrankingforests)
- metafeatures_graph: graph-based CF metafeatures (proposed in https://arxiv.org/abs/1807.09097)
- metafeatures_landmarkers and metafeatures_landmarkers_all: subsampling landmarkers metafeatures (proposed in https://link.springer.com/chapter/10.1007/978-3-319-67786-6_14)
- metafeatures_statistical: statistical and information theoretical metafeatures (proposed in https://link.springer.com/chapter/10.1007/978-3-319-46227-1_25)
- results: metalevel evaluation results for all metamodels used
- performance: baselevel performance obtained in multiple evaluation measures: RMSE, NMAE, AUC and NDCG
- targets: multicriteria metatargets (proposed in https://arxiv.org/abs/1807.09097)

Other Files:
- auxiliary.R: auxiliary functions to process metadata and metatargets
- tuningCF.R: auxiliary functions used to perform hyperparameter tuning in CF4CF and CF4CF-META
- NEW_ALS.R: override function used to retrieve factorized matrices from recommenderlab's ALS
- RF_multioutput.R: auxiliary script used to instantiate a multi-output regressor to be used in our ALORS implementation
- results_paper.R: main script which loads the results from the folder and creates the visualizations 

Lastly, notice the datasets folder used in some scipts has been removed due to size restrictions and copyright ownership. Please refer to the appropriate references in the following paper to obtain the original sources to download the data: "Cunha, T., Soares, C., & de Carvalho, A. C. P. L. F. (2018). Algorithm Selection for Collaborative Filtering: the influence of graph metafeatures and multicriteria metatargets. ArXiv E-Prints, 1–25. Retrieved from http://arxiv.org/abs/1807.09097".
