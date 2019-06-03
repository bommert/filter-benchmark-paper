# Experiments: Files for running the experiments on a high performance compute cluster

mlr_2.12.tar.gz: version of mlr that was used to run all experiments

task_generation.R: creates tasksAndRins.RData and data_info.RData (necessary for experiments)

filter_benchmark_experiments.R: definition of batchtools experiments for benchmark study
filter_benchmark_results.R: extraction of results of benchmark study from batchtools registry
filter_benchmark_results_stability.R: extraction of features for best configurations of benchmark study from batchtools registry and calculation of stability values

filter_time_experiments.R: definition of batchtools experiments for scaling behavior study
filter_time_results.R: extraction of results of scaling behavior study from batchtools registry

The following files are sourced in filter_benchmark_experiments.R:
config.R
make_lrn.R 
microarray_filters.R
paramset.R
resampling.R

The following files are sourced in filter_benchmark_results_stability.R:
stability_measure.R



# Evaluation: Files for generation of plots

filter_benchmark_rank_cors.R: similarity of filter methods, creates mean_cor.RData
filter_time_evaluation.R: comparison of scaling behavior
filter_benchmark_tuning.R: determine best configurations per filter method and dataset
filter_benchmark_evaluation.R: search for optimal filter methods
filter_benchmark_evaluation_stability.R: stability analysis of best configurations
tables.R: creation of tables




# Notice
In order for the code to run, there must be folders called Results and Plots in the same directory as the folders Experiments and Evaluation.










