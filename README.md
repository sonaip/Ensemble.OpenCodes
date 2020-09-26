
## Ensemble perception in the time domain: evidence in favor of logarithmic encoding of time intervals

__Yue Ren, Fredrik Allenmark, Hermann J. Müller, Zhuanghua Shi__
General Experimental Psychology, LMU Munich, Munich, Germany

__Abstract__
Although time perception is based on the internal representation of time, whether the subjective timeline is scaled linearly or logarithmically remains an open issue. Evidence from previous research is mixed: while the classical internal-clock model assumes a linear scale with scalar variability, there is evidence that logarithmic timing provides a better fit to behavioral data. A major challenge for investigating the nature of the internal scale is that the retrieval process required for time judgments may involve a remapping of the subjective time back to the objective scale, complicating any direct interpretation of behavioral findings. Here, we used a novel approach, requiring rapid intuitive ‘ensemble’ averaging of a whole set of time intervals, to probe the subjective timeline. Specifically, observers’ task was to average a series of successively presented, auditory or visual, intervals in the time range 300–1300 ms. Importantly, the intervals were taken from three sets of durations, which were distributed such that the arithmetic mean (from the linear scale) and the geometric mean (from the logarithmic scale) were clearly distinguishable. Consistently across the three sets and the two presentation modalities, our results revealed subjective averaging to be close to the geometric mean, indicative of a logarithmic timeline underlying time perception. 

Experimental data and analysis code:

* Main_Figure.R: Generate all figures for the manuscript
* model_simulation.R: compared four different ensemble models: Arithmetic, Harmonic, Weighted, and Geometric means
* rawData.rds: raw PSE/JND estimated from UML procedures
* sub_trialseq.mat: raw UML trial sequences and responses. 
