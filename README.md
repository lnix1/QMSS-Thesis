# This repository serves to store the code and data used for my QMSS Thesis.

## The analysis conducted for my thesis used the scripts contained here in the following order:

1. 'download_data.R' to retrieve the initial dataset, wrangle, and write out a dataset for use in model fitting.

2. 'run_model.R' to read in the stan model and fit the final model by sampling.

3. 'create_data_viz.R' to analyze the final posterior samples and produce the graphics used in my thesis.

### *NOTE:* For replication of plots, please utilize the data viz script and do not attempt to download the data or run the model. Downloading the dataset from PISA can take upwards of an hour (so I have provided the post cleaning dataset) and running the model took 5 days original (so I have provided the posterior samples).

### *Note:* To work with the posterior samples and produce plots, you will need to have the .rds file (which I can email to you) and place the .rds within the 'Samples' folder.
