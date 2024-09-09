# JSSAM_popcomp

This repository is the replication package of the paper "Inferring a population composition from survey data with nonignorable nonresponse: Borrowing information from external sources," accepted for publication by the Journal of Survey Statistics and Methodology in September 2024.

The "scripts" folder contains the codes needed to reproduce the results in the paper. The files' names are explicative. Codes recall data and sources as follows:
- Data (NSI sample, Almalaurea survey data, and SNR values) are contained in the folder "data;"
- All custom functions are in the "functions" folder;
- Posterior samples (MCMC output) are typically saved in the "output" folder. Note that the output of step 1 is the input of step 2, and so on.
- Figures and tables reported in the paper are generated using scripts named "Results_";
- Figures reported in the paper are saved in the "figures" folder;
- Figures not reported in the paper (trace plots) are saved in the "extra_figures" folder.

If you have any questions, send an e-mail to veronica.ballerini@unifi.it
