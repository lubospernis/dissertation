Replication File for 

Gary King; Emmanuela Gakidou; Kosuke Imai; Jason Lakin; Ryan T. Moore; 
Clayton Nall; Nirmala Ravishankar; Manett Vargas; Martha María Téllez-Rojo, 
Juan Eugenio Hernández Ávila, Mauricio Hernández Ávila, and Héctor Hernández 
Llamas. Public Policy for the Poor? A Randomized Ten-Month Evaluation of the 
Mexican Universal Health Insurance Program, The Lancet, forthcoming, 2009

Data Sets

Three primary data sets were used in the evaluation:

1. ALL.dta.  A STATA file with all of the outcome variables from the
baseline and post-treatment evaluation surveys.

2. combined6statesfinal.dta.  A STATA file containing demographic
information from the constructed health clusters.

3. clustmatchlist.dta.  A STATA file containing the matched pairs of
clusters used in the experiment.

4. The remaining data files in the archive are to compare the clusters
in our study with the population of health clusters.

A codebook for the survey questionnaire appears in both Spanish and
English.  The survey question codes appear in the questionnaire
exactly as they appear in the dataset.  (Note, however, that on the
English form the questions above Section 7 should be labeled one
section higher; e.g., the code for Question 7.1 is P08D01, not
P07D01).


The all variables from the followup survey were coded with an appended
".T2".  With only a few exceptions, any question asked on the followup
survey was asked at baseline as well.

Running the replication:

The replication code was run in R.  We recommend using a Unix system,
as most of the file syntax will work only if executed on a Unix or
Linux machine.

1. Download and extract the file replicatelancet.tar.gz in a location
with multiple gigabytes of disk space.  The extracted folder
EvalReplicateAbridged is the main replication directory.  The
replication files may, with a small amount of rewriting, work on a PC
or Mac, but the file syntax at various places is intended for a
Unix/Linux machine.

2. Download the datafiles provided in the DVN directory.  For files
that permit subsetting, select "Download as" and then "Original
File"

3. Follow the directions indicating the folder(s) in the replication
folder system into which they should be copied.  (Some files need to be
copied into more than one folder.)

4. Load R version 2.8.1 (2008-12-22) from www.r-project.org.  Run the
file BatchLancet.R, in the main replication folder.  This program
takes about 18-20 hours to run on a fast machine, with most of the
time occupied by multiple imputation.  The program may work under 
later version of R, but we can offer no guarantees.

5. Comments in the Batch file indicate the output produced by each
file.

