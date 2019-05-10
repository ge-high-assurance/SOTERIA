# SOTERIA
Safe and Optimal Techniques Enabling Recovery, Integrity, and Assurance

The goal of the SOTERIA program was to advance the techniques and tools available
for safety analysis. In addition to the model-based framework for modeling, visualizing,
and analyzing the safety of system architectures, we provide an automated way to 
synthesize safety-informed architectures. This provides a rapid design space exploration
for trade-off analysis, especially important when there are competing safety objectives
such as integrity and availability.

This project uses safety-analysis as a submodule. Here are the steps for cloning the 
main project with the submodule in it. (Please reference git-scm.com/book/en/v1/Git-Tools-Submodules
for more details on working with submodules.)

First, clone the project. The submodule directory will be created, but will be empty at first.

```
$ git clone https://github.com/ge-high-assurance/SOTERIA.git
Cloning into 'SOTERIA'...
remote: Enumerating objects: 22, done.
remote: Counting objects: 100% (22/22), done.
remote: Compressing objects: 100% (18/18), done.
remote: Total 22 (delta 2), reused 6 (delta 1), pack-reused 0
Unpacking objects: 100% (22/22), done.
$ cd SOTERIA
$ ls safety-analysis/
$
```

Run the following two commands to initialize the local configuration file and then fetch
all the data from the project and check out the appropriate commit listed in the superproject. 

```
$ git submodule init
Submodule 'safety-analysis' (https://github.com/ge-high-assurance/safety-analysis.git) registered for path 'safety-analysis'
$ git submodule update
Cloning into '/SOTERIA/safety-analysis'...
Submodule path 'safety-analysis': checked out 'dcbe278b26c047a6dce039f748384ee6189fc8db'
```
