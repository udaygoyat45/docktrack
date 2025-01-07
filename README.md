DocTrack

- A plugin that can be added on top of github
- Ensures that documenation is up-to-date
- Utilizes a tree like structure to do so
- Created purely in OCaml

This should work in accordance with git. With every one repository, there should be an initialization of a new "feature tree". 
Feature tree keeps track of all the features within a repo. A feature can contrain sub-features as its children. New feature 
can be created anywhere within the tree. Whenever a new commit is created, the user is prompted to either create a new feature,
or attach the changes to an already existing feature. Each feature has a temporaly created linked list of features created. 
Each new feature automatically gets a "not-documented" tag to it. Meaning, the user still needs to update the documentation accordingly
and then mark that one change as "documented". 


### Git Integration

- When a new repo is created, the user should be prompted to initialize the feature tree
- When a new commit is created, the user should be prompted to either create a new feature, or attach the changes to an already existing feature
- When a feature is created, the user should be prompted to either create a new file, or attach the changes to an already existing file
- When a file is created, the user should be prompted to either create a new feature, or attach the changes to an already existing feature
- When a feature is created, the user should be prompted to either create a new file, or attach the changes to an already existing file
