# COMET Project: Developing interactive learning modules for hands-on-econometrics skills

Welcome to the repository the COMET project.  This repository houses all our materials for Jupyter-based notebooks to support teaching introductory and intermediate econometrics courses.  You can find the URL at [comet.arts.ubc.ca](https://comet.arts.ubc.ca).

This project was made possible by the students at the University of British Columbia as part of a [teaching and learning enhancement](https://tlef.ubc.ca/) grant.

## Important Documents

You can find the most important documents here, in our repository:

- The `CODE_OF_CONDUCT.md` file contains our project and repository code of conduct, which is expected that all members and contributors follow.
- The `LICENSE` file contains our project's open-source license but see [our copyright page](https://comet.arts.ubc.ca/pages/copyright.html) for more information.

All of the other documentation is on our project site, which you should refer to for style guides, citations, and other material.

You can find details about this project here, in addition to the key files and formats.  By contributing to this project, you agree to our project code of conduct.

# Repository Guidelines

It is very important that you follow these guidelines when committing work to the repository, in order to keep things well-organized.

#$ Repository Organization

This repository is organized into several distinct parts, housed on the `main` branch's `root` directory.

* The directory `meta/` contains non-project specific files, such as our linter.
* The directory `project/` contains all the the main project files.  These are organized into two parts:
  * The sub-directory `docs/` contains all our project content, mainly notebook files.
  * All other files and directories contain the code to build and maintain the website.

We use the [Quarto](https://quarto.org/) framework to write content and build the website, although we also support raw `.ipynb` notebooks, as well.

When a commit is merged into `main` it triggers an action (in `.github/workflows`) which builds the website, then (a) pushes it to `gh-pages` and (b) moves select artifacts to the `artifacts` branch so they can be used in a JupyterLab environment.  You should not merge or commit to either of these two branches.

* The action runs using the Docker image in `.dockerfile`.  You can [use this locally](https://nektosact.com/installation/index.html) to test if your builds will work.

## Commit Guidelines

Ensure that you keep your commits clean and tidy:

- Use a descriptive commit title and description
- Make sure you have removed temporary files and other materials

Any commits or merge requests that do not meet these guidelines will be rejected and will need to be re-done.

## Large Files

Large files are problematic in Git: because they are stored as binaries _any_ change to them (including inconsequential ones) creates a new version of the file, which is then stored in the repository's commit history.  This means that the repo can quickly balloon in size to an unmanageable degree.  We use the [git Large File Storage](https://git-lfs.github.com/) system.

Currently, the list of filetypes which should be automatically version controlled can be viewed in the `.gitattributes` file in the main repository.  However, you should avoid committing large files whenever possible.


