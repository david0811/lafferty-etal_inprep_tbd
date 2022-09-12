# paper-metarepo-template

Template repository for meta-repositories for experiments linked to papers.

I got the idea to use meta-repositories for journal articles, as well as the basic structure of this template repository, from Chris Vernon and Casey Burleyson at [IM3](https://github.com/IMMM-SFA).

## Purpose

A meta-repository creates a single point of access for someone to find all of the components that were used to create a published work. This structure facilitates code-checking and reproducibility. The meta-repository for a particular analysis contain references to all used data and software as well as containing clear descriptions of how to use your scripts to reproduce your experiment and recreate publication figures. It should also contain links to released versions of your code (which might correspond, for example, to versions of the paper submitted for publication).

## Using the template

Click `Use this template` on the main repository page (shows up to the left of `Code`). [Name your repository](naming-your-meta-repository), fill in a description, select whether you want the repository to be `Public` or `Private` (usually private until a preprint is ready, but this may differ for specific cases), and leave `Include all branches` unchecked.

### Naming your meta-repository

The following naming conventions should be used when naming your repository:  
- Single author:  `lastname_year_journal`
- Multi author:  `lastname-etal_year_journal`
- Multiple publications in the same journal: `lastname-etal_year-letter_journal` (e.g., `human-etal_2020-b_nature`)

It may not always be clear what journal you are submitting to when you create your repository. In this case, use `TBD` instead of the journal name, and update once a target journal has been identified. Similarly, if revising for submission to a new journal, rename the repository accordingly.

You can initialize the year with the current year, or use a placeholder like `inprep`. Update once you submit, and update again once the paper is published.

### Customize your `.gitignore` file

The `.gitignore` supplied with this template is configured for Julia. If you're using a different language, update the `.gitignore` accordingly, and also modify as needed for your particular project.

### Choose a license

By default, this template uses the MIT license. This is a good, general, permissive choice, but if you want to [use a different license](https://choosealicense.com/), delete the existing `LICENSE.md` and [add a new license file](https://docs.github.com/en/communities/setting-up-your-project-for-healthy-contributions/adding-a-license-to-a-repository) corresponding to the desired license.

### Suggestions

- Create your meta-repository when you start to work on your experiment (along with your paper contract and preliminary outline). This will make it easier to track your progress and document everything necessary, while it can be more work to dig up all of the details if you wait until the paper is being prepared.
- The meta-repository is not a place to store raw data. If the data is available using a DOI, provide a reference and link. If you generated the data as part of your experiment, archive it and create a DOI (using a repository like [Zenodo](https://zenodo.org/)), then reference and link with instructions. This archive should be updated as needed for any further revisions.
- Create complete documentation in the `README.md`, as well as in the scripts themselves, which will help debugging and code handoffs. Have others read your documentation and test to ensure that the instructions are clear and that they can reproduce your analysis.
- Try to avoid hard-coding absolute paths into your code. Instead, make them relative to the repository root so they will work regardless of how someone clones the relevant repositories.
- If you're submitting to a double blind journal, we will want to hide any author identifiers prior to submission.

## Creating a release for your meta-repository

It is important to [version and release your meta-repository](https://docs.github.com/en/repositories/archiving-a-github-repository/referencing-and-citing-content) due to changes that may occur during the publication review process. This ensures that changes are traceable, and provides access to earlier versions (which may be more appropriate for forking in the future).

## Using the README template

A sample meta-repository README markdown template is provided in this repository in the file `metarepo_readme_template.md`. 

To use it, after creating a new repository using this template and cloning to your local machine:

1. Change your directory to the local repository path.
2. Run `git rm README.md` to delete this file (`README.md`) and commit it using `git commit -m 'remove default README'`.
3. Run `git mv metarepo_readme_template.md README.md` to rename `metarepo_readme_template.md` as `README.md`.
4. Run `git add README.md` to stage the new file that will show up on load in your remote GitHub repository.
5. Run `git rm metarepo_readme_template.md` to remove the original template.
6. Run `git commit -m 'create new README'` to set the changes.
7. Run `git push` to send the changes to the remote GitHub repository.
8. Modify the `README.md` file appropriately to represent your experiment and use the `add`, `commit`, `push` workflow to update your remote repository.