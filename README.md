# :clipboard: Requirements

R 4.4.1

# :hammer_and_wrench: Installation

`git clone git@github.com:clbenoit/MethylDB.git`

# :gear: Configuration

Edit the [config file](config.yml)

  - **cache_directory** : directory to store MethylDB cache files in. Necessary for good app performances. If NULL, a temporary directory will be used and cache will be lost on computer restart
  - **db_path** : Path where you genomic variation database is stored
  - **prefix** :  Name of you Genomic variation stuctured database
  - **use_browser** : Do you want to activate genome browser functionnality to visualize bam files ? (TRUE|FALSE)

For example 
`
prefix = "my_methylation_database"
db_path = "/home/my_databases/"
`
Will set up MethylDB to use the following SQLite database : /home/my_databases/my_methylation_database.db 

# :rocket: Run the application

Go to the app root directory and run

`Sys.setenv(MY_VARIABLE = "default"); shiny::runApp()`

# Demo app

##  :computer: Run demo locally

Go to the app root directory and run

`Sys.setenv(MY_VARIABLE = "demo"); shiny::runApp()` 

## :globe_with_meridians: Live demo App

<a href="https://omicsverse.fr" target="_blank">See live demo</a>

# :dna: Manage your genomic variations database


# :soon: Incoming features


# :warning: Troubleshouting



