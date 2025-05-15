
# Folder structure

```
├── data
│   ├── analysis # models and processed sentiment scores
│   ├── clean # additional cleaned data during analysis
│   ├── external # external data
│   ├── interim # base data for analysis steps
│   └── topic selection # helper files during topic selection
├── notebooks # example code
├── pipeline # collect and compute inital data before analysis
├── Rcode # actual analysis
├── src # python code for data collection
└── tests # some test for the data collection code
```


# Installation

This repository uses R and Python.
Python is used for data collection, while R is used for data cleaning and analysis.

## Install Python Dependencies

The virtual environment is managed by [uv](https://docs.astral.sh/uv/getting-started/installation/).
To install it, run uv sync.

### API keys

- create a `keys.txt` and add all available API keys, one per line, to the file.
- they will be read in and used automatically.


## Install R Dependencies

To install the R dependencies open the R files in RStudio and follow the installation instructions for missing packages for each file.

# Usage of the youtube API

- example usage is shown in `notebooks/01-yt_requests/02-example_usage.ipynb`

# Citation

TBD