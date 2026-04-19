### At a Glance

This project analyzes the social impact and accessibility of video gaming, specifically focusing on the experiences of players with disabilities. Utilizing survey data, it provides a "Roadmap for Improvement" by identifying key accessibility features and demonstrating how gaming serves as a vital social equalizer.

-----

## Gamer Study: Accessibility & Social Connectivity Analysis

### Project Overview

This R-based data analysis project processes raw survey data to explore the intersection of gaming, disability, and social connection. The primary objective is to synthesize insights into how gaming helps individuals stay connected and where technical barriers still exist for disabled players.

### Key Features

  * **Data Cleaning Pipeline:** Automated script to standardize survey results, handle duplicates, and correct demographic inconsistencies.
  * **Social Connectivity Metrics:** Comparative analysis of social benefits (e.g., making new friends, staying connected) between disabled and non-disabled player groups.
  * **Accessibility Deep Dive:** Targeted analysis of accessibility barriers and the perceived importance of features like adjustable text size and subtitles.
  * **Strategic Visualization:** Generation of publication-ready charts, including:
      * Social Connectivity Bar Graphs (Total Counts & Percentages).
      * The "Accessibility Divide" Pie Chart.
      * Priorities for Progress (Roadmap for Improvement).

### Technical Stack

  * **Language:** R
  * **Core Libraries:**
      * `tidyverse`: For data manipulation and visualization (`ggplot2`).
      * `janitor`: For data cleaning and tabulating.

### Project Structure

  * `data/`: Contains the raw survey dataset (`Gamer Study Sample Data Raw.csv`).
  * `scripts/`:
      * `data_clean.R`: Specialized script for data preprocessing.
      * `Data_Analysis.R`: Main script for generating social and accessibility insights.
      * `team21_submission_script.R`: Final synthesized script for analysis and exports.
  * `output/`: Automated directory for exported CSVs and visualization PNGs.

### Getting Started

1.  Ensure R and the necessary packages (`tidyverse`, `janitor`) are installed.
2.  Open the `Plot2026.Rproj` file to set your working directory.
3.  Run `team21_submission_script.R` to process the raw data and generate the visualization suite in the `/output` folder.

### Key Insights from the Analysis

> **The Path Forward:** While 71% of disabled players report finding new friends through gaming, approximately 50% still report significant accessibility barriers. The "Roadmap for Improvement" highlights **Adjustable Text Size** and **Adjustable Difficulty** as top priorities for the community.
