<p align="center">
  <img src="www/icon.svg" width="200" alt="CLIP-ART Logo">
</p>

# CLIP-ART: Code-less Interactive Plot - Automated R Tool

**CLIP-ART** is an R Shiny application designed to streamline the data visualizations by bringing the power of **ggplot2** to a graphical user interface (GUI). It empowers researchers to generate, customize, and export complex plots without writing a single line of code.

**Serverless Web-based Application:** [ShinyApps.io](https://gourean.shinyapps.io/clip-art)

**Shinylive Web-based Application:**[Shinylive](https://gourean.github.io/clip-art)

## Key Features

*   **Data Import & Management**:
    *   Accepts .csv, .xlsx, and .sav files.
    *   **Data Editor**: View, edit, and manipulate your dataset directly within the app.
    *   **Variable Mapping**: Intuitive selection of X, Y, and Grouping variables.

*   **Supported Visualizations**:
    *   **Scatter Plot**: Visualize relationships between two continuous variables.
    *   **Histogram**: Explore data distribution.
    *   **Box Plot**: Display summary statistics and outliers.
    *   **Bar Chart**: Compare categorical data with error bars.
    *   **Violin Plot**: Visualize data density and distribution.
    *   **Density Plot**: Smooth representation of data distribution.
    *   **Dot Plot**: Detailed view of individual data points.
    *   **Raincloud Plot**: Visualize data distribution with a combination of box plots, violin plots, and jittered points.

*   **Advanced Customization**:
    *   **Themes**: Choose from clean, publication-ready themes (Minimal, Classic, B&W).
    *   **Colors**: Custom color pickers with transparency (alpha) control. Support for manual color palettes.
    *   **Geometry Options**: Dynamic controls for bin width, point size, and stroke.
    *   **Reference Lines**: Add custom horizontal reference lines with specific colors.
    *   **Faceting**: Easily split your plots by a categorical variable.

*   **Export**:
    *   Download high-resolution plots in **PDF**, **SVG**, and **JPEG** formats.
    *   Download manipulated datasets as **CSV**.

*   **User Interface**:
    *   **Responsive Design**: Built with `bslib` for a modern, mobile-friendly experience.
    *   **Interactive Sidebar**: Collapsible settings panel for maximum visualization space.

## Installation & Requirements

**No Installation Required**: You can run the app directly in your browser without installing anything:
*   [**Serverless (ShinyApps.io)**](https://gourean.shinyapps.io/clip-art) - Best for quick access.
*   [**Shinylive**](https://gourean.github.io/clip-art) - Best for client-side processing.

To run the app **locally**, you will need **R** and several R packages.

### 1. Prerequisites
Ensure you have R and RStudio installed.

### 2. Install Dependencies
Run the following code in your R console to install the required packages:

```r
install.packages(c(
  "shiny",
  "bslib",
  "ggplot2",
  "dplyr",
  "DT",
  "colourpicker",
  "ggthemes",
  "glue",
  "svglite",
  "readxl",
  "haven",
  "ggdist"
))
```

## Usage

1.  **Run the App**:
    Open `app.R` in RStudio and click the **"Run App"** button, or run:
    ```r
    shiny::runApp("path/to/CLIP-ART/app.R")
    ```

2.  **Step-by-Step Workflow**:
    *   **Upload**: Import your dataset (.csv, .xlsx, or .sav).
    *   **Configure**:
        *   Choose your **Plot Type** (e.g., Box Plot, Scatter Plot).
        *   Map your **X Axis**, **Y Axis**, and optional **Group/Color** variables.
    *   **Customize**:
        *   Use the **Settings Sidebar** to adjust titles, axis labels, colors, and themes.
        *   Fine-tune plot geometry (point size, stroke, bin width).
        *   Add reference lines or facet by a variable.
    *   **Visualise**: View the interactive plot updates in real-time.
    *   **Export**: Download your final figure in your preferred format.

## Support

If you find this tool useful, consider supporting its development:

[![Donate](https://raw.githubusercontent.com/stefan-niedermann/paypal-donate-button/master/paypal-donate-button.png)](https://paypal.me/gourean)

## Development

Developed via **Vibe Coding** with **Gemini 3 Pro**.



