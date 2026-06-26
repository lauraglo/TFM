![R](https://img.shields.io/badge/R-4.0+-276DC3?logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-1.7+-276DC3)
![License](https://img.shields.io/badge/license-academic-lightgrey)

# Terminology Editor — Interactive BIO Corpus Annotation Tool

This project delivers a browser-based tool for the interactive annotation and correction of terminology in scientific corpora. Built as an R Shiny application, it allows linguists and NLP practitioners to load a pre-tokenised corpus in BIO format, visually inspect each abstract, select individual tokens, and assign or revise their BIO tags (B-KEY, I-KEY, O) with instant visual feedback — no command-line interaction required. The tool was developed as part of the Master's thesis *"A system for annotating terms in multilingual corpora"* and validated on a domain-specific corpus of over 210,000 tagged tokens from scientific abstracts. The annotated output can be downloaded directly for use as training data in downstream terminology extraction models.

---

## Problem Statement

Building high-quality labelled corpora for terminology extraction is expensive and time-consuming: automatic taggers are imperfect, and manual correction of raw BIO-tagged files in a text editor is error-prone and not user-friendly. This tool bridges the gap by providing a **structured editing interface** that enforces BIO consistency constraints (e.g., I-KEY cannot begin a keyword phrase without a preceding B-KEY), tracks per-abstract change counts, and exports corrected files in the same tab-separated format consumed by NLP pipelines.

---

## Dataset

Two BIO-tagged corpora of scientific abstracts are included in the repository and can be loaded directly into the app:

| File | Tokens | Purpose |
|---|---|---|
| `train.txt` | ~142,500 | Training corpus for terminology extraction models |
| `test.txt` | ~67,800 | Held-out test corpus |

**BIO tag scheme:**

| Tag | Meaning | Example |
|---|---|---|
| `B-KEY` | Beginning of a keyword / term | *Feedforward* maximum power point... |
| `I-KEY` | Inside (continuation of) a keyword | Feedforward *maximum power point*... |
| `O` | Outside — not part of a keyword | ...of *PV* systems |

**File format** — tab-separated, one token per line, blank lines separating abstracts:

```
Feedforward    B-KEY
maximum        I-KEY
power          I-KEY
point          I-KEY
tracking       I-KEY
of             O
PV             B-KEY
systems        I-KEY
               
...
```

---

## Methodology

The application implements an interactive **human-in-the-loop annotation workflow**:

1. **Load** — upload any BIO-tagged `.txt` or `.csv` file; the app parses tokens and abstract boundaries automatically.
2. **Inspect** — abstracts are displayed in a paginated interactive table with existing B-KEY tokens highlighted in yellow and I-KEY tokens in cyan.
3. **Select** — click a row to select an abstract, then highlight a word on screen to target it.
4. **Tag** — click B-KEY, I-KEY, or O to assign the tag. The app enforces BIO validity: I-KEY is only accepted if the immediately preceding token is B-KEY or I-KEY.
5. **Track** — each abstract records the number of edits (`NChanges`) so annotators can prioritise unreviewed documents.
6. **Export** — download the corrected corpus in the original tab-separated BIO format.

**State management** uses Shiny reactive values with three parallel data representations:
- `a` — ground-truth BIO data (saved to file on download)
- `b` — abstract-level summary table shown to the user
- `c` — HTML-enriched version used for in-browser highlighting

---

## Results

The tool was developed and evaluated in the context of a Master's thesis on multilingual terminology extraction. Key outcomes:

| Metric | Value |
|---|---|
| Corpus size supported | 210,000+ tokens |
| Annotation actions | B-KEY, I-KEY, O tagging + validation |
| BIO constraint enforcement | I-KEY blocked unless preceded by B-KEY/I-KEY |
| Export format | Tab-separated BIO (pipeline-ready) |
| Deployment | Local (RStudio / CLI) or hosted on shinyapps.io |

Full methodology, experiments, and evaluation are available in the published thesis: [https://oa.upm.es/72734/](https://oa.upm.es/72734/)

---

## Key Findings

- Enforcing BIO constraints interactively (rather than post-hoc) eliminates the most common annotation errors in multi-word term tagging.
- Colour-coded highlighting (yellow for B-KEY, cyan for I-KEY) provides immediate visual confirmation of tagging decisions, reducing cognitive load compared to plain text editing.
- The change counter per abstract enables efficient quality-control passes: annotators can filter for documents with zero edits (unreviewed) or many edits (complex/uncertain cases).
- Reactive state management in Shiny allows all three data views to stay synchronised without page reloads, making the editing experience fluid even for corpora with thousands of tokens.

---

## How to Run

### Prerequisites

Install the required R packages (one-time setup):

```r
source("install.R")
```

Or install manually:

```r
install.packages(c("shiny", "dplyr", "tableHTML", "magrittr",
                   "stringr", "tidyverse", "tools", "DT"))
# devtools required only for development installations
install.packages("devtools")
```

### Option 1 — RStudio

1. Open `app.R` in RStudio.
2. Click the **Run App** button.
3. For full functionality (text selection), choose **Open in Browser** rather than the RStudio viewer pane.

### Option 2 — Command line

```bash
Rscript app.R
```

Then navigate to the local address shown in the terminal (e.g., `http://127.0.0.1:XXXX`).

### Option 3 — Hosted demo

A temporary deployment is available at:
[https://terminologyeditor.shinyapps.io/TerminologyEditor/](https://terminologyeditor.shinyapps.io/TerminologyEditor/)

### Usage workflow

1. Click **Choose File** and upload `train.txt` or `test.txt` (or your own BIO-tagged file).
2. Select a row in the table to focus on an abstract.
3. Highlight a word in the abstract text.
4. Click **Tag word as B-KEY**, **Tag word as I-KEY**, or **Tag word as O**.
5. The status message below the buttons confirms success or reports an error.
6. Click **Download file** to export the annotated corpus.

---

## Tech Stack

| Library | Purpose |
|---|---|
| `shiny` | Web application framework |
| `DT` | Interactive DataTable rendering |
| `dplyr` / `tidyverse` | Data manipulation |
| `stringr` | String operations for token counting |
| `tableHTML` | HTML table generation |
| `magrittr` | Pipe operator (`%>%`) |
| JavaScript (inline) | Cross-browser text selection capture |

---

## Reference

Master's thesis — *Un sistema para anotar términos en corpus multilingües* (*A system for annotating terms in multilingual corpora*), Universidad Politécnica de Madrid (UPM), 2022. The thesis itself is written in Spanish.
Full manuscript: [https://oa.upm.es/72734/](https://oa.upm.es/72734/)
