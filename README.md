# Physics-Informed XAI Framework for Real-Time Multi-Fuel Characterization

This repository provides an interactive **Shiny-based analytical framework** for real-time spectral characterization of liquid fuels (Gasoline and Diesel) using **physics-informed features** and **explainable artificial intelligence (XAI)** methods.

The application integrates:
- Microwave / RF spectral response analysis (S11 vs Frequency),
- Feature extraction grounded in resonance physics,
- White-box (Decision Tree, GLM) and black-box (global importance & local explanation) XAI techniques,
- High-resolution publication-ready visual outputs.

The framework is designed for **research, method development, and demonstration purposes**, with a strong emphasis on transparency and interpretability.

---

## ✨ Key Features

- **Multi-file / multi-sheet ingestion** (`.xlsx`, `.csv`)
- Automatic detection of:
  - Fuel type (Gasoline / Diesel)
  - Sample class (Branded / Unbranded)
- Physics-informed feature extraction:
  - Resonant frequency (GHz)
  - Notch depth (dB)
  - Derived Q-factor (proxy)
- **Spectral visualization** across fuel groups
- **White-box XAI**
  - Decision Tree (publication-quality visualization)
  - Generalized Linear Model (logistic regression + explicit equation)
- **Black-box XAI**
  - Global feature importance
  - Local sample-level explanation (LIME-inspired)
- Export-ready figures (300–600 DPI) and Excel summaries

---

## 📂 Expected Input Data Format

The application accepts **Gasoline** and **Diesel** measurement files in `.xlsx` or `.csv` format.

### Mandatory columns
- `Frequency` (Hz or GHz)
- `S11` (dB)

Column names are matched **case-insensitively** and sanitized automatically.

> If frequency values exceed `1e6`, they are assumed to be in Hz and converted to GHz internally.

### File naming conventions (recommended)
- Include `gasoline`, `diesel`, `benzin`, or `mazot` in file or sheet names for automatic fuel detection.
- Samples starting with `b` or containing `-b` are interpreted as **Branded**.

---

## 🚀 How to Run

### 1. Requirements
- **R ≥ 4.2**
- R packages:
  ```r
  shiny
  bslib
  tidyverse
  writexl
  readxl
  rpart
  rpart.plot
  gridExtra
