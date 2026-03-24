## Code for "The Uncommon Sense Effect: experimental evidence for underspecification as the mental basis of logical inference"

[Elliot Schwartz](https://www.schwartzworld.net/), [Griffin Pion](https://www.griffinpion.com/), [Jake Quilty-Dunn](https://sites.google.com/site/jakequiltydunn/home), [Eric Mandelbaum](https://www.ericmandelbaum.com/), & [Spencer Caplan](https://www.spencercaplan.org/)

Please email any of the corresponding authors if you have theoretical or paper questions:
 - scaplan@gc.cuny.edu
 - gpion@gradcenter.cuny.edu
 - eschwartz@gradcenter.cuny.edu

(For questions about the code or analysis specifically, please reach out to Spencer)

### Preprint:
 - [PsyArXiv](https://osf.io/preprints/psyarxiv/yecmg_v1)

---

## Setup

* **Bash** tested on GNU bash, **version 3.2.57**(1)-release (arm64-apple-darwin23)
* **R** scripts have been tested on **version 4.3.1**. The following R packages are required to create the plots and run statistical analysis. ```
  dplyr, tidyr, stringr, ggplot2, Hmisc, scales, multimode, Matrix, tibble, lme4, reshape2, tidyverse, cowplot, ggpubr, ggrepel, xtable, broom, ggbeeswarm, rlang, mclust, kableExtra```
  - The scripts will attempt to install them automatically, though in my experimence it is far preferred to ensure that these are present and available on the local system ahead of time.


No other setup is required.

***n.b.*** a number of scripts assume a Unix-style directory stucture (already satisfied on Linux or Mac OS systems). You may need to make some manual adjustments if running on Windows (or, perhaps easier, would be to run using "linux subsystem for windows" if this applies to you).


## Running

The following script runs all generation and analyses:

```
$ runall.sh
```

## Abstract

Logical reasoning is one of humanity's most powerful abilities. A widespread assumption across psychology, linguistics, and philosophy holds that reasoning operates over concepts that refer to objects and properties in the world, yet this has rarely been tested empirically. We introduce a novel paradigm that exploits lexical ambiguity to differentiate candidate representations for human inference: word-forms, reference-fixing concepts, or more abstract "underspecified representations" that constrain meaning without fully determining it. Across three experiments (N=158), participants evaluated deductive arguments equivocating over polysemous or homonymous terms. Although these arguments are logically equivalent under standard analyses, participants reliably accepted non-truth-preserving polysemous arguments while rejecting homonymous ones. This asymmetry -- the Uncommon Sense Effect -- points to underspecified representations as the default basis of logical inference. This challenges a foundational assumption about meaning, and opens new empirical avenues for studying the link between what we think and why we think it.
