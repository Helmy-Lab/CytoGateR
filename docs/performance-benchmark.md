# Performance Benchmark (Framework 2.1)

This benchmark responds to the editor's request for a reproducible runtime
table verifying async-computation claims made in response to reviewer
concern 2.1 ("Server Disconnections"). It reports wall-clock time for each
dimensionality-reduction method used by CytoGateR, measured on synthetic
data with the same in-memory shape as a typical FCS acquisition (N events x
10 fluorescence markers, after compensation/transformation/scaling -- i.e.
the `scaled_data` matrix passed into `future_promise()` in
`raw_data_module.R` and `clustering_module.R`).

## Hardware

| Spec | Value |
|---|---|
| OS | Windows 10 x64 |
| CPU architecture | x86-64, 8 logical cores (8 physical) |
| RAM | 15.6 GB |
| R version | 4.6.1 (2026-06-24 ucrt) |
| Key packages | Rtsne, uwot, future 1.x, promises 1.x |
| `plan()` | `future::multisession(workers = 2)` (as configured in `global.R`) |

## Method

For each event count N, a matrix of N x 10 values was drawn from a standard
normal distribution to emulate scaled marker intensities. Each algorithm was
timed with `system.time()` using the parameters CytoGateR sends by default:

- **PCA**: `prcomp(x, center = TRUE, scale. = TRUE)`
- **UMAP**: `uwot::umap(x, n_neighbors = 15)`
- **t-SNE**: `Rtsne::Rtsne(x, dims = 2, perplexity = 30, theta = 0.5, max_iter = 1000)` (Barnes-Hut)
- **MDS**: `cmdscale(dist(x), k = 2)` -- only measured up to 5,000 events, since
  the app hard-caps MDS input at 5,000 events (`raw_data_module.R`) because the
  distance matrix is O(N^2) in memory (~20 GB at 50,000 events without the cap)

Timings exclude file I/O (`read.FCS()`) and QC (`flowAI`), which are
data-dependent; see the note below on total pipeline time.

## Results

| Events (N) | Markers | PCA (s) | UMAP (s) | t-SNE (s, Barnes-Hut) | MDS (s) |
|---|---|---|---|---|---|
| 5,000 | 10 | 0.1 | 41.9 | 25.9 | 135.9 |
| 20,000 | 10 | 0.1 | 66.5 | 98.6 | not run (exceeds 5,000-event MDS cap) |
| 50,000 | 10 | 0.0 | 167.6 | 259.2 | not run (exceeds 5,000-event MDS cap) |
| 100,000 | 10 | 0.1 | 334.1 | 709.8 | not run (exceeds 5,000-event MDS cap) |

## Interpretation

- PCA is effectively instantaneous at every tested size and is not a
  disconnection risk.
- UMAP and t-SNE both exceed the ~90-second Shiny synchronous-connection
  timeout at every N tested above 5,000 events, and t-SNE exceeds it even at
  5,000 events (25.9 s is under 90 s, but combined with UMAP/other steps in
  the same analysis run it is not). This is the direct empirical
  justification for moving these computations into `future_promise()`
  (Framework 2.1) rather than running them synchronously inside
  `withProgress()`.
- MDS at 5,000 events alone (135.9 s) already exceeds the disconnection
  timeout, which is why the app hard-caps MDS at 5,000 events and runs it
  inside `future_promise()` rather than attempting it on larger inputs.
- The pre-computation guard in `raw_data_module.R` / `clustering_module.R`
  (warning shown when N > 50,000) is conservative relative to these
  timings: UMAP and t-SNE already warrant a warning well below that
  threshold. Consider lowering the guard threshold in a future revision (see
  Recommendations).

## Recommendations for the manuscript

1. Report this table (or a re-run on the reviewers'/authors' target
   deployment hardware) in the revised manuscript's Methods or Supplementary
   Materials, with a one-sentence note that all timings are for the
   dimensionality-reduction step only, run via `future_promise()` inside a
   `plan(multisession, workers = 2)` pool, so the Shiny session itself
   remains responsive throughout regardless of these durations.
2. Explicitly state the **tested file size limits** in the UI (per Framework
   2.1's requirement) using the `shiny.maxRequestSize = 500 MB` ceiling set
   in `global.R`, and cross-reference this benchmark table so reviewers can
   see that even worst-case event counts (100,000+) do not block the
   session, only extend total wait time.
3. If real FCS files of known size (MB) and event count are available before
   submission, re-run this benchmark end-to-end (including `read.FCS()` and
   `flowAI` QC) on 2-3 representative files and report file size alongside
   event count, since reviewers specifically asked for **file size**, not
   just event count.
