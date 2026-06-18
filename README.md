# Symbolic Regression with Genetic Programming in Racket

This project implements symbolic regression using a genetic programming algorithm in Racket. It searches for a mathematical expression that approximates a target value `z` from two input variables, `a` and `b`.

The file *original_version.rkt* is a simpler version with fewer options and basic usage. This was used for the documentation.

The improved version includes stronger configuration validation, faster fitness evaluation, parsimony pressure, real-valued constants, a more independent island model, reproducible seeded runs, train/validation reporting, and optional early stopping.

The .txt files are example inputs, where some are random and others represent simple expressions.

## File

```text
improved_version.rkt
```

## Requirements

* Racket
* The Racket `plot` package

## Input Format

The input file must contain data points with three numeric values:

```text
a b z
```

The program accepts either parenthesized triples:

```racket
(1 2 3)
(2 3 5)
(4 1 5)
```

or raw numbers grouped by triples:

```text
1 2 3
2 3 5
4 1 5
```

## Basic Usage

Run with the default input file:

```bash
racket improved_version.rkt
```

Run with a specific input file:

```bash
racket improved_version.rkt --input prueba.txt
```

Run with a fixed random seed:

```bash
racket improved_version.rkt --input prueba.txt --seed 42
```

Run a larger search:

```bash
racket improved_version.rkt \
  --input prueba.txt \
  --generations 5000 \
  --population-size 201 \
  --islands 6 \
  --seed 42
```

## Output

By default, results are written to:

```text
symbolic-regression-output/
```

The output directory contains:

```text
best-expression.txt
history.csv
best-surface.png
convergence.png
```

`best-expression.txt` contains the final model, training metrics, validation metrics, node count, tree height, infix expression, and raw expression tree.

`history.csv` contains one row per recorded generation:

```text
generation,fitness,sse,mae,nodes,height,expression
```

`best-surface.png` shows a 3D surface plot of the best evolved expression.

`convergence.png` shows the evolution of penalized fitness across generations.

## Command-Line Options

| Option                    | Description                                                                   | Default                      |
| ------------------------- | ----------------------------------------------------------------------------- | ---------------------------- |
| `-i`, `--input`           | Input file containing triples `(a b z)`                                       | `prueba.txt`                 |
| `-g`, `--generations`     | Maximum number of generations                                                 | `1000`                       |
| `-p`, `--population-size` | Individuals per island. Must be odd and at least `3`                          | `101`                        |
| `--islands`               | Number of independent populations                                             | `4`                          |
| `--tournament-size`       | Tournament selection size                                                     | `5`                          |
| `--mutation-percent`      | Mutation probability as a percentage                                          | `10`                         |
| `--migrants`              | Number of top individuals migrated between islands                            | `10`                         |
| `--migration-period`      | Migrate every `N` generations. Use `0` to disable migration                   | `10`                         |
| `--initial-depth`         | Maximum depth of initial expression trees                                     | `5`                          |
| `--grow-depth`            | Maximum depth used by growth mutation                                         | `6`                          |
| `--max-nodes`             | Maximum preferred tree size before pruning                                    | `50`                         |
| `--parsimony`             | Node-count penalty added to SSE                                               | `0.001`                      |
| `--validation-percent`    | Percentage of points held out for validation                                  | `20`                         |
| `--patience`              | Stop after `N` generations without meaningful improvement. Use `0` to disable | `0`                          |
| `--min-improvement`       | Minimum fitness improvement needed to reset patience                          | `1e-6`                       |
| `--seed`                  | Random seed for reproducible runs                                             | disabled                     |
| `-o`, `--output-dir`      | Output directory                                                              | `symbolic-regression-output` |
| `--plot-every`            | Save a surface plot every `N` generations                                     | `0`                          |
| `--progress-every`        | Print progress every `N` generations                                          | `100`                        |
| `--no-save-plots`         | Disable PNG plot generation                                                   | disabled                     |
| `--show-plots`            | Open the final 3D surface plot in a GUI window                                | disabled                     |

## Algorithm Overview

The program evolves symbolic expression trees. Each tree represents a candidate mathematical expression using variables, constants, and protected binary operators.

Supported operators:

```racket
+     ; addition
-     ; subtraction
*     ; protected multiplication
div   ; protected division
expo  ; protected exponentiation
ln    ; protected logarithm with arbitrary base
```

Although the operator is named `ln` internally, it behaves as a binary log-base operator:

```text
log_base(left, right)
```

## Fitness Function

Each expression is evaluated on the training data. The program computes:

```text
SSE = sum of squared errors
MAE = mean absolute error
```

The optimization target is penalized fitness:

```text
fitness = SSE + parsimony * node_count
```

This means smaller expressions are preferred when two models have similar training error.

Use a larger `--parsimony` value to favor simpler expressions more strongly. Use `--parsimony 0` to disable parsimony pressure.

## Genetic Programming Components

The algorithm uses:

* Random initial populations
* Tournament selection
* Subtree crossover
* Operator mutation
* Growth mutation
* Constant mutation
* Tree pruning
* Elitism
* Multiple islands with periodic migration

Each island evolves mostly independently. Migration allows good expressions to spread without forcing every island to immediately follow the same global best model.

## Validation Split

By default, the program holds out 20% of the input points for validation:

```bash
--validation-percent 20
```

The model is evolved only on the training points. After evolution, the best expression is evaluated on the validation points.

To disable validation:

```bash
--validation-percent 0
```

## Early Stopping

Early stopping is disabled by default:

```bash
--patience 0
```

To stop after 500 generations without meaningful improvement:

```bash
--patience 500 --min-improvement 1e-6
```

## Reproducibility

Use `--seed` to make runs more reproducible:

```bash
racket solve_improved.rkt --input prueba.txt --seed 42
```

The improved version creates separate pseudo-random generators for each island. This avoids sharing one global random stream across parallel island evaluations.

## Example Experiments

Fast test run:

```bash
racket improved_version.rkt \
  --input prueba.txt \
  --generations 100 \
  --population-size 51 \
  --islands 2 \
  --no-save-plots \
  --seed 1
```

Standard run:

```bash
racket improved_version.rkt \
  --input prueba.txt \
  --generations 1000 \
  --population-size 101 \
  --islands 4 \
  --seed 42
```

Longer run with early stopping:

```bash
racket improved_version.rkt \
  --input prueba.txt \
  --generations 10000 \
  --population-size 201 \
  --islands 6 \
  --patience 1000 \
  --min-improvement 1e-6 \
  --seed 42
```

Simpler expressions:

```bash
racket improved_version.rkt \
  --input prueba.txt \
  --parsimony 0.01 \
  --max-nodes 35 \
  --seed 42
```

More exploratory search:

```bash
racket improved_version.rkt \
  --input prueba.txt \
  --population-size 301 \
  --islands 8 \
  --mutation-percent 20 \
  --migration-period 20 \
  --seed 42
```

## Recommended Workflow

1. Start with a small run to verify the input file.
2. Use a fixed seed while debugging.
3. Inspect `history.csv` to check convergence.
4. Increase generations, population size, or islands for better search.
5. Adjust `--parsimony` and `--max-nodes` if expressions become too large.
6. Use validation metrics to detect overfitting.

## Limitations and Future Improvements

Possible future improvements include:

* Expression simplification and clever expression generation
* Definition of better symbol weights to improve generation.
* Additional unary functions such as `sin`, `cos`, `sqrt`, and natural logarithm
* Stronger constant optimization after evolution
* Cross-validation instead of a single holdout split
* Unit tests with `rackunit`
* Splitting the file into modules for data loading, tree operations, fitness, evolution, plotting, and CLI handling
