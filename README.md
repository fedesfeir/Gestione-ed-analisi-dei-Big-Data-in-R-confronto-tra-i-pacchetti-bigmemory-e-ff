# Gestione-ed-analisi-dei-Big-Data-in-R-confronto-tra-i-pacchetti-bigmemory-e-ff
# Gestione ed Analisi dei Big Data in R  
## Confronto tra i pacchetti *bigmemory* e *ff*

Questo repository contiene il materiale relativo alla tesi di laurea di **Federica Sfeir**, intitolata:

**“Gestione ed analisi dei Big Data in R: confronto tra i pacchetti bigmemory e ff”**  
Sapienza Università di Roma – Corso di Laurea in Statistica, Economia e Finanza  
Anno Accademico 2023/2024

---

## Contenuto del repository

- `Federica_Sfeir_Tesi1.pdf` – versione integrale della tesi  
- `code/` – directory contenente gli script R utilizzati per:
  - gestione dei Big Data con *bigmemory*
  - gestione dei Big Data con *ff*
  - regressione lineare con *biglm*
  - analisi comparativa di tempi e memoria
- eventuali dataset di esempio (se includibili)

---

## Obiettivi della tesi

La tesi analizza:
- Le problematiche legate alla gestione di Big Data in memoria
- Il funzionamento delle strutture dati dei pacchetti **bigmemory** e **ff**
- I vantaggi e limiti di ciascun approccio
- Un confronto sperimentale in termini di:
  - utilizzo della RAM
  - tempo di esecuzione
  - scalabilità

---

## Pacchetti R utilizzati

```r
library(bigmemory)
library(biganalytics)
library(biglm)
library(ff)
library(ffbase)
