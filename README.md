# datasetsfun

Analise de acidentes de transito em Porto Alegre (2000-2014) utilizando R. Trabalho desenvolvido para a disciplina de Analise de Dados.

## Estrutura do projeto

```
Apresentacao/       # Slides da apresentacao (LaTeX + PDF)
Artigo/             # Artigo academico (LaTeX + PDF)
Bases/
  analysis.R        # Script principal de analise
  data/             # Datasets CSV (acidentes por ano, lombadas, pardais)
  images/           # Graficos gerados pelo script
```

## Dados

Os datasets contem registros de acidentes de transito de Porto Alegre, com informacoes sobre:

- Tipo de acidente, local, horario, dia da semana
- Veiculos envolvidos (auto, taxi, moto, onibus, caminhao, bicicleta, etc.)
- Vitimas (feridos, mortes, mortes posteriores, fatais)
- Condicoes (tempo, noite/dia, regiao)

## Como executar

```bash
cd Bases
Rscript analysis.R
```

### Dependencias

- R
- Pacote `arules` (para regras de associacao Apriori)

```r
install.packages("arules")
```

## Analise

O script realiza:

1. **Resumo por ano** — total de acidentes, mortes, feridos e acidentes fatais
2. **Histogramas** — distribuicao de acidentes por horario e por mes (gerados em `images/`)
3. **Correlacoes** — entre tipos de veiculo e severidade (fatais, mortes, feridos)
4. **Regras de associacao (Apriori)** — padroes em acidentes fatais (dia/noite, tipo de acidente)
