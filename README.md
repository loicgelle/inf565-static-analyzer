# Analyseur statique pour un sous-ensemble de Java

## Installation

1. Cloner le projet :

```
git clone git@github.com:loicgelle/inf565-static-analyzer.git
```

2. Compiler le projet :

```
cd code && make
```

## Utilisation

* Obtenir de l'aide :

```
./analyzer
```

* Interpréter la méthode `main` de la classe `class_name` d'un fichier Java `p.java`

```
./analyzer --interpret class_name p.java
```

* Exécuter les analyses statiques sur le fichier `p.java`

```
./analyzer p.java
```

## Description des fichiers d'exemple

Des fichiers Java d'exemple et de test sont fournis dans le répertoire `code/examples/`. Plus d'informations à venir.


## Structure du projet

* `code/analyzer.ml` : point d'entrée du programme
* `code/simple_java_display.ml` : module d'affichage des programmes
* `code/static_analysis_init_vars.ml` : module d'analyse statique d'initialisation
* `code/static_analysis_typing.ml` : module de typage

En ce qui concerne plus spécifiquement l'analyse statique sur les variables :

* `code/static_analysis_variables` : point d'entrée de l'analyse
* `code/abstract_domain.ml` : module de gestion des environnements de variable (interfaçage avec les domaines abstraits)
* `code/domains.ml` : interface des domaines abstraits
* `code/domain_wrapper_boolean.ml` : domaine abstrait qui lie les informations sur les booléens et les informations sur les entiers
* `code/domain_constants.ml` : domaine abstrait des constantes
* `code/domain_intervals.ml` : domaine abstrait des intervalles
* `code/domain_congruences.ml` : domaine abstrait des congruences
* `code/domain_congruences_and_intervals.ml` : produit abstrait des domaines des intervalles et des congruences
