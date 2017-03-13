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
