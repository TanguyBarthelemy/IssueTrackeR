# Why ?

Because…

Un petit document qui expliquent certains choix de développement faits.

## `NA`, `NULL` ou `""`

Certains éléments sont vides, manquants ou valent `NA` par défault lors de la lecture depuis GitHub.

En fait tout d'abord, on ne peut pas laisser `NULL` car dans la plupart des cas, cela effacerait la valeur :

```r
a1 <- "val1"
a2 <- "val2"
a3 <- NULL
v <- c(a1, a2, a3) # a3 n'est pas pris en compte

b1 <- 1
b2 <- NULL
liste <- list(a = 3.4, b = "ancienne valeur")
liste$a <- b1
liste$b <- b2 # la valeur b est effacée
```

Maintenant, que mettre à la place ?

2 choix sont possibles :

- La valeur `NA` correspondantes (`NA_integer_`, `NA_character_`, `NA_real_`...)
- Une valeur par défault (exemple `""` pour les chaînes de caarctères, `0L` pour les entiers...)

On va donc distinguer de cas de figure concernant nos objets (issues, milestones et labels) :
- Les objets qui devrait être présents mais sont actuellement vides (pour une milestone, ce sont le titre, la description, le createur...) -> on va mettre une valeur par défault
- Les objets qui (dans ce cas précis) ont des raisons d'être absent (par exemple pour une milestones, la due_date, le closed_at...) -> on va mettre un `NA`

## Arguments documentation

Certains arguments sont utilisés dans différentes fonctions. Il faut les documenter mais à partir du moment où ils ont le même sens, autant les documenter une seule fois et ré-utiliser la documentation.

Voilà quelle fonction documente quel paramètre :
- `reset_options` documente `verbose`

## Le choix de `@method`

Pour les fonctions d'extraction, on ne met pas les quotes autour des noms de fonction dans le tag `@method`.

Ainsi on écrira:

```
#' @method [ IssuesTB
#' @method [<- IssuesTB
...
```

et non

```
#' @method `[` IssuesTB
#' @method `[<-` IssuesTB
...
```

Car il insert des quotes dans la documentation et que cela génère l'erreur:

```
❯ checking Rd \usage sections ... WARNING
  Bad \usage lines found in Rd file 'extraction-issues.Rd':
    <unescaped bksl>method{`[`}{IssuesTB}(x, ...) <- value
    <unescaped bksl>method{`[[`}{IssuesTB}(x, ...) <- value
  
  Functions with \usage entries need to have the appropriate \alias
  entries, and all their arguments documented.
  The \usage entries must correspond to syntactically valid R code.
  See chapter ‘Writing R documentation files’ in the ‘Writing R
  Extensions’ manual.
```
