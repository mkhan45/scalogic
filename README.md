# Scala Logic language

```
edge(1, 2)
edge(2, 3)
connected(x, z) :- ((x == z || edge(x, z)) || (edge(x, y) && connected(y, z)))

edge(1, 2): true
edge(2, 3): true
edge(1, 3): false
connected(1, 3): true
```
