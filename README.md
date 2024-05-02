# Scala Logic language

Website: <https://mkhan45.github.io/scalogic/>

A logic programming language similar to MiniKanren. The code for the engine (<200LOC)
is in `Reunify.scala`

```
edge(1, 2)
edge(2, 3)
connected(x, z) :- ((x == z || edge(x, z)) || (edge(x, y) && connected(y, z)))

edge(1, 2): true
edge(2, 3): true
edge(1, 3): false
connected(1, 3): true

sameLength(x, y) :- ((x == () && y == ()) || ((x == (xh, xs) && y == (yh, ys)) && sameLength(xs, ys)))

// solves for a valid xs
(xh, xs) == (1, (2, (3, (4, ())))): Map(xh -> 1, xs -> (2, (3, (4, ()))))

sameLength((1, (2, ())), (2, (3, ()))): true
sameLength((1, (2, ())), (2, ())): false

// solves for a valid a that makes the two lists the same length
sameLength((1, (3, (5, ()))), (1, (3, a))): HashMap(a -> (yh'', ()))
```

## Running

### Development mode:
```sh
sbt> ~fastLinkJS
```
```sh
cd vite
npm run dev
```

### Build
```sh
./build_web.sh
basic-http-server docs
```
```
