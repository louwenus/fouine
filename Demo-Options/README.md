
pour (re)compiler, lancer
dune build

On recommande de faire
```
ln -s _build/default/bin/fouine.exe fouine
```

On peut executer fouine de diverses facons, que vous pouvez tester en tapant successivement :

```
./fouine -help

./fouine test1.ml

./fouine -debug test1.ml

./fouine -shout test1.ml

./fouine -shout -debug test1.ml
```

Voyez le fichier bin/fouine.ml pour comprendre comment gerer les diverses options.
