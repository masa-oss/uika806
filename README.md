<div id="top"></div>

## Technology used
<p style="display: inline">
<img src="https://img.shields.io/badge/-Java-007396.svg?logo=java&style=popout">


</p>


## About

This is a scheme interpreter that I wrote for my hobby, written in Java.


The following Japanese e-book is written about this interpreter.

『[はじめてのscheme制作](https://zenn.dev/clazz/books/a752b62f7b5e64)』([Zenn Books](https://zenn.dev/books))



## How to build

Download dependent files.
```
cd download
mvn dependency:copy-dependencies
cd ..
```

Open the project in NetBeans or build it with ant with the command below .

```
cd ant.run
ant small
cd ..

Mac:
chmod +x run4.sh
./run4.sh

Windows:
.¥uika806.bat
```

## Programs referred to

This program uses some of the JSchemeMin sources.

https://github.com/chungkwong/JSchemeMin


## License

Mozilla Public License




