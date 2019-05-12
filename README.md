# search-bonds
Данная программа вычисляет расстояние между атомами
и выводит такие пары атомов, расстояние между которыми не больше
заданного. 

Пример запуска программы:
./search-bonds-exe 1M0L.pdb C N 1.8 result 

где аргументы:
1) 1M0L.pdb - файл с молекулой в формате PDB;
2) C N - атомы, расстояние между которыми оценивается;
3) 1.8 - верхняя граница допустимого расстояния между атомами;
4) result - файл с результатом работы программы.

### Build Status

[![Build Status](https://travis-ci.org/wurthel/search-bonds.svg?branch=master)](https://travis-ci.org/wurthel/search-bonds)
