# search-bonds

Installation with Stack
-----------------------
Stackage is a stable package archive. Stackage builds are supposed to
be reproducible. Stackage also provides Long Term Support releases.
To build `search-bonds` with Stackage dependencies, use the `stack` tool:

  * install [`stack`](https://docs.haskellstack.org/)
  * if necessary, install GHC: run `stack setup`
  * run: `stack update`
  * in the project source directory run: `stack build`

### Build Status

[![Build Status](https://travis-ci.org/wurthel/search-bonds.svg?branch=master)](https://travis-ci.org/wurthel/search-bonds)

To run
-----------------------
Данная программа вычисляет расстояние между атомами
и выводит такие пары атомов, расстояние между которыми не больше
заданного. 

To run the programm:

`./search-bonds-exe 1M0L.pdb C N 1.8 result`

where:
1) 1M0L.pdb - molecule in PDB format;
2) C N - atoms, the distance between which is estimated;
3) 1.8 - the upper limit of the allowable distance between atoms;
4) result - file with the result of the program.
