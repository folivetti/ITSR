#!/bin/bash
stack run crossval towerData 0 -- +RTS -N2
stack run crossval towerData 1 -- +RTS -N2
stack run crossval towerData 2 -- +RTS -N2
stack run crossval towerData 3 -- +RTS -N2
stack run crossval towerData 4 -- +RTS -N2
