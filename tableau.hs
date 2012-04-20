-- Module	: YoungCalculus (Representation Theoretic Operations on Young Diagrams and Tableau)
-- Copyright	: (c) 2012 Grant Rotskoff
-- License 	: GPL-3
--
-- Maintainer 	: gmr1887@gmail.com
-- Stability 	: experimental


-- Displaying the standard tableau, various parts of the young diagram calculus
module YoungCalculus where

import Partitions
import Sn

-- youngDiagram
-- youngTableau

dim :: YoungDiagram -> Int
dim = hookLength
 
