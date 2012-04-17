-- Displaying the standard tableau, various parts of the young diagram calculus
module YoungCalculus where

import Partitions
import Sn

-- youngDiagram
-- youngTableau

dim :: YoungDiagram -> Int
dim = hookLength
 