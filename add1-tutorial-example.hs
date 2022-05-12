------------------------------------------------------------------------------
-- |
-- Module      :  Introdutory example using ForSyDe-Deep
-- Copyright   :  (c) Denis Loubach
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Stability   :  experimental
-- Portability :  portable
--
------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
module Add1Deep where

import ForSyDe.Deep
import Data.Int (Int32)

-- 1st step: process function definition
-- A process function which adds one to its input
add1_pf :: ProcFun (Int32 -> Int32)
add1_pf = $(newProcFun
          [d|
             add1_pf :: Int32 -> Int32
             add1_pf n = n + 1
          |])

-- 2nd step: system function definition
-- System function (simply a process in this case) which uses add1_pf
add1_sf :: Signal Int32 -> Signal Int32
add1_sf = mapSY "add1_sf" add1_pf

-- 3rd step: system definition in terms of inputs/outputs
-- System definition associated to the system function
add1_sd :: SysDef (Signal Int32 -> Signal Int32)
add1_sd = newSysDef add1_sf "add1Quartus" ["inSignal"] ["outSignal"]


-- simulation setup
add1_simulation = simulate add1_sd
inputSignal =  map (fromIntegral) [1..10]
-- run that to simulate
-- *Add1Deep> add1_simulation [1..10]
-- [2,3,4,5,6,7,8,9,10,11]

-- Hardware Generation
compileVHDL :: IO ()
compileVHDL = writeVHDLOps vhdlOps add1_sd
 where vhdlOps = defaultVHDLOps{execQuartus=Just quartusOps}
       quartusOps = QuartusOps{action=FullCompilation,
                               fMax=Just 50, -- in MHz
                               fpgaFamiliyDevice=Just ("Cyclone V",
                                                       Just "5CSEMA4U23C6"),
                               -- Possibility for Pin Assignments
                               pinAssigs=[]
                              }
-- program the hw
-- quartus_pgm -c DE-SoC -m JTAG -o "p;./add1Quartus/vhdl/add1Quartus.sof@2"

-- Modelsim (not tested yet)
vhdlSim = writeAndModelsimVHDL Nothing add1_sd [1..10]
