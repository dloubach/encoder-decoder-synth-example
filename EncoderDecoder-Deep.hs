------------------------------------------------------------------------------
-- |
-- Module      :  Encoder Decoder Synthesis example using ForSyDe-Deep
-- Copyright   :  (c) Denis Loubach
-- License     :  BSD-style (see the file LICENSE)
-- 
-- Stability   :  experimental
-- Portability :  portable
--
------------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
-- this stands for the application model
module EncoderDecoderDeep where

import ForSyDe.Deep
import Data.Int (Int32)

-- 1st step: process function (PF) definition
add_pf :: ProcFun (Int32 -> Int32 -> Int32)
add_pf = $(newProcFun
          [d|
             add_pf :: Int32 -> Int32 -> Int32
             add_pf x y = x + y
          |])

-- 2nd step: system function (SF) definition
add_sf :: Signal Int32 -> Signal Int32 -> Signal Int32
add_sf = zipWithSY "add_sf" add_pf

-- 3rd step: system definition (SD) in terms of inputs/outputs
add_sd :: SysDef (Signal Int32 -> Signal Int32 -> Signal Int32)
add_sd = newSysDef add_sf "add_sd" ["input1","input2"] ["output"]


-- 4th step: simulation setup
add_simulation = simulate add_sd
-- run that to simulate
-- *EncoderDecoderDeep> add_simulation [1..10] [1..10]
-- [2,4,6,8,10,12,14,16,18,20]


-- 5th step: hardware generation
compileVHDL :: IO ()
compileVHDL = writeVHDLOps vhdlOps add_sd
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

-- -- Modelsim (not tested yet)
-- vhdlSim = writeAndModelsimVHDL Nothing add1_sd [1..10]
