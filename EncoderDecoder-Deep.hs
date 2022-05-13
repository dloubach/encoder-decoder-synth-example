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

-- ---------------------------------------------------------------------------
-- add function
-- ---------------------------------------------------------------------------
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
add_simulation :: [Int32] -> [Int32] -> [Int32]
add_simulation = simulate add_sd
-- run that to simulate
-- *EncoderDecoderDeep> add_simulation [1..10] [1..10]
-- [2,4,6,8,10,12,14,16,18,20]

-- ---------------------------------------------------------------------------
-- sub function
-- ---------------------------------------------------------------------------
-- 1st step: process function (PF) definition
sub_pf :: ProcFun (Int32 -> Int32 -> Int32)
sub_pf = $(newProcFun
          [d|
             sub_pf :: Int32 -> Int32 -> Int32
             sub_pf x y = x - y
          |])

-- 2nd step: system function (SF) definition
sub_sf :: Signal Int32 -> Signal Int32 -> Signal Int32
sub_sf = zipWithSY "sub_sf" sub_pf

-- 3rd step: system definition (SD) in terms of inputs/outputs
sub_sd :: SysDef (Signal Int32 -> Signal Int32 -> Signal Int32)
sub_sd = newSysDef sub_sf "sub_sd" ["input1","input2"] ["output"]


-- 4th step: simulation setup
sub_simulation :: [Int32] -> [Int32] -> [Int32]
sub_simulation = simulate sub_sd
-- run that to simulate
-- *EncoderDecoderDeep> sub_simulation [1..10] [1..10]
-- [2,4,6,8,10,12,14,16,18,20]

-- ---------------------------------------------------------------------------
-- application model as a new system function
-- ---------------------------------------------------------------------------
lambdaExample_sf
  :: Signal Int32 -> Signal Int32 -> (Signal Int32, Signal Int32)
lambdaExample_sf s_key s_input = (s_enc,s_output)
   where
     s_enc    = (instantiate "add_sd" add_sd) s_input s_key
     s_output = (instantiate "sub_sd" sub_sd) s_enc s_key

lambdaExample_sd
  :: SysDef
       (Signal Int32 -> Signal Int32 -> (Signal Int32, Signal Int32))
lambdaExample_sd =
  newSysDef lambdaExample_sf "lambdaExample_sd" ["input1","input2"] ["output"]

lambdaExample_simulation = simulate lambdaExample_sd
-- run that to simulate
-- lambdaExample_simulation [1, 4, 6, 1, 1] [256, 512, 1024, 2048, -512]
-- ([257,516,1030,2049,-511],[256,512,1024,2048,-512])

-- ---------------------------------------------------------------------------
-- final step: hardware generation
-- ---------------------------------------------------------------------------
compileVHDL :: IO ()
compileVHDL = writeVHDLOps vhdlOps lambdaExample_sd
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
