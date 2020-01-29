package vexriscv

import spinal.core._

object RiscvFloat {
  // F EXTENSION
  object FADD   { def S  = M"0000000------------------1010011" }    
  object FSUB   { def S  = M"0000100------------------1010011" } 
  object FMUL   { def S  = M"0001000------------------1010011" } 
  object FDIV   { def S  = M"0001100------------------1010011" } 
  object FSGNJ  { def S  = M"0010000----------000-----1010011" } 
  object FSGNJN { def S  = M"0010000----------001-----1010011" } 
  object FSGNJX { def S  = M"0010000----------010-----1010011" } 
  object FMIN   { def S  = M"0010100----------000-----1010011" } 
  object FMAX   { def S  = M"0010100----------000-----1010011" } 
  object FSQRT  { def S  = M"010110000000-------------1010011" } 

  object FLE    { def S  = M"1010000----------000-----1010011" } 
  object FLT    { def S  = M"1010000----------001-----1010011" } 
  object FEQ    { def S  = M"1010000----------010-----1010011" } 

  object FCVT { 
    object S    { def W  = M"110100000000-------------1010011"  
                  def WU = M"110100000001-------------1010011" } 
    object W    { def S  = M"110000000000-------------1010011" } 
    object WU   { def S  = M"110000000001-------------1010011" } 
  }

  object FMV {
    object X    { def W  = M"1110000----------000-----1010011" } 
    object W    { def X  = M"1111100----------000-----1010011" } 
  }             

  object FCLASS  { def S = M"1110000----------001-----1010011" } 

  def FLW                = M"0000000----------010-----0000111" 
  def FSW                = M"0000000----------010-----0100111"

  object FMADD  { def S  = M"-----00------------------1010011" } 
  object FMSUB  { def S  = M"-----00------------------1010111" } 
  object FMNADD { def S  = M"-----00------------------1011011" } 
  object FMNSUB { def S  = M"-----00------------------1011111" } 
}


