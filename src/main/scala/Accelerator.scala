import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))

  })

  // State enum and register
  val init :: loop :: up :: down :: left :: right :: black :: white :: done :: borderOne :: borderLine :: Nil = Enum(11)
  val stateReg = RegInit(init)

  // Support registers
  val x = RegInit(0.U(32.W))
  val y = RegInit(0.U(32.W))
  val borderAdress = RegInit(0.U(32.W))

  // Default values
  io.done := false.B
  io.address := 0.U(16.W)
  io.writeEnable := false.B
  io.dataWrite := 0.U(32.W)

  // FSMD Switch
  switch(stateReg) {
    is(init) {
      x := 1.U
      y := 0.U
      borderAdress := 400.U
      
      when (io.start) {
        stateReg := borderOne
      } .otherwise {
        stateReg := init
      }
    }
    is(loop) {
      when (y === 18.U){
        y := 1.U
        when ( x === 18.U){
          stateReg := done
        } .otherwise{
          x := x + 1.U
            io.address := 20.U + x +1.U
            when (io.dataRead === 0.U) {
              stateReg := black
            } .otherwise {
              stateReg := up
            }
        }
      } .otherwise{
        y := y + 1.U
        io.address := 20.U * (y + 1.U) + x
          when (io.dataRead === 0.U) {
            stateReg := black
          } .otherwise {
            stateReg := up
          }
      }
    }
    is(up) {
      io.address := 20.U * y + x + 1.U
      when (io.dataRead === 0.U) {
        stateReg := black
      } .otherwise {
        stateReg := down 
      }
    }
    is(down) {
      io.address := 20.U * y + x - 1.U 
      when (io.dataRead === 0.U) {
        stateReg := black
      } .otherwise {
        stateReg := right 
      }
    }
    is(left) {
      io.address := 20.U * (y + 1.U) + x 
      when (io.dataRead === 0.U) {
        stateReg := black
      } .otherwise {
        stateReg := white 
      }
    }
    is(right) {
      io.address := 20.U * (y - 1.U) + x 
      when (io.dataRead === 0.U) {
        stateReg := black
      } .otherwise {
        stateReg := left 
      }
    }
    is(black) {
      io.address := 20.U * y + x + 400.U
      io.dataWrite := 0.U
      io.writeEnable := true.B
      stateReg := loop
    }
    is(white) {
      io.address := 20.U * y + x + 400.U
      io.dataWrite := 255.U 
      io.writeEnable := true.B
      stateReg := loop
    }
    is(done) {
      io.done := true.B
      stateReg := done
    }

    is(borderOne){
      io.writeEnable := true.B
      io.address := borderAdress
      io.dataWrite := 0.U 
      borderAdress := borderAdress + 1.U
      when(borderAdress <= 418.U || borderAdress >= 779.U){
        when(borderAdress >= 799.U){
          stateReg := loop
        } .otherwise {
        stateReg := borderOne
        }
      } .otherwise {
          stateReg := borderLine
        }
    }
    is(borderLine){
      io.writeEnable := true.B
      io.address := borderAdress
      io.dataWrite := 0.U
      borderAdress := borderAdress + 19.U
      stateReg := borderOne
    }

  }
}
