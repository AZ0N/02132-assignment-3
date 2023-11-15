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
  val init :: loop :: up :: down :: left :: right :: black :: white :: done :: Nil = Enum(9)
  val stateReg = RegInit(init)

  // Support registers
  val x = RegInit(0.U(32.W))
  val y = RegInit(0.U(32.W))

  // Default values
  io.done := false.B
  io.address := 0.U(16.W)
  io.writeEnable := false.B
  io.dataWrite := 0.U(32.W)

  // FSMD Switch
  switch(stateReg) {
    is(init) {
      x := 0.U
      y := 0.U
      
      when (io.start) {
        stateReg := black
      } .otherwise {
        stateReg := init
      }
    }
    is(loop) {
      when (y === 19.U) {
        y := 0.U

        // Check done
        when (x === 19.U) {
          stateReg := done
        } .otherwise {
          x := x + 1.U
          // Since y = 0, color the pixel black
          stateReg := black
        }
      } .otherwise {
        y := y + 1.U
        when (x === 0.U || x === 19.U || y + 1.U === 19.U) {
          stateReg := black
        } .otherwise {
          io.address := 20.U * (y + 1.U) + x
          when (io.dataRead === 0.U) {
            stateReg := black
          } .otherwise {
            stateReg := up
          }
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
  }
}
